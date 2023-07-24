use std::collections::BTreeMap;
use std::path::PathBuf;
use std::sync::atomic::AtomicUsize;
use std::sync::{Arc, Weak};

use futures::future::BoxFuture;
use futures::{FutureExt, TryFutureExt};
use lazy_async_pool::Pool;
use models::{Error, ErrorKind, ResultExt};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use tokio::io::{AsyncBufReadExt, AsyncRead, AsyncWrite, AsyncWriteExt, BufReader};
use tokio::net::UnixStream;
use tokio::runtime::Handle;
use tokio::sync::{oneshot, Mutex};
use yajrc::{Id, RpcError, RpcMethod, RpcRequest, RpcResponse};

use crate::NonDetachingJoinHandle;

type DynWrite = Box<dyn AsyncWrite + Unpin + Send + Sync + 'static>;
type ResponseMap = BTreeMap<Id, oneshot::Sender<Result<Value, RpcError>>>;

const MAX_TRIES: u64 = 3;

pub struct RpcClient {
    id: Arc<AtomicUsize>,
    _handler: NonDetachingJoinHandle<()>,
    writer: DynWrite,
    responses: Weak<Mutex<ResponseMap>>,
}
impl RpcClient {
    pub fn new<
        W: AsyncWrite + Unpin + Send + Sync + 'static,
        R: AsyncRead + Unpin + Send + Sync + 'static,
    >(
        writer: W,
        reader: R,
        id: Arc<AtomicUsize>,
    ) -> Self {
        let writer: DynWrite = Box::new(writer);
        let responses = Arc::new(Mutex::new(ResponseMap::new()));
        let weak_responses = Arc::downgrade(&responses);
        RpcClient {
            id,
            _handler: tokio::spawn(async move {
                let mut lines = BufReader::new(reader).lines();
                while let Some(line) = lines.next_line().await.transpose() {
                    match line.map_err(Error::from).and_then(|l| {
                        serde_json::from_str::<RpcResponse>(&l)
                            .with_kind(ErrorKind::Deserialization)
                    }) {
                        Ok(l) => {
                            if let Some(id) = l.id {
                                if let Some(res) = responses.lock().await.remove(&id) {
                                    if let Err(e) = res.send(l.result) {
                                        tracing::warn!(
                                            "RpcClient Response for Unknown ID: {:?}",
                                            e
                                        );
                                    }
                                } else {
                                    tracing::warn!(
                                        "RpcClient Response for Unknown ID: {:?}",
                                        l.result
                                    );
                                }
                            } else {
                                tracing::info!("RpcClient Notification: {:?}", l);
                            }
                        }
                        Err(e) => {
                            tracing::error!("RpcClient Error: {}", e);
                            tracing::debug!("{:?}", e);
                        }
                    }
                }
            })
            .into(),
            writer,
            responses: weak_responses,
        }
    }

    pub async fn request<T: RpcMethod>(
        &mut self,
        method: T,
        params: T::Params,
    ) -> Result<T::Response, RpcError>
    where
        T: Serialize,
        T::Params: Serialize,
        T::Response: for<'de> Deserialize<'de>,
    {
        let id = Id::Number(
            self.id
                .fetch_add(1, std::sync::atomic::Ordering::SeqCst)
                .into(),
        );
        let request = RpcRequest {
            id: Some(id.clone()),
            method,
            params,
        };
        if let Some(w) = self.responses.upgrade() {
            let (send, recv) = oneshot::channel();
            w.lock().await.insert(id.clone(), send);
            self.writer
                .write_all((serde_json::to_string(&request)? + "\n").as_bytes())
                .await
                .map_err(|e| {
                    let mut err = yajrc::INTERNAL_ERROR.clone();
                    err.data = Some(json!(e.to_string()));
                    err
                })?;
            match recv.await {
                Ok(val) => {
                    return Ok(serde_json::from_value(val?)?);
                }
                Err(_err) => {
                    tokio::task::yield_now().await;
                }
            }
        }
        tracing::debug!(
            "Client has finished {:?}",
            futures::poll!(&mut self._handler)
        );
        let mut err = yajrc::INTERNAL_ERROR.clone();
        err.data = Some(json!("RpcClient thread has terminated"));
        Err(err)
    }
}

type PoolGenerator = Box<dyn Fn() -> PoolReturn + Send + Sync>;

type PoolReturn = BoxFuture<'static, Result<RpcClient, std::io::Error>>;

pub struct UnixRpcClient {
    pool: Pool<RpcClient, PoolGenerator, PoolReturn, std::io::Error>,
    path: PathBuf,
}
impl UnixRpcClient {
    pub fn new(path: PathBuf) -> Self {
        let rt = Handle::current();
        let id = Arc::new(AtomicUsize::new(0));
        let path_down = path.clone();
        Self {
            pool: Pool::new(
                0,
                Box::new(move || {
                    let path = path_down.clone();
                    let id = id.clone();
                    rt.spawn(async move {
                        let (r, w) = UnixStream::connect(&path).await?.into_split();
                        Ok(RpcClient::new(w, r, id))
                    })
                    .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))
                    .and_then(|x| async move { x })
                    .boxed()
                }),
            ),
            path,
        }
    }

    pub async fn request<T: RpcMethod>(
        &self,
        method: T,
        params: T::Params,
    ) -> Result<T::Response, RpcError>
    where
        T: Serialize + Clone,
        T::Params: Serialize + Clone,
        T::Response: for<'de> Deserialize<'de>,
    {
        let mut tries = 0;
        loop {
            tries += 1;
            let mut client = self.pool.clone().get().await?;
            let res = client.request(method.clone(), params.clone()).await;
            match &res {
                Err(e) if e.code == yajrc::INTERNAL_ERROR.code => {
                    client.destroy();
                }
                _ => break res,
            }
            if tries > MAX_TRIES {
                tracing::warn!("Max Tries exceeded");
                break res;
            }
        }
    }

    pub fn path(&self) -> &PathBuf {
        &self.path
    }
}
