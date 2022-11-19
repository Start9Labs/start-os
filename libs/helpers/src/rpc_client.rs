use std::collections::BTreeMap;
use std::sync::atomic::AtomicUsize;
use std::sync::{Arc, Weak};

use models::{Error, ErrorKind, ResultExt};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use tokio::io::{AsyncBufReadExt, AsyncRead, AsyncWrite, AsyncWriteExt, BufReader};
use tokio::sync::{oneshot, Mutex};
use yajrc::{Id, RpcError, RpcMethod, RpcRequest, RpcResponse};

use crate::NonDetachingJoinHandle;

type DynWrite = Box<dyn AsyncWrite + Unpin + Send + Sync + 'static>;
type ResponseMap = BTreeMap<Id, oneshot::Sender<Result<Value, RpcError>>>;

pub struct RpcClient {
    id: AtomicUsize,
    _handler: NonDetachingJoinHandle<()>,
    writable: Weak<Mutex<(DynWrite, ResponseMap)>>,
}
impl RpcClient {
    pub fn new<
        W: AsyncWrite + Unpin + Send + Sync + 'static,
        R: AsyncRead + Unpin + Send + Sync + 'static,
    >(
        writer: W,
        reader: R,
    ) -> Self {
        let writer: DynWrite = Box::new(writer);
        let writable = Arc::new(Mutex::new((writer, ResponseMap::new())));
        let weak_writable = Arc::downgrade(&writable);
        RpcClient {
            id: AtomicUsize::new(0),
            _handler: tokio::spawn(async move {
                let mut lines = BufReader::new(reader).lines();
                while let Some(line) = lines.next_line().await.transpose() {
                    let mut w = writable.lock().await;
                    match line.map_err(Error::from).and_then(|l| {
                        serde_json::from_str::<RpcResponse>(&l)
                            .with_kind(ErrorKind::Deserialization)
                    }) {
                        Ok(l) => {
                            if let Some(id) = l.id {
                                if let Some(res) = w.1.remove(&id) {
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
            writable: weak_writable,
        }
    }

    pub async fn request<T: RpcMethod>(
        &self,
        method: T,
        params: T::Params,
    ) -> Result<T::Response, RpcError>
    where
        T: Serialize,
        T::Params: Serialize,
        T::Response: for<'de> Deserialize<'de>,
    {
        if let Some(w) = self.writable.upgrade() {
            let mut w = w.lock().await;
            let id = Id::Number(
                self.id
                    .fetch_add(1, std::sync::atomic::Ordering::SeqCst)
                    .into(),
            );
            w.0.write_all(
                (serde_json::to_string(&RpcRequest {
                    id: Some(id.clone()),
                    method,
                    params,
                })? + "\n")
                    .as_bytes(),
            )
            .await
            .map_err(|e| {
                let mut err = yajrc::INTERNAL_ERROR.clone();
                err.data = Some(json!(e.to_string()));
                err
            })?;
            let (send, recv) = oneshot::channel();
            w.1.insert(id, send);
            drop(w);
            if let Ok(val) = recv.await {
                return Ok(serde_json::from_value(val?)?);
            }
        }
        let mut err = yajrc::INTERNAL_ERROR.clone();
        err.data = Some(json!("RpcClient thread has terminated"));
        Err(err)
    }
}
