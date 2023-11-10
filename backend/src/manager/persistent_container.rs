use std::sync::Arc;
use std::time::Duration;

use color_eyre::eyre::eyre;
use helpers::UnixRpcClient;
use models::ProcedureName;
use nix::sys::signal::Signal;
use serde::de::DeserializeOwned;
use tokio::sync::watch::{self, Receiver};
use tokio::sync::{oneshot, Mutex};
use tracing::instrument;

use super::manager_seed::ManagerSeed;
use super::{
    add_network_for_main, get_long_running_ip, long_running_docker, remove_network_for_main,
    GetRunningIp,
};
use crate::prelude::*;
use crate::procedure::docker::DockerContainer;
use crate::util::NonDetachingJoinHandle;

struct ProcedureId(u64);

/// Persistant container are the old containers that need to run all the time
/// The goal is that all services will be persistent containers, waiting to run the main system.
pub struct PersistentContainer {
    _running_docker: NonDetachingJoinHandle<()>,
    // TODO: Drb: Implement to spec https://github.com/Start9Labs/start-sdk/blob/master/lib/types.ts#L223
    pub rpc_client: Receiver<Arc<UnixRpcClient>>,
    manager_seed: Arc<ManagerSeed>,
    procedures: Mutex<Vec<(ProcedureName, ProcedureId)>>,
}

// BLUJ TODO Need to get the only action is this and not procedure/<docker,js_scripts>
// BLUJ Modify the rpc client to match the new type

impl PersistentContainer {
    #[instrument(skip_all)]
    pub async fn init(seed: &Arc<ManagerSeed>) -> Result<Self, Error> {
        Ok(if let Some(containers) = &seed.manifest.containers {
            let (running_docker, rpc_client) =
                spawn_persistent_container(seed.clone(), containers.main.clone()).await?;
            Self {
                _running_docker: running_docker,
                rpc_client,
                manager_seed: seed.clone(),
                procedures: Default::default(),
            }
        } else {
            todo!("DRB No containers in manifest")
        })
    }

    pub fn rpc_client(&self) -> Arc<UnixRpcClient> {
        self.rpc_client.borrow().clone()
    }

    pub async fn execute<O>(
        &self,
        name: ProcedureName,
        input: Value,
        timeout: Option<Duration>,
    ) -> Result<Result<O, (i32, String)>, Error>
    where
        O: DeserializeOwned,
    {
        match self._execute(name, input, timeout).await {
            Ok(Ok(a)) => Ok(Ok(imbl_value::from_value(a).map_err(|e| {
                Error::new(
                    eyre!("Error deserializing output: {}", e),
                    crate::ErrorKind::Deserialization,
                )
            })?)),
            Ok(Err(e)) => Ok(Err(e)),
            Err(e) => Err(e),
        }
    }

    pub async fn sanboxed<O>(
        &self,
        name: ProcedureName,
        input: Value,
        timeout: Option<Duration>,
    ) -> Result<Result<O, (i32, String)>, Error>
    where
        O: DeserializeOwned,
    {
        match self._sandboxed(name, input, timeout).await {
            Ok(Ok(a)) => Ok(Ok(imbl_value::from_value(a).map_err(|e| {
                Error::new(
                    eyre!("Error deserializing output: {}", e),
                    crate::ErrorKind::Deserialization,
                )
            })?)),
            Ok(Err(e)) => Ok(Err(e)),
            Err(e) => Err(e),
        }
    }
    async fn _execute(
        &self,
        name: ProcedureName,
        input: Value,
        timeout: Option<Duration>,
    ) -> Result<Result<Value, (i32, String)>, Error> {
        todo!(
            r#"""
            DRB
            Call into the persistant via rpc, start a procedure.
            Procedure already has access to rpc to call back, maybe an id to track?
            Should be able to cancel.
            Note(Main): Only one should be running at a time
            Note(Main): Has additional effect of setRunning 
            Note: The input (Option<I>) is not generic because we don't want to clone this fn for each type of input
            Note: The output is not generic because we don't want to clone this fn for each type of output
        """#
        )
    }

    async fn _sandboxed(
        &self,
        name: ProcedureName,
        input: Value,
        timeout: Option<Duration>,
    ) -> Result<Result<Value, (i32, String)>, Error> {
        todo!("DRB")
    }

    pub async fn send_signal(&self, gid: Arc<super::Gid>, signal: Signal) -> Result<(), Error> {
        todo!("DRB")
    }
}

pub async fn spawn_persistent_container(
    seed: Arc<ManagerSeed>,
    container: DockerContainer,
) -> Result<(NonDetachingJoinHandle<()>, Receiver<Arc<UnixRpcClient>>), Error> {
    let (send_inserter, inserter) = oneshot::channel();
    Ok((
        tokio::task::spawn(async move {
            let mut inserter_send: Option<watch::Sender<Arc<UnixRpcClient>>> = None;
            let mut send_inserter: Option<oneshot::Sender<Receiver<Arc<UnixRpcClient>>>> = Some(send_inserter);
            loop {
                if let Err(e) = async {
                    let (mut runtime, inserter) =
                        long_running_docker(&seed, &container).await?;


                    let ip = match get_long_running_ip(&seed, &mut runtime).await {
                        GetRunningIp::Ip(x) => x,
                        GetRunningIp::Error(e) => return Err(e),
                        GetRunningIp::EarlyExit(e) => {
                            tracing::error!("Early Exit");
                            tracing::debug!("{:?}", e);
                            return Ok(());
                        }
                    };
                    let svc = add_network_for_main(&seed, ip).await?;

                    if let Some(inserter_send) = inserter_send.as_mut() {
                        let _ = inserter_send.send(Arc::new(inserter));
                    } else {
                        let (s, r) = watch::channel(Arc::new(inserter));
                        inserter_send = Some(s);
                        if let Some(send_inserter) = send_inserter.take() {
                            let _ = send_inserter.send(r);
                        }
                    }

                    let res = tokio::select! {
                        a = runtime.running_output => a.map_err(|_| Error::new(eyre!("Manager runtime panicked!"), crate::ErrorKind::Docker)).map(|_| ()),
                    };

                    remove_network_for_main(svc).await?;

                    res
                }.await {
                    tracing::error!("Error in persistent container: {}", e);
                    tracing::debug!("{:?}", e);
                } else {
                    break;
                }
                tokio::time::sleep(Duration::from_millis(200)).await;
            }
        })
        .into(),
        inserter.await.map_err(|_| Error::new(eyre!("Container handle dropped before inserter sent"), crate::ErrorKind::Unknown))?,
    ))
}
