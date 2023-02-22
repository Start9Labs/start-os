use std::sync::Arc;
use std::time::Duration;

use color_eyre::eyre::eyre;
use helpers::UnixRpcClient;
use tokio::sync::oneshot;
use tokio::sync::watch::{self, Receiver};
use tracing::instrument;

use super::manager_seed::ManagerSeed;
use super::{
    add_network_for_main, get_long_running_ip, long_running_docker, remove_network_for_main,
    GetRunningIp,
};
use crate::procedure::docker::DockerContainer;
use crate::util::NonDetachingJoinHandle;
use crate::Error;

pub struct PersistentContainer {
    _running_docker: NonDetachingJoinHandle<()>,
    pub rpc_client: Receiver<Arc<UnixRpcClient>>,
}

impl PersistentContainer {
    #[instrument(skip(seed))]
    pub async fn init(seed: &Arc<ManagerSeed>) -> Result<Option<Self>, Error> {
        Ok(if let Some(containers) = &seed.manifest.containers {
            let (running_docker, rpc_client) =
                spawn_persistent_container(seed.clone(), containers.main.clone()).await?;
            Some(Self {
                _running_docker: running_docker,
                rpc_client,
            })
        } else {
            None
        })
    }

    pub fn rpc_client(&self) -> Arc<UnixRpcClient> {
        self.rpc_client.borrow().clone()
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
