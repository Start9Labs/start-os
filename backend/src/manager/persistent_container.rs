use std::{
    fmt::{Debug, Display},
    path::PathBuf,
    sync::Arc,
};
use std::{path::Path, time::Duration};

use color_eyre::eyre::eyre;
use helpers::UnixRpcClient;
use tokio::sync::oneshot;
use tokio::sync::watch::{self, Receiver};
use tracing::instrument;

use super::manager_seed::ManagerSeed;
use super::{add_network_for_main, get_long_running_ip, remove_network_for_main, GetRunningIp};
use crate::procedure::docker::DockerContainer;
use crate::util::NonDetachingJoinHandle;
use crate::Error;

pub struct PersistantPaths {
    path: PathBuf,
}

impl PersistantPaths {
    pub fn root(&self) -> &Path {
        self.path.as_path()
    }

    pub fn socket_folder(&self) -> PathBuf {
        self.path.join("sockets")
    }
    pub fn socket_path(&self) -> PathBuf {
        self.socket_folder().join("sockets.sock")
    }
}

struct RunningDocker(NonDetachingJoinHandle<()>);

impl RunningDocker {
    pub async fn new(
        seed: Arc<ManagerSeed>,
        paths: Arc<PersistantPaths>,
        rpc_client: Arc<UnixRpcClient>,
    ) -> Result<Self, Error> {
        let detached_handle: NonDetachingJoinHandle<()> = tokio::task::spawn(async move {
            loop {
                async {
                    let mut runtime = seed
                        .manifest
                        .containers
                        .main
                        .long_running_execute(&seed, paths.clone(), rpc_client.clone())
                        .await?;

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

                    let res = runtime
                        .running_output
                        .await
                        .map_err(|_| {
                            Error::new(eyre!("Manager runtime panicked!"), crate::ErrorKind::Docker)
                        })
                        .map(|_| ());
                    remove_network_for_main(svc).await?;

                    res
                }
                .await
                .unwrap_or_else(Self::_log_persistant_errors);
                tokio::time::sleep(Duration::from_millis(200)).await;
            }
        })
        .into();
        Ok(RunningDocker(detached_handle))
    }
    fn _log_persistant_errors(e: impl Display + Debug) {
        tracing::error!("Error in persistent container: {}", e);
        tracing::debug!("{:?}", e);
    }
}

/** BLUJ
* SO this is going to be neading a loadmodule that is created by the start_init;
*
* Then we need to create the reading socket I suppose
* Then start the docker and run the docker running the load module.
*
* docker run \
           -v $libs:/start-init \
           -v $sockets:/start9 \
           --rm -it $(docker build -q .) sh -c "
               apk add nodejs &&
               node /start-init/bundleEs.js
           "

 * Then we need a way of sending the commands down to this. Neat
*/
pub struct PersistentContainer {
    running_docker: RunningDocker,
    pub rpc_client: Arc<UnixRpcClient>,
    paths: Arc<PersistantPaths>,
}

// TODO Need to clean up the folders that where created.

impl PersistentContainer {
    #[instrument(skip_all)]
    pub async fn init(seed: &Arc<ManagerSeed>) -> Result<Self, Error> {
        let paths = Arc::new(PersistantPaths {
            path: Path::new("/tmp/embassy/containers").join(format!(
                "{id}_{version}",
                id = &seed.manifest.id,
                version = &seed.manifest.version
            )),
        });
        if tokio::fs::metadata(&paths.root()).await.is_ok() {
            tokio::fs::remove_dir_all(&paths.root()).await?;
        }
        tokio::fs::create_dir_all(&paths.root()).await?;
        tokio::fs::create_dir_all(&paths.socket_folder()).await?;

        // tokio::fs::metadata(&socket_path)
        let rpc_client = Arc::new(UnixRpcClient::new(paths.socket_path().to_path_buf()));
        let running_docker =
            RunningDocker::new(seed.clone(), paths.clone(), rpc_client.clone()).await?;
        Ok(Self {
            running_docker,
            rpc_client,
            paths,
        })
    }

    pub fn rpc_client(&self) -> Arc<UnixRpcClient> {
        self.rpc_client.clone()
    }
    // async fn spawn_persistent_container(
    //     seed: Arc<ManagerSeed>,
    // ) -> Result<(NonDetachingJoinHandle<()>, Receiver<Arc<UnixRpcClient>>), Error> {
    //     let (send_inserter, inserter) = oneshot::channel();
    //     Ok((
    //     tokio::task::spawn(async move {
    //         let mut inserter_send: Option<watch::Sender<Arc<UnixRpcClient>>> = None;
    //         let mut send_inserter: Option<oneshot::Sender<Receiver<Arc<UnixRpcClient>>>> = Some(send_inserter);
    //         loop {
    //             if let Err(e) = async {
    //                 let (mut runtime, inserter) =
    //                 seed.manifest.containers.main.long_running_docker(&seed, &seed.manifest.containers.main).await?;

    //                 let ip = match get_long_running_ip(&seed, &mut runtime).await {
    //                     GetRunningIp::Ip(x) => x,
    //                     GetRunningIp::Error(e) => return Err(e),
    //                     GetRunningIp::EarlyExit(e) => {
    //                         tracing::error!("Early Exit");
    //                         tracing::debug!("{:?}", e);
    //                         return Ok(());
    //                     }
    //                 };
    //                 let svc = add_network_for_main(&seed, ip).await?;

    //                 if let Some(inserter_send) = inserter_send.as_mut() {
    //                     let _ = inserter_send.send(Arc::new(inserter));
    //                 } else {
    //                     let (s, r) = watch::channel(Arc::new(inserter));
    //                     inserter_send = Some(s);
    //                     if let Some(send_inserter) = send_inserter.take() {
    //                         let _ = send_inserter.send(r);
    //                     }
    //                 }

    //                 let res = tokio::select! {
    //                     a = runtime.running_output => a.map_err(|_| Error::new(eyre!("Manager runtime panicked!"), crate::ErrorKind::Docker)).map(|_| ()),
    //                 };

    //                 remove_network_for_main(svc).await?;

    //                 res
    //             }.await {
    //                 tracing::error!("Error in persistent container: {}", e);
    //                 tracing::debug!("{:?}", e);
    //             } else {
    //                 break;
    //             }
    //             tokio::time::sleep(Duration::from_millis(200)).await;
    //         }
    //     })
    //     .into(),
    //     inserter.await.map_err(|_| Error::new(eyre!("Container handle dropped before inserter sent"), crate::ErrorKind::Unknown))?,
    // ))
    // }
}
