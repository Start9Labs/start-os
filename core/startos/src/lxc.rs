use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Weak};
use std::time::Duration;

use helpers::UnixRpcClient;
use imbl_value::InternedString;
use serde::Serialize;
use tokio::process::Command;
use tokio::sync::Mutex;
use tokio::time::Instant;

use crate::disk::mount::util::unmount;
use crate::prelude::*;
use crate::util::serde::IoFormat;
use crate::util::{new_guid, Invoke};

const LXC_IMAGE_NAME: &str = "startos-service";
const LXC_CONTAINER_DIR: &str = "/var/lib/lxc";
const CONTAINER_RPC_SERVER_SOCKET: &str = "run/rpc.sock"; // must not be absolute path

pub struct LxcManager {
    containers: Mutex<Vec<Weak<InternedString>>>,
}
impl LxcManager {
    pub fn new() -> Self {
        Self {
            containers: Default::default(),
        }
    }

    pub async fn create(self: &Arc<Self>, config: LxcConfig) -> Result<LxcContainer, Error> {
        let container = LxcContainer::new(self, config).await?;
        let mut guard = self.containers.lock().await;
        *guard = std::mem::take(&mut *guard)
            .into_iter()
            .filter(|g| g.strong_count() > 0)
            .chain(std::iter::once(Arc::downgrade(&container.guid)))
            .collect();
        Ok(container)
    }

    pub async fn gc(&self) -> Result<(), Error> {
        let expected = BTreeSet::from_iter(
            self.containers
                .lock()
                .await
                .iter()
                .filter_map(|g| g.upgrade())
                .map(|g| (&*g).clone()),
        );
        for container in String::from_utf8(
            Command::new("lxc")
                .arg("list")
                .arg("-cn")
                .arg("-fcsv")
                .invoke(ErrorKind::Lxc)
                .await?,
        )?
        .lines()
        .map(|s| s.trim())
        {
            if !expected.contains(container) {
                Command::new("lxc")
                    .arg("delete")
                    .arg("--force")
                    .arg(container)
                    .invoke(ErrorKind::Lxc)
                    .await?;
            }
        }
        Ok(())
    }
}

pub struct LxcContainer {
    manager: Weak<LxcManager>,
    guid: Arc<InternedString>,
    config: LxcConfig,
    exited: bool,
}
impl LxcContainer {
    async fn new(manager: &Arc<LxcManager>, config: LxcConfig) -> Result<Self, Error> {
        let guid = new_guid();
        Command::new("lxc")
            .arg("launch")
            .arg(LXC_IMAGE_NAME)
            .arg(&*guid)
            .arg("-e")
            .input(Some(&mut std::io::Cursor::new(
                IoFormat::Yaml.to_vec(&config)?,
            )))
            .invoke(ErrorKind::Lxc)
            .await?;
        Ok(Self {
            manager: Arc::downgrade(manager),
            guid: Arc::new(guid),
            config,
            exited: false,
        })
    }

    pub fn rootfs_dir(&self) -> PathBuf {
        Path::new(LXC_CONTAINER_DIR)
            .join(&*self.guid)
            .join("rootfs")
    }

    pub async fn exit(mut self) -> Result<(), Error> {
        for mountpoint in String::from_utf8(
            Command::new("find")
                .arg(self.rootfs_dir())
                .arg("-depth")
                .arg("!")
                .arg("-exec")
                .arg("mountpoint")
                .arg("-q")
                .arg("{}")
                .arg(";")
                .arg("-print")
                .invoke(ErrorKind::Filesystem)
                .await?,
        )?
        .lines()
        {
            unmount(mountpoint).await?;
        }
        Command::new("lxc")
            .arg("stop")
            .arg(&**self.guid)
            .invoke(ErrorKind::Lxc)
            .await?;

        self.exited = true;

        Ok(())
    }

    pub async fn connect_rpc(&self, timeout: Option<Duration>) -> Result<UnixRpcClient, Error> {
        let started = Instant::now();
        let sock_path = self.rootfs_dir().join(CONTAINER_RPC_SERVER_SOCKET);
        while tokio::fs::metadata(&sock_path).await.is_err() {
            if timeout.map_or(false, |t| started.elapsed() > t) {
                return Err(Error::new(
                    eyre!("timed out waiting for socket"),
                    ErrorKind::Timeout,
                ));
            }
            tokio::time::sleep(Duration::from_millis(100)).await;
        }
        Ok(UnixRpcClient::new(sock_path))
    }
}
impl Drop for LxcContainer {
    fn drop(&mut self) {
        if !self.exited {
            tracing::warn!(
                "Container {} was ungracefully dropped. Cleaning up dangling containers...",
                &**self.guid
            );
            drop(std::mem::take(&mut self.guid));
            if let Some(manager) = self.manager.upgrade() {
                tokio::spawn(async move {
                    if let Err(e) = manager.gc().await {
                        tracing::error!("Error cleaning up dangling LXC containers: {e}");
                        tracing::debug!("{e:?}")
                    } else {
                        tracing::info!("Successfully cleaned up dangling LXC containers");
                    }
                });
            }
        }
    }
}

#[derive(Default, Serialize)]
pub struct LxcConfig {}
