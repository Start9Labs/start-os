use std::collections::BTreeSet;
use std::sync::{Arc, Weak};

use imbl_value::InternedString;
use serde::Serialize;
use tokio::process::Command;
use tokio::sync::Mutex;

use crate::prelude::*;
use crate::util::serde::IoFormat;
use crate::util::{new_guid, Invoke};

pub struct LxcManager {
    containers: Mutex<Vec<Weak<InternedString>>>,
}
impl LxcManager {
    pub async fn create(self: &Arc<Self>, config: LxcConfig) -> Result<LxcContainer, Error> {
        let container = LxcContainer::new(self.clone(), config).await?;
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
    manager: Arc<LxcManager>,
    guid: Arc<InternedString>,
    config: LxcConfig,
    exited: bool,
}
impl LxcContainer {
    async fn new(manager: Arc<LxcManager>, config: LxcConfig) -> Result<Self, Error> {
        let guid = new_guid();
        Command::new("lxc")
            .arg("launch")
            .arg("startos-init")
            .arg(&*guid)
            .arg("-e")
            .input(Some(&mut std::io::Cursor::new(
                IoFormat::Yaml.to_vec(&config)?,
            )))
            .invoke(ErrorKind::Lxc)
            .await?;
        Ok(Self {
            manager,
            guid: Arc::new(guid),
            config,
            exited: false,
        })
    }

    pub async fn exit(mut self) -> Result<(), Error> {
        Command::new("lxc")
            .arg("stop")
            .arg(&**self.guid)
            .invoke(ErrorKind::Lxc)
            .await?;
        Command::new("lxc")
            .arg("delete")
            .arg(&**self.guid)
            .invoke(ErrorKind::Lxc)
            .await?;

        self.exited = true;

        Ok(())
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
            let manager = self.manager.clone();
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

#[derive(Serialize)]
pub struct LxcConfig {}
