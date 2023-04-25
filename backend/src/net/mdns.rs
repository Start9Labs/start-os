use std::collections::BTreeMap;
use std::net::Ipv4Addr;
use std::sync::{Arc, Weak};

use color_eyre::eyre::eyre;
use tokio::process::{Child, Command};
use tokio::sync::Mutex;
use tracing::instrument;

use crate::util::Invoke;
use crate::{Error, ResultExt};

pub async fn resolve_mdns(hostname: &str) -> Result<Ipv4Addr, Error> {
    Ok(String::from_utf8(
        Command::new("avahi-resolve-host-name")
            .kill_on_drop(true)
            .arg("-4")
            .arg(hostname)
            .invoke(crate::ErrorKind::Network)
            .await?,
    )?
    .split_once("\t")
    .ok_or_else(|| {
        Error::new(
            eyre!("Failed to resolve hostname: {}", hostname),
            crate::ErrorKind::Network,
        )
    })?
    .1
    .trim()
    .parse()?)
}

pub struct MdnsController(Mutex<MdnsControllerInner>);
impl MdnsController {
    pub async fn init() -> Result<Self, Error> {
        Ok(MdnsController(Mutex::new(
            MdnsControllerInner::init().await?,
        )))
    }
    pub async fn add(&self, alias: String) -> Result<Arc<()>, Error> {
        self.0.lock().await.add(alias).await
    }
    pub async fn gc(&self, alias: String) -> Result<(), Error> {
        self.0.lock().await.gc(alias).await
    }
}

pub struct MdnsControllerInner {
    alias_cmd: Option<Child>,
    services: BTreeMap<String, Weak<()>>,
}

impl MdnsControllerInner {
    #[instrument(skip_all)]
    async fn init() -> Result<Self, Error> {
        let mut res = MdnsControllerInner {
            alias_cmd: None,
            services: BTreeMap::new(),
        };
        res.sync().await?;
        Ok(res)
    }
    #[instrument(skip_all)]
    async fn sync(&mut self) -> Result<(), Error> {
        if let Some(mut cmd) = self.alias_cmd.take() {
            cmd.kill().await.with_kind(crate::ErrorKind::Network)?;
        }
        self.alias_cmd = Some(
            Command::new("avahi-alias")
                .kill_on_drop(true)
                .args(
                    self.services
                        .iter()
                        .filter(|(_, rc)| rc.strong_count() > 0)
                        .map(|(s, _)| s),
                )
                .spawn()?,
        );
        Ok(())
    }
    async fn add(&mut self, alias: String) -> Result<Arc<()>, Error> {
        let rc = if let Some(rc) = Weak::upgrade(&self.services.remove(&alias).unwrap_or_default())
        {
            rc
        } else {
            Arc::new(())
        };
        self.services.insert(alias, Arc::downgrade(&rc));
        self.sync().await?;
        Ok(rc)
    }
    async fn gc(&mut self, alias: String) -> Result<(), Error> {
        if let Some(rc) = Weak::upgrade(&self.services.remove(&alias).unwrap_or_default()) {
            self.services.insert(alias, Arc::downgrade(&rc));
        }
        self.sync().await?;
        Ok(())
    }
}
