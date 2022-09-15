use std::collections::BTreeMap;
use std::net::Ipv4Addr;

use color_eyre::eyre::eyre;
use tokio::process::{Child, Command};
use tokio::sync::Mutex;
use torut::onion::TorSecretKeyV3;

use super::interface::InterfaceId;
use crate::s9pk::manifest::PackageId;
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
    pub async fn add<'a, I: IntoIterator<Item = (InterfaceId, TorSecretKeyV3)>>(
        &self,
        pkg_id: &PackageId,
        interfaces: I,
    ) -> Result<(), Error> {
        self.0.lock().await.add(pkg_id, interfaces).await
    }
    pub async fn remove<I: IntoIterator<Item = InterfaceId>>(
        &self,
        pkg_id: &PackageId,
        interfaces: I,
    ) -> Result<(), Error> {
        self.0.lock().await.remove(pkg_id, interfaces).await
    }
}

pub struct MdnsControllerInner {
    alias_cmd: Option<Child>,
    services: BTreeMap<(PackageId, InterfaceId), TorSecretKeyV3>,
}

impl MdnsControllerInner {
    async fn init() -> Result<Self, Error> {
        let mut res = MdnsControllerInner {
            alias_cmd: None,
            services: BTreeMap::new(),
        };
        res.sync().await?;
        Ok(res)
    }
    async fn sync(&mut self) -> Result<(), Error> {
        if let Some(mut cmd) = self.alias_cmd.take() {
            cmd.kill().await.with_kind(crate::ErrorKind::Network)?;
        }
        self.alias_cmd = Some(
            Command::new("avahi-alias")
                .kill_on_drop(true)
                .args(self.services.iter().map(|(_, key)| {
                    key.public()
                        .get_onion_address()
                        .get_address_without_dot_onion()
                }))
                .spawn()?,
        );
        Ok(())
    }
    async fn add<'a, I: IntoIterator<Item = (InterfaceId, TorSecretKeyV3)>>(
        &mut self,
        pkg_id: &PackageId,
        interfaces: I,
    ) -> Result<(), Error> {
        self.services.extend(
            interfaces
                .into_iter()
                .map(|(interface_id, key)| ((pkg_id.clone(), interface_id), key)),
        );
        self.sync().await?;
        Ok(())
    }
    async fn remove<I: IntoIterator<Item = InterfaceId>>(
        &mut self,
        pkg_id: &PackageId,
        interfaces: I,
    ) -> Result<(), Error> {
        for interface_id in interfaces {
            self.services.remove(&(pkg_id.clone(), interface_id));
        }
        self.sync().await?;
        Ok(())
    }
}
