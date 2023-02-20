use std::collections::BTreeMap;
use std::net::{IpAddr, Ipv4Addr, SocketAddr};
use std::sync::{Arc, Weak};

use color_eyre::eyre::eyre;
use models::InterfaceId;
use patch_db::{DbHandle, LockType, PatchDb};
use sqlx::PgExecutor;
use tracing::instrument;

use crate::error::ErrorCollection;
use crate::hostname::Hostname;
use crate::net::dns::DnsController;
use crate::net::forward::LpfController;
use crate::net::keys::Key;
#[cfg(feature = "avahi")]
use crate::net::mdns::MdnsController;
use crate::net::ssl::{export_cert, export_key, SslManager};
use crate::net::tor::TorController;
use crate::net::vhost::VHostController;
use crate::s9pk::manifest::PackageId;
use crate::volume::cert_dir;
use crate::{Error, HOST_IP};

pub struct NetController {
    pub(super) tor: TorController,
    #[cfg(feature = "avahi")]
    pub(super) mdns: MdnsController,
    pub(super) vhost: VHostController,
    pub(super) dns: DnsController,
    pub(super) lpf: LpfController,
    pub(super) ssl: Arc<SslManager>,
    pub(super) os_bindings: Vec<Arc<()>>,
}

impl NetController {
    #[instrument(skip_all)]
    pub async fn init(
        tor_control: SocketAddr,
        dns_bind: &[SocketAddr],
        ssl: SslManager,
        hostname: &Hostname,
        os_key: &Key,
    ) -> Result<Self, Error> {
        let ssl = Arc::new(ssl);
        let mut res = Self {
            tor: TorController::init(tor_control).await?,
            #[cfg(feature = "avahi")]
            mdns: MdnsController::init().await?,
            vhost: VHostController::new(ssl.clone()),
            dns: DnsController::init(dns_bind).await?,
            lpf: LpfController::new(),
            ssl,
            os_bindings: Vec::new(),
        };
        res.add_os_bindings(hostname, os_key).await?;
        Ok(res)
    }

    async fn add_os_bindings(&mut self, hostname: &Hostname, key: &Key) -> Result<(), Error> {
        // Internal DNS
        self.vhost
            .add(
                key.clone(),
                Some("embassy".into()),
                443,
                ([127, 0, 0, 1], 80).into(),
                false,
            )
            .await?;
        self.os_bindings
            .push(self.dns.add(None, HOST_IP.into()).await?);

        // LAN IP
        self.os_bindings.push(
            self.vhost
                .add(key.clone(), None, 443, ([127, 0, 0, 1], 80).into(), false)
                .await?,
        );

        // localhost
        self.os_bindings.push(
            self.vhost
                .add(
                    key.clone(),
                    Some("localhost".into()),
                    443,
                    ([127, 0, 0, 1], 80).into(),
                    false,
                )
                .await?,
        );
        self.os_bindings.push(
            self.vhost
                .add(
                    key.clone(),
                    Some(hostname.no_dot_host_name()),
                    443,
                    ([127, 0, 0, 1], 80).into(),
                    false,
                )
                .await?,
        );

        // LAN mDNS
        self.os_bindings.push(
            self.vhost
                .add(
                    key.clone(),
                    Some(hostname.local_domain_name()),
                    443,
                    ([127, 0, 0, 1], 80).into(),
                    false,
                )
                .await?,
        );

        // Tor (http)
        self.os_bindings.push(
            self.tor
                .add(&key.tor_key(), 80, ([127, 0, 0, 1], 80).into())
                .await?,
        );

        // Tor (https)
        self.os_bindings.push(
            self.vhost
                .add(
                    key.clone(),
                    Some(key.tor_address().to_string()),
                    443,
                    ([127, 0, 0, 1], 80).into(),
                    false,
                )
                .await?,
        );
        self.os_bindings.push(
            self.tor
                .add(&key.tor_key(), 443, ([127, 0, 0, 1], 443).into())
                .await?,
        );

        Ok(())
    }

    #[instrument(skip_all)]
    pub async fn create_service(
        self: &Arc<Self>,
        package: PackageId,
        ip: Ipv4Addr,
    ) -> Result<NetService, Error> {
        let dns = self.dns.add(Some(package.clone()), ip).await?;

        Ok(NetService {
            id: package,
            ip,
            dns,
            controller: Arc::downgrade(self),
            tor: BTreeMap::new(),
            lan: BTreeMap::new(),
            lpf: BTreeMap::new(),
        })
    }

    async fn add_tor(
        &self,
        key: &Key,
        external: u16,
        target: SocketAddr,
    ) -> Result<Vec<Arc<()>>, Error> {
        let mut rcs = Vec::with_capacity(1);
        rcs.push(self.tor.add(&key.tor_key(), external, target).await?);
        Ok(rcs)
    }

    async fn remove_tor(&self, key: &Key, external: u16, rcs: Vec<Arc<()>>) -> Result<(), Error> {
        drop(rcs);
        self.tor.gc(&key.tor_key(), external).await
    }

    async fn add_lan(
        &self,
        key: Key,
        external: u16,
        target: SocketAddr,
        connect_ssl: bool,
    ) -> Result<Vec<Arc<()>>, Error> {
        let mut rcs = Vec::with_capacity(2);
        rcs.push(
            self.vhost
                .add(
                    key.clone(),
                    Some(key.local_address()),
                    external,
                    target.into(),
                    connect_ssl,
                )
                .await?,
        );
        #[cfg(feature = "avahi")]
        rcs.push(self.mdns.add(key.base_address()).await?);
        Ok(rcs)
    }

    async fn remove_lan(&self, key: &Key, external: u16, rcs: Vec<Arc<()>>) -> Result<(), Error> {
        drop(rcs);
        #[cfg(feature = "avahi")]
        self.mdns.gc(key.base_address()).await?;
        self.vhost.gc(Some(key.local_address()), external).await
    }

    async fn add_lpf(&self, external: u16, target: SocketAddr) -> Result<Arc<()>, Error> {
        self.lpf.add(external, target).await
    }

    async fn remove_lpf(&self, external: u16, rcs: Vec<Arc<()>>) -> Result<(), Error> {
        drop(rcs);
        self.lpf.gc(external).await
    }
}

pub struct NetService {
    id: PackageId,
    ip: Ipv4Addr,
    dns: Arc<()>,
    controller: Weak<NetController>,
    tor: BTreeMap<(InterfaceId, u16), (Key, Vec<Arc<()>>)>,
    lan: BTreeMap<(InterfaceId, u16), (Key, Vec<Arc<()>>)>,
    lpf: BTreeMap<u16, (u16, Vec<Arc<()>>)>,
}
impl NetService {
    #[instrument(skip(self))]
    fn net_controller(&self) -> Result<Arc<NetController>, Error> {
        Weak::upgrade(&self.controller).ok_or_else(|| {
            Error::new(
                eyre!("NetController is shutdown"),
                crate::ErrorKind::Network,
            )
        })
    }
    #[instrument(skip(self, secrets))]
    pub async fn add_tor<Ex>(
        &mut self,
        secrets: &mut Ex,
        id: InterfaceId,
        external: u16,
        internal: u16,
    ) -> Result<String, Error>
    where
        for<'a> &'a mut Ex: PgExecutor<'a>,
    {
        let key = Key::for_interface(secrets, Some((self.id.clone(), id.clone()))).await?;
        let ctrl = self.net_controller()?;
        let tor_idx = (id, external);
        let mut tor = self
            .tor
            .remove(&tor_idx)
            .unwrap_or_else(|| (key.clone(), Vec::new()));
        tor.1.append(
            &mut ctrl
                .add_tor(&key, external, SocketAddr::new(self.ip.into(), internal))
                .await?,
        );
        self.tor.insert(tor_idx, tor);
        Ok(key.tor_address().to_string())
    }
    pub async fn remove_tor(&mut self, id: InterfaceId, external: u16) -> Result<(), Error> {
        let ctrl = self.net_controller()?;
        if let Some((key, rcs)) = self.tor.remove(&(id, external)) {
            ctrl.remove_tor(&key, external, rcs).await?;
        }
        Ok(())
    }
    pub async fn add_lan<Ex>(
        &mut self,
        secrets: &mut Ex,
        id: InterfaceId,
        external: u16,
        internal: u16,
        connect_ssl: bool,
    ) -> Result<String, Error>
    where
        for<'a> &'a mut Ex: PgExecutor<'a>,
    {
        let key = Key::for_interface(secrets, Some((self.id.clone(), id.clone()))).await?;
        let addr = key.local_address();
        let ctrl = self.net_controller()?;
        let lan_idx = (id, external);
        let mut lan = self
            .lan
            .remove(&lan_idx)
            .unwrap_or_else(|| (key.clone(), Vec::new()));
        lan.1.append(
            &mut ctrl
                .add_lan(
                    key,
                    external,
                    SocketAddr::new(self.ip.into(), internal),
                    connect_ssl,
                )
                .await?,
        );
        self.lan.insert(lan_idx, lan);
        Ok(addr)
    }
    pub async fn remove_lan(&mut self, id: InterfaceId, external: u16) -> Result<(), Error> {
        let ctrl = self.net_controller()?;
        if let Some((key, rcs)) = self.lan.remove(&(id, external)) {
            ctrl.remove_lan(&key, external, rcs).await?;
        }
        Ok(())
    }
    pub async fn add_lpf(&mut self, db: &PatchDb, internal: u16) -> Result<u16, Error> {
        let ctrl = self.net_controller()?;
        let mut db = db.handle();
        let lpf_model = crate::db::DatabaseModel::new().lan_port_forwards();
        lpf_model.lock(&mut db, LockType::Write).await?; // TODO: replace all this with an RMW
        let mut lpf = lpf_model.get_mut(&mut db).await?;
        let external = lpf.alloc(self.id.clone(), internal).ok_or_else(|| {
            Error::new(
                eyre!("No ephemeral ports available"),
                crate::ErrorKind::Network,
            )
        })?;
        lpf.save(&mut db).await?;
        drop(db);
        let rc = ctrl.add_lpf(external, (self.ip, internal).into()).await?;
        let (_, mut lpfs) = self.lpf.remove(&internal).unwrap_or_default();
        lpfs.push(rc);
        self.lpf.insert(internal, (external, lpfs));

        Ok(external)
    }
    pub async fn remove_lpf(&mut self, db: &PatchDb, internal: u16) -> Result<(), Error> {
        let ctrl = self.net_controller()?;
        if let Some((external, rcs)) = self.lpf.remove(&internal) {
            ctrl.remove_lpf(external, rcs).await?;
        }

        Ok(())
    }
    pub async fn export_cert<Ex>(
        &self,
        secrets: &mut Ex,
        id: &InterfaceId,
        ip: IpAddr,
    ) -> Result<(), Error>
    where
        for<'a> &'a mut Ex: PgExecutor<'a>,
    {
        let key = Key::for_interface(secrets, Some((self.id.clone(), id.clone()))).await?;
        let ctrl = self.net_controller()?;
        let cert = ctrl.ssl.with_certs(key, ip).await?;
        let cert_dir = cert_dir(&self.id, id);
        tokio::fs::create_dir_all(&cert_dir).await?;
        export_key(
            &cert.key().openssl_key_nistp256(),
            &cert_dir.join(format!("{id}.key.pem")),
        )
        .await?;
        export_cert(
            &cert.fullchain_nistp256(),
            &cert_dir.join(format!("{id}.cert.pem")),
        )
        .await?; // TODO: can upgrade to ed25519?
        Ok(())
    }
    pub async fn remove_all(mut self) -> Result<(), Error> {
        let mut errors = ErrorCollection::new();
        if let Some(ctrl) = Weak::upgrade(&self.controller) {
            for ((_, external), (key, rcs)) in std::mem::take(&mut self.lan) {
                errors.handle(ctrl.remove_lan(&key, external, rcs).await);
            }
            for ((_, external), (key, rcs)) in std::mem::take(&mut self.tor) {
                errors.handle(ctrl.remove_tor(&key, external, rcs).await);
            }
            for (_, (external, rcs)) in std::mem::take(&mut self.lpf) {
                errors.handle(ctrl.remove_lpf(external, rcs).await);
            }
            std::mem::take(&mut self.dns);
            errors.handle(ctrl.dns.gc(Some(self.id.clone()), self.ip).await);
            self.ip = Ipv4Addr::new(0, 0, 0, 0);
            errors.into_result()
        } else {
            Err(Error::new(
                eyre!("NetController is shutdown"),
                crate::ErrorKind::Network,
            ))
        }
    }
}

impl Drop for NetService {
    fn drop(&mut self) {
        if self.ip != Ipv4Addr::new(0, 0, 0, 0) {
            tracing::debug!("Dropping NetService for {}", self.id);
            let svc = std::mem::replace(
                self,
                NetService {
                    id: Default::default(),
                    ip: Ipv4Addr::new(0, 0, 0, 0),
                    dns: Default::default(),
                    controller: Default::default(),
                    tor: Default::default(),
                    lan: Default::default(),
                    lpf: Default::default(),
                },
            );
            tokio::spawn(async move { svc.remove_all().await.unwrap() });
        }
    }
}
