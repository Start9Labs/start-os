use std::collections::BTreeMap;
use std::net::{IpAddr, Ipv4Addr, SocketAddr};
use std::sync::{Arc, Weak};

use color_eyre::eyre::eyre;
use imbl::OrdMap;
use lazy_format::lazy_format;
use models::{HostId, OptionExt, PackageId};
use patch_db::PatchDb;
use sqlx::PgExecutor;
use torut::onion::{OnionAddressV3, TorSecretKeyV3};
use tracing::instrument;

use crate::db::prelude::PatchDbExt;
use crate::error::ErrorCollection;
use crate::hostname::Hostname;
use crate::net::dns::DnsController;
use crate::net::forward::LanPortForwardController;
use crate::net::host::binding::{AddSslOptions, BindInfo, BindOptions};
use crate::net::host::{Host, HostKind};
use crate::net::ssl::{export_cert, export_key};
use crate::net::tor::TorController;
use crate::net::vhost::{AlpnInfo, VHostController};
use crate::util::serde::MaybeUtf8String;
use crate::volume::cert_dir;
use crate::{Error, HOST_IP};

pub struct NetController {
    db: PatchDb,
    pub(super) tor: TorController,
    pub(super) vhost: VHostController,
    pub(super) dns: DnsController,
    pub(super) forward: LanPortForwardController,
    pub(super) os_bindings: Vec<Arc<()>>,
}

impl NetController {
    #[instrument(skip_all)]
    pub async fn init(
        db: PatchDb,
        tor_control: SocketAddr,
        tor_socks: SocketAddr,
        dns_bind: &[SocketAddr],
        hostname: &Hostname,
        os_tor_key: TorSecretKeyV3,
    ) -> Result<Self, Error> {
        let mut res = Self {
            db: db.clone(),
            tor: TorController::new(tor_control, tor_socks),
            vhost: VHostController::new(db),
            dns: DnsController::init(dns_bind).await?,
            forward: LanPortForwardController::new(),
            os_bindings: Vec::new(),
        };
        res.add_os_bindings(hostname, os_tor_key).await?;
        Ok(res)
    }

    async fn add_os_bindings(
        &mut self,
        hostname: &Hostname,
        tor_key: TorSecretKeyV3,
    ) -> Result<(), Error> {
        let alpn = Err(AlpnInfo::Specified(vec![
            MaybeUtf8String("http/1.1".into()),
            MaybeUtf8String("h2".into()),
        ]));

        // Internal DNS
        self.vhost
            .add(
                Some("embassy".into()),
                443,
                ([127, 0, 0, 1], 80).into(),
                alpn.clone(),
            )
            .await?;
        self.os_bindings
            .push(self.dns.add(None, HOST_IP.into()).await?);

        // LAN IP
        self.os_bindings.push(
            self.vhost
                .add(None, 443, ([127, 0, 0, 1], 80).into(), alpn.clone())
                .await?,
        );

        // localhost
        self.os_bindings.push(
            self.vhost
                .add(
                    Some("localhost".into()),
                    443,
                    ([127, 0, 0, 1], 80).into(),
                    alpn.clone(),
                )
                .await?,
        );
        self.os_bindings.push(
            self.vhost
                .add(
                    Some(hostname.no_dot_host_name()),
                    443,
                    ([127, 0, 0, 1], 80).into(),
                    alpn.clone(),
                )
                .await?,
        );

        // LAN mDNS
        self.os_bindings.push(
            self.vhost
                .add(
                    Some(hostname.local_domain_name()),
                    443,
                    ([127, 0, 0, 1], 80).into(),
                    alpn.clone(),
                )
                .await?,
        );

        // Tor (http)
        self.os_bindings.push(
            self.tor
                .add(tor_key.clone(), 80, ([127, 0, 0, 1], 80).into())
                .await?,
        );

        // Tor (https)
        self.os_bindings.push(
            self.vhost
                .add(
                    Some(tor_key.public().get_onion_address().to_string()),
                    443,
                    ([127, 0, 0, 1], 80).into(),
                    alpn.clone(),
                )
                .await?,
        );
        self.os_bindings.push(
            self.tor
                .add(tor_key, 443, ([127, 0, 0, 1], 443).into())
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
            shutdown: false,
            id: package,
            ip,
            dns,
            controller: Arc::downgrade(self),
            binds: BTreeMap::new(),
        })
    }

    async fn add_tor(
        &self,
        key: TorSecretKeyV3,
        external: u16,
        target: SocketAddr,
    ) -> Result<Vec<Arc<()>>, Error> {
        let mut rcs = Vec::with_capacity(1);
        rcs.push(self.tor.add(key, external, target).await?);
        Ok(rcs)
    }

    async fn remove_tor(
        &self,
        key: TorSecretKeyV3,
        external: u16,
        rcs: Vec<Arc<()>>,
    ) -> Result<(), Error> {
        drop(rcs);
        self.tor.gc(Some(key), Some(external)).await
    }

    // async fn add_lan(
    //     &self,
    //     key: Key,
    //     external: u16,
    //     target: SocketAddr,
    //     connect_ssl: Result<(), AlpnInfo>,
    // ) -> Result<Vec<Arc<()>>, Error> {
    //     let mut rcs = Vec::with_capacity(2);
    //     rcs.push(
    //         self.vhost
    //             .add(
    //                 key.clone(),
    //                 Some(key.local_address()),
    //                 external,
    //                 target.into(),
    //                 connect_ssl,
    //             )
    //             .await?,
    //     );
    //     // rcs.push(self.mdns.add(key.base_address()).await?);
    //     // TODO
    //     Ok(rcs)
    // }

    // async fn remove_lan(&self, key: &Key, external: u16, rcs: Vec<Arc<()>>) -> Result<(), Error> {
    //     drop(rcs);
    //     // self.mdns.gc(key.base_address()).await?;
    //     // TODO
    //     self.vhost.gc(Some(key.local_address()), external).await
    // }
}

#[derive(Default)]
struct HostBinds {
    lan: BTreeMap<u16, (u16, Option<AddSslOptions>, Arc<()>)>,
    tor: BTreeMap<OnionAddressV3, (BTreeMap<u16, SocketAddr>, Arc<()>)>,
}

pub struct NetService {
    shutdown: bool,
    id: PackageId,
    ip: Ipv4Addr,
    dns: Arc<()>,
    controller: Weak<NetController>,
    binds: BTreeMap<HostId, HostBinds>,
}
impl NetService {
    fn net_controller(&self) -> Result<Arc<NetController>, Error> {
        Weak::upgrade(&self.controller).ok_or_else(|| {
            Error::new(
                eyre!("NetController is shutdown"),
                crate::ErrorKind::Network,
            )
        })
    }

    pub async fn bind(
        &mut self,
        kind: HostKind,
        id: HostId,
        internal_port: u16,
        options: BindOptions,
    ) -> Result<(), Error> {
        let id_ref = &id;
        let pkg_id = &self.id;
        let host = ctrl
            .db
            .mutate(|d| {
                let mut ports = d.as_private().as_available_ports().de()?;
                let hosts = d
                    .as_public_mut()
                    .as_package_data_mut()
                    .as_idx_mut(pkg_id)
                    .or_not_found(pkg_id)?
                    .as_installed_mut()
                    .or_not_found(pkg_id)?
                    .as_hosts_mut();
                hosts.add_binding(&mut ports, kind, &id, internal_port, options)?;
                let host = hosts
                    .as_idx(&id)
                    .or_not_found(lazy_format!("Host {id_ref} for {pkg_id}"))?
                    .de()?;
                d.as_private_mut().as_available_ports_mut().ser(&ports)?;
                Ok(host)
            })
            .await?;
        self.update(id, host).await
    }

    pub async fn update(&mut self, id: HostId, host: Host) -> Result<(), Error> {
        let ctrl = self.net_controller()?;
        let binds = {
            if !self.binds.contains_key(&id) {
                self.binds.insert(id.clone(), Default::default());
            }
            self.binds.get_mut(&id).unwrap()
        };
        if true {
            // TODO: if should listen lan

            for (port, bind) in &host.bindings {
                if let Some(external) = bind.assigned_lan_port {
                    let new_lan_bind = if let Some(b) = binds
                        .lan
                        .remove(port)
                        .filter(|(_, ssl, _)| ssl == &bind.options.add_ssl)
                    {
                        b
                    } else {
                        if let Some(ssl) = &bind.options.add_ssl {
                            let rc = ctrl
                                .vhost
                                .add(None, external, (self.ip, *port).into(), bind.options.ssl)
                                .await?;
                            (*port, Some(ssl.clone()), rc)
                        } else {
                            todo!()
                        }
                    };
                }
            }
        }
        for addr in match host.kind {
            HostKind::Multi => itertools::Either::Left(host.addresses.iter()),
            // HostKind::Single | HostKind::Static => itertools::Either::Right(&host.primary),
        } {}
    }

    pub async fn remove_all(mut self) -> Result<(), Error> {
        self.shutdown = true;
        let mut errors = ErrorCollection::new();
        if let Some(ctrl) = Weak::upgrade(&self.controller) {
            for ((_, external), rcs) in std::mem::take(&mut self.lan) {
                errors.handle(ctrl.remove_lan(&key, external, rcs).await);
            }
            for ((_, external), (key, rcs)) in std::mem::take(&mut self.tor) {
                errors.handle(ctrl.remove_tor(key, external, rcs).await);
            }
            std::mem::take(&mut self.dns);
            errors.handle(ctrl.dns.gc(Some(self.id.clone()), self.ip).await);
            errors.into_result()
        } else {
            tracing::warn!("NetService dropped after NetController is shutdown");
            Err(Error::new(
                eyre!("NetController is shutdown"),
                crate::ErrorKind::Network,
            ))
        }
    }
}

impl Drop for NetService {
    fn drop(&mut self) {
        if !self.shutdown {
            tracing::debug!("Dropping NetService for {}", self.id);
            let svc = std::mem::replace(
                self,
                NetService {
                    shutdown: true,
                    id: Default::default(),
                    ip: Ipv4Addr::new(0, 0, 0, 0),
                    dns: Default::default(),
                    controller: Default::default(),
                    tor: Default::default(),
                    lan: Default::default(),
                },
            );
            tokio::spawn(async move { svc.remove_all().await.unwrap() });
        }
    }
}
