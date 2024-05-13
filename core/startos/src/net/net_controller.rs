use std::collections::{BTreeMap, BTreeSet};
use std::net::{Ipv4Addr, SocketAddr};
use std::sync::{Arc, Weak};

use color_eyre::eyre::eyre;
use imbl::OrdMap;
use lazy_format::lazy_format;
use models::{HostId, OptionExt, PackageId};
use torut::onion::{OnionAddressV3, TorSecretKeyV3};
use tracing::instrument;

use crate::db::model::Database;
use crate::error::ErrorCollection;
use crate::hostname::Hostname;
use crate::net::dns::DnsController;
use crate::net::forward::LanPortForwardController;
use crate::net::host::address::HostAddress;
use crate::net::host::binding::{AddSslOptions, BindOptions};
use crate::net::host::{host_for, Host, HostKind};
use crate::net::tor::TorController;
use crate::net::vhost::{AlpnInfo, VHostController};
use crate::prelude::*;
use crate::util::serde::MaybeUtf8String;
use crate::HOST_IP;

pub struct NetController {
    db: TypedPatchDb<Database>,
    pub(super) tor: TorController,
    pub(super) vhost: VHostController,
    pub(super) dns: DnsController,
    pub(super) forward: LanPortForwardController,
    pub(super) os_bindings: Vec<Arc<()>>,
}

impl NetController {
    #[instrument(skip_all)]
    pub async fn init(
        db: TypedPatchDb<Database>,
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

        // Tor
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
        self.os_bindings.extend(
            self.tor
                .add(
                    tor_key,
                    vec![
                        (80, ([127, 0, 0, 1], 80).into()),   // http
                        (443, ([127, 0, 0, 1], 443).into()), // https
                    ],
                )
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
}

#[derive(Default, Debug)]
struct HostBinds {
    lan: BTreeMap<u16, (u16, Option<AddSslOptions>, Arc<()>)>,
    tor: BTreeMap<OnionAddressV3, (OrdMap<u16, SocketAddr>, Vec<Arc<()>>)>,
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
        dbg!("bind", &kind, &id, internal_port, &options);
        let pkg_id = &self.id;
        let host = self
            .net_controller()?
            .db
            .mutate(|db| {
                let mut ports = db.as_private().as_available_ports().de()?;
                let host = host_for(db, pkg_id, &id, kind)?;
                host.add_binding(&mut ports, internal_port, options)?;
                let host = host.de()?;
                db.as_private_mut().as_available_ports_mut().ser(&ports)?;
                Ok(host)
            })
            .await?;
        self.update(id, host).await
    }
    pub async fn clear_bindings(&mut self) -> Result<(), Error> {
        let package_id = &self.id;
        self.net_controller()?
            .db
            .mutate(|db| {
                db.as_public_mut()
                    .as_package_data_mut()
                    .as_idx_mut(package_id)
                    .or_not_found(package_id)?
                    .as_hosts_mut()
                    .ser(&Default::default())
            })
            .await?;
        Ok(())
    }

    async fn update(&mut self, id: HostId, host: Host) -> Result<(), Error> {
        dbg!(&host);
        dbg!(&self.binds);
        let ctrl = self.net_controller()?;
        let binds = {
            if !self.binds.contains_key(&id) {
                self.binds.insert(id.clone(), Default::default());
            }
            self.binds.get_mut(&id).unwrap()
        };
        if true
        // TODO: if should listen lan
        {
            for (port, bind) in &host.bindings {
                let old_lan_bind = binds.lan.remove(port);
                let old_lan_port = old_lan_bind.as_ref().map(|(external, _, _)| *external);
                let lan_bind = old_lan_bind.filter(|(external, ssl, _)| {
                    ssl == &bind.options.add_ssl
                        && bind.assigned_lan_port.as_ref() == Some(external)
                }); // only keep existing binding if relevant details match
                if let Some(external) = bind.assigned_lan_port {
                    let new_lan_bind = if let Some(b) = lan_bind {
                        b
                    } else {
                        if let Some(ssl) = &bind.options.add_ssl {
                            let rc = ctrl
                                .vhost
                                .add(
                                    None,
                                    external,
                                    (self.ip, *port).into(),
                                    if bind.options.secure.as_ref().map_or(false, |s| s.ssl) {
                                        Ok(())
                                    } else {
                                        Err(ssl.alpn.clone())
                                    },
                                )
                                .await?;
                            (*port, Some(ssl.clone()), rc)
                        } else {
                            let rc = ctrl.forward.add(external, (self.ip, *port).into()).await?;
                            (*port, None, rc)
                        }
                    };
                    binds.lan.insert(*port, new_lan_bind);
                }
                if let Some(external) = old_lan_port {
                    ctrl.vhost.gc(None, external).await?;
                    ctrl.forward.gc(external).await?;
                }
            }
            let mut removed = BTreeSet::new();
            let mut removed_ssl = BTreeSet::new();
            binds.lan.retain(|internal, (external, ssl, _)| {
                if host.bindings.contains_key(internal) {
                    true
                } else {
                    if ssl.is_some() {
                        removed_ssl.insert(*external);
                    } else {
                        removed.insert(*external);
                    }
                    false
                }
            });
            for external in removed {
                ctrl.forward.gc(external).await?;
            }
            for external in removed_ssl {
                ctrl.vhost.gc(None, external).await?;
            }
        }
        let tor_binds: OrdMap<u16, SocketAddr> = host
            .bindings
            .iter()
            .flat_map(|(internal, info)| {
                let non_ssl = (
                    info.options.preferred_external_port,
                    SocketAddr::from((self.ip, *internal)),
                );
                if let (Some(ssl), Some(ssl_internal)) =
                    (&info.options.add_ssl, info.assigned_lan_port)
                {
                    itertools::Either::Left(
                        [
                            (
                                ssl.preferred_external_port,
                                SocketAddr::from(([127, 0, 0, 1], ssl_internal)),
                            ),
                            non_ssl,
                        ]
                        .into_iter(),
                    )
                } else {
                    itertools::Either::Right([non_ssl].into_iter())
                }
            })
            .collect();
        let mut keep_tor_addrs = BTreeSet::new();
        for addr in match host.kind {
            HostKind::Multi => {
                // itertools::Either::Left(
                host.addresses.iter()
                // )
            } // HostKind::Single | HostKind::Static => itertools::Either::Right(&host.primary),
        } {
            match addr {
                HostAddress::Onion { address } => {
                    keep_tor_addrs.insert(address);
                    let old_tor_bind = binds.tor.remove(address);
                    let tor_bind = old_tor_bind.filter(|(ports, _)| ports == &tor_binds);
                    let new_tor_bind = if let Some(tor_bind) = tor_bind {
                        tor_bind
                    } else {
                        let key = ctrl
                            .db
                            .peek()
                            .await
                            .into_private()
                            .into_key_store()
                            .into_onion()
                            .get_key(address)?;
                        let rcs = ctrl
                            .tor
                            .add(key, tor_binds.clone().into_iter().collect())
                            .await?;
                        (tor_binds.clone(), rcs)
                    };
                    binds.tor.insert(address.clone(), new_tor_bind);
                }
            }
        }
        for addr in binds.tor.keys() {
            if !keep_tor_addrs.contains(addr) {
                ctrl.tor.gc(Some(addr.clone()), None).await?;
            }
        }
        Ok(())
    }

    pub async fn remove_all(mut self) -> Result<(), Error> {
        self.shutdown = true;
        let mut errors = ErrorCollection::new();
        if let Some(ctrl) = Weak::upgrade(&self.controller) {
            for (_, binds) in std::mem::take(&mut self.binds) {
                for (_, (external, ssl, rc)) in binds.lan {
                    drop(rc);
                    if ssl.is_some() {
                        errors.handle(ctrl.vhost.gc(None, external).await);
                    } else {
                        errors.handle(ctrl.forward.gc(external).await);
                    }
                }
                for (addr, (_, rcs)) in binds.tor {
                    drop(rcs);
                    errors.handle(ctrl.tor.gc(Some(addr), None).await);
                }
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

    pub fn get_ip(&self) -> Ipv4Addr {
        self.ip
    }

    pub fn get_ext_port(&self, host_id: HostId, internal_port: u16) -> Result<u16, Error> {
        let host_id_binds = self.binds.get_key_value(&host_id);
        match host_id_binds {
            Some((_, binds)) => {
                if let Some(ext_port_info) = binds.lan.get(&internal_port) {
                    Ok(ext_port_info.0)
                } else {
                    Err(Error::new(
                        eyre!(
                            "Internal Port {} not found in NetService binds",
                            internal_port
                        ),
                        crate::ErrorKind::NotFound,
                    ))
                }
            }
            None => Err(Error::new(
                eyre!("HostID {} not found in NetService binds", host_id),
                crate::ErrorKind::NotFound,
            )),
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
                    binds: BTreeMap::new(),
                },
            );
            tokio::spawn(async move { svc.remove_all().await.unwrap() });
        }
    }
}
