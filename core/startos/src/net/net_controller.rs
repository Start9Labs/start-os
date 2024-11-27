use std::collections::{BTreeMap, BTreeSet};
use std::net::{Ipv4Addr, SocketAddr};
use std::sync::{Arc, Weak};

use color_eyre::eyre::eyre;
use imbl::OrdMap;
use imbl_value::InternedString;
use ipnet::IpNet;
use models::{HostId, OptionExt, PackageId};
use torut::onion::{OnionAddressV3, TorSecretKeyV3};
use tracing::instrument;

use crate::db::model::Database;
use crate::error::ErrorCollection;
use crate::hostname::Hostname;
use crate::net::dns::DnsController;
use crate::net::forward::LanPortForwardController;
use crate::net::host::address::HostAddress;
use crate::net::host::binding::{AddSslOptions, BindId, BindOptions, NetInfo};
use crate::net::host::{host_for, Host, HostKind, Hosts};
use crate::net::network_interface::NetworkInterfaceController;
use crate::net::service_interface::{HostnameInfo, IpHostname, OnionHostname};
use crate::net::tor::TorController;
use crate::net::vhost::{AlpnInfo, VHostController};
use crate::prelude::*;
use crate::util::serde::MaybeUtf8String;
use crate::HOST_IP;

pub struct PreInitNetController {
    pub db: TypedPatchDb<Database>,
    tor: TorController,
    vhost: VHostController,
    net_iface: Arc<NetworkInterfaceController>,
    os_bindings: Vec<Arc<()>>,
    server_hostnames: Vec<Option<InternedString>>,
}
impl PreInitNetController {
    #[instrument(skip_all)]
    pub async fn init(
        db: TypedPatchDb<Database>,
        tor_control: SocketAddr,
        tor_socks: SocketAddr,
        hostname: &Hostname,
        os_tor_key: TorSecretKeyV3,
    ) -> Result<Self, Error> {
        let net_iface = Arc::new(NetworkInterfaceController::new(db.clone()));
        let mut res = Self {
            db: db.clone(),
            tor: TorController::new(tor_control, tor_socks),
            vhost: VHostController::new(db, net_iface.clone()),
            net_iface,
            os_bindings: Vec::new(),
            server_hostnames: Vec::new(),
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

        self.server_hostnames = vec![
            // LAN IP
            None,
            // Internal DNS
            Some("embassy".into()),
            Some("startos".into()),
            // localhost
            Some("localhost".into()),
            Some(hostname.no_dot_host_name()),
            // LAN mDNS
            Some(hostname.local_domain_name()),
        ];

        for hostname in self.server_hostnames.iter().cloned() {
            self.os_bindings.push(self.vhost.add(
                hostname,
                443,
                false,
                ([127, 0, 0, 1], 80).into(),
                alpn.clone(),
            )?);
        }

        // Tor
        self.os_bindings.push(self.vhost.add(
            Some(InternedString::from_display(
                &tor_key.public().get_onion_address(),
            )),
            443,
            false,
            ([127, 0, 0, 1], 80).into(),
            alpn.clone(),
        )?);
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
}

pub struct NetController {
    db: TypedPatchDb<Database>,
    pub(super) tor: TorController,
    pub(super) vhost: VHostController,
    pub(super) net_iface: Arc<NetworkInterfaceController>,
    pub(super) dns: DnsController,
    pub(super) forward: LanPortForwardController,
    pub(super) os_bindings: Vec<Arc<()>>,
    pub(super) server_hostnames: Vec<Option<InternedString>>,
}

impl NetController {
    pub async fn init(
        PreInitNetController {
            db,
            tor,
            vhost,
            net_iface,
            os_bindings,
            server_hostnames,
        }: PreInitNetController,
        dns_bind: &[SocketAddr],
    ) -> Result<Self, Error> {
        let mut res = Self {
            db,
            tor,
            vhost,
            net_iface,
            dns: DnsController::init(dns_bind).await?,
            forward: LanPortForwardController::new(),
            os_bindings,
            server_hostnames,
        };
        res.os_bindings
            .push(res.dns.add(None, HOST_IP.into()).await?);
        Ok(res)
    }

    #[instrument(skip_all)]
    pub async fn create_service(
        self: &Arc<Self>,
        package: PackageId,
        ip: Ipv4Addr,
    ) -> Result<NetService, Error> {
        let dns = self.dns.add(Some(package.clone()), ip).await?;

        let mut res = NetService {
            shutdown: false,
            id: package,
            ip,
            dns,
            controller: Arc::downgrade(self),
            binds: BTreeMap::new(),
        };
        res.clear_bindings(Default::default()).await?;
        Ok(res)
    }
}

#[derive(Default, Debug)]
struct HostBinds {
    lan: BTreeMap<
        u16,
        (
            NetInfo,
            Option<AddSslOptions>,
            BTreeSet<InternedString>,
            Vec<Arc<()>>,
        ),
    >,
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

    pub async fn clear_bindings(&mut self, except: BTreeSet<BindId>) -> Result<(), Error> {
        let pkg_id = &self.id;
        let hosts = self
            .net_controller()?
            .db
            .mutate(|db| {
                let mut res = Hosts::default();
                for (host_id, host) in db
                    .as_public_mut()
                    .as_package_data_mut()
                    .as_idx_mut(pkg_id)
                    .or_not_found(pkg_id)?
                    .as_hosts_mut()
                    .as_entries_mut()?
                {
                    host.as_bindings_mut().mutate(|b| {
                        for (internal_port, info) in b {
                            if !except.contains(&BindId {
                                id: host_id.clone(),
                                internal_port: *internal_port,
                            }) {
                                info.disable();
                            }
                        }
                        Ok(())
                    })?;
                    res.0.insert(host_id, host.de()?);
                }
                Ok(res)
            })
            .await?;
        let mut errors = ErrorCollection::new();
        for (id, host) in hosts.0 {
            errors.handle(self.update(id, host).await);
        }
        errors.into_result()
    }

    pub async fn update(&mut self, id: HostId, host: Host) -> Result<(), Error> {
        let ctrl = self.net_controller()?;
        let mut hostname_info = BTreeMap::new();
        let binds = self.binds.entry(id.clone()).or_default();

        let peek = ctrl.db.peek().await;

        // LAN
        let server_info = peek.as_public().as_server_info();
        let net_ifaces = server_info.as_network_interfaces().de()?;
        let hostname = server_info.as_hostname().de()?;
        for (port, bind) in &host.bindings {
            if !bind.enabled {
                continue;
            }
            let old_lan_bind = binds.lan.remove(port);
            let lan_bind = old_lan_bind
                .as_ref()
                .filter(|(external, ssl, _, _)| {
                    ssl == &bind.options.add_ssl && bind.net == *external
                })
                .cloned(); // only keep existing binding if relevant details match
            if bind.net.assigned_port.is_some() || bind.net.assigned_ssl_port.is_some() {
                let new_lan_bind = if let Some(b) = lan_bind {
                    b
                } else {
                    let mut rcs = Vec::with_capacity(2 + host.addresses.len());
                    let mut hostnames = BTreeSet::new();
                    if let Some(ssl) = &bind.options.add_ssl {
                        let external = bind
                            .net
                            .assigned_ssl_port
                            .or_not_found("assigned ssl port")?;
                        let target = (self.ip, *port).into();
                        let connect_ssl = if let Some(alpn) = ssl.alpn.clone() {
                            Err(alpn)
                        } else {
                            if bind.options.secure.as_ref().map_or(false, |s| s.ssl) {
                                Ok(())
                            } else {
                                Err(AlpnInfo::Reflect)
                            }
                        };
                        for hostname in ctrl.server_hostnames.iter().cloned() {
                            rcs.push(ctrl.vhost.add(
                                hostname,
                                external,
                                bind.net.public,
                                target,
                                connect_ssl.clone(),
                            )?);
                        }
                        for address in host.addresses() {
                            match address {
                                HostAddress::Onion { address } => {
                                    let hostname = InternedString::from_display(address);
                                    if hostnames.insert(hostname.clone()) {
                                        rcs.push(ctrl.vhost.add(
                                            Some(hostname),
                                            external,
                                            false,
                                            target,
                                            connect_ssl.clone(),
                                        )?);
                                    }
                                }
                                HostAddress::Domain { address } => {
                                    if hostnames.insert(address.clone()) {
                                        let address = Some(address.clone());
                                        rcs.push(ctrl.vhost.add(
                                            address.clone(),
                                            external,
                                            bind.net.public,
                                            target,
                                            connect_ssl.clone(),
                                        )?);
                                        if ssl.preferred_external_port == 443 {
                                            rcs.push(ctrl.vhost.add(
                                                address.clone(),
                                                5443,
                                                false,
                                                target,
                                                connect_ssl.clone(),
                                            )?);
                                            rcs.push(ctrl.vhost.add(
                                                address.clone(),
                                                443,
                                                true,
                                                target,
                                                connect_ssl.clone(),
                                            )?);
                                        }
                                    }
                                }
                            }
                        }
                    }
                    if let Some(security) = bind.options.secure {
                        if bind.options.add_ssl.is_some() && security.ssl {
                            // doesn't make sense to have 2 listening ports, both with ssl
                        } else {
                            let external =
                                bind.net.assigned_port.or_not_found("assigned lan port")?;
                            rcs.push(ctrl.forward.add(external, (self.ip, *port).into()).await?);
                        }
                    }
                    (bind.net, bind.options.add_ssl.clone(), hostnames, rcs)
                };
                let mut bind_hostname_info: Vec<HostnameInfo> =
                    hostname_info.remove(port).unwrap_or_default();
                for (interface, iface_info) in &net_ifaces {
                    if !iface_info.public {
                        bind_hostname_info.push(HostnameInfo::Ip {
                            network_interface_id: interface.clone(),
                            public: false,
                            hostname: IpHostname::Local {
                                value: InternedString::from_display(&{
                                    let hostname = &hostname;
                                    lazy_format!("{hostname}.local")
                                }),
                                port: new_lan_bind.0.assigned_port,
                                ssl_port: new_lan_bind.0.assigned_ssl_port,
                            },
                        });
                    }
                    for address in host.addresses() {
                        if let HostAddress::Domain { address } = address {
                            if new_lan_bind
                                .1
                                .as_ref()
                                .map_or(false, |ssl| ssl.preferred_external_port == 443)
                            {
                                bind_hostname_info.push(HostnameInfo::Ip {
                                    network_interface_id: interface.clone(),
                                    public: iface_info.public,
                                    hostname: IpHostname::Domain {
                                        domain: address.clone(),
                                        subdomain: None,
                                        port: None,
                                        ssl_port: Some(443),
                                    },
                                });
                            } else if iface_info.public && new_lan_bind.0.public {
                                bind_hostname_info.push(HostnameInfo::Ip {
                                    network_interface_id: interface.clone(),
                                    public: iface_info.public,
                                    hostname: IpHostname::Domain {
                                        domain: address.clone(),
                                        subdomain: None,
                                        port: new_lan_bind.0.assigned_port,
                                        ssl_port: new_lan_bind.0.assigned_ssl_port,
                                    },
                                });
                            }
                        }
                    }
                    if !iface_info.public || new_lan_bind.0.public {
                        for ipnet in &iface_info.ip_info.0 {
                            match ipnet {
                                IpNet::V4(net) => {
                                    bind_hostname_info.push(HostnameInfo::Ip {
                                        network_interface_id: interface.clone(),
                                        public: iface_info.public,
                                        hostname: IpHostname::Ipv4 {
                                            value: net.addr(),
                                            port: new_lan_bind.0.assigned_port,
                                            ssl_port: new_lan_bind.0.assigned_ssl_port,
                                        },
                                    });
                                }
                                IpNet::V6(net) => {
                                    bind_hostname_info.push(HostnameInfo::Ip {
                                        network_interface_id: interface.clone(),
                                        public: iface_info.public,
                                        hostname: IpHostname::Ipv6 {
                                            value: net.addr(),
                                            port: new_lan_bind.0.assigned_port,
                                            ssl_port: new_lan_bind.0.assigned_ssl_port,
                                        },
                                    });
                                }
                            }
                        }
                    }
                }
                hostname_info.insert(*port, bind_hostname_info);
                binds.lan.insert(*port, new_lan_bind);
            }
            if let Some((lan, _, hostnames, _)) = old_lan_bind {
                if let Some(external) = lan.assigned_ssl_port {
                    for hostname in ctrl.server_hostnames.iter().cloned() {
                        ctrl.vhost.gc(hostname, external)?;
                    }
                    for hostname in hostnames {
                        ctrl.vhost.gc(Some(hostname), external)?;
                    }
                }
                if let Some(external) = lan.assigned_port {
                    ctrl.forward.gc(external).await?;
                }
            }
        }
        let mut removed = BTreeSet::new();
        binds.lan.retain(|internal, (external, _, hostnames, _)| {
            if host.bindings.get(internal).map_or(false, |b| b.enabled) {
                true
            } else {
                removed.insert((*external, std::mem::take(hostnames)));

                false
            }
        });
        for (lan, hostnames) in removed {
            if let Some(external) = lan.assigned_ssl_port {
                for hostname in ctrl.server_hostnames.iter().cloned() {
                    ctrl.vhost.gc(hostname, external)?;
                }
                for hostname in hostnames {
                    ctrl.vhost.gc(Some(hostname), external)?;
                }
            }
            if let Some(external) = lan.assigned_port {
                ctrl.forward.gc(external).await?;
            }
        }

        struct TorHostnamePorts {
            non_ssl: Option<u16>,
            ssl: Option<u16>,
        }
        let mut tor_hostname_ports = BTreeMap::<u16, TorHostnamePorts>::new();
        let mut tor_binds = OrdMap::<u16, SocketAddr>::new();
        for (internal, info) in &host.bindings {
            if !info.enabled {
                continue;
            }
            tor_binds.insert(
                info.options.preferred_external_port,
                SocketAddr::from((self.ip, *internal)),
            );
            if let (Some(ssl), Some(ssl_internal)) =
                (&info.options.add_ssl, info.net.assigned_ssl_port)
            {
                tor_binds.insert(
                    ssl.preferred_external_port,
                    SocketAddr::from(([127, 0, 0, 1], ssl_internal)),
                );
                tor_hostname_ports.insert(
                    *internal,
                    TorHostnamePorts {
                        non_ssl: Some(info.options.preferred_external_port)
                            .filter(|p| *p != ssl.preferred_external_port),
                        ssl: Some(ssl.preferred_external_port),
                    },
                );
            } else {
                tor_hostname_ports.insert(
                    *internal,
                    TorHostnamePorts {
                        non_ssl: Some(info.options.preferred_external_port),
                        ssl: None,
                    },
                );
            }
        }

        let mut keep_tor_addrs = BTreeSet::new();
        for tor_addr in host.addresses().filter_map(|a| {
            if let HostAddress::Onion { address } = a {
                Some(address)
            } else {
                None
            }
        }) {
            keep_tor_addrs.insert(tor_addr);
            let old_tor_bind = binds.tor.remove(tor_addr);
            let tor_bind = old_tor_bind.filter(|(ports, _)| ports == &tor_binds);
            let new_tor_bind = if let Some(tor_bind) = tor_bind {
                tor_bind
            } else {
                let key = peek
                    .as_private()
                    .as_key_store()
                    .as_onion()
                    .get_key(tor_addr)?;
                let rcs = ctrl
                    .tor
                    .add(key, tor_binds.clone().into_iter().collect())
                    .await?;
                (tor_binds.clone(), rcs)
            };
            for (internal, ports) in &tor_hostname_ports {
                let mut bind_hostname_info = hostname_info.remove(internal).unwrap_or_default();
                bind_hostname_info.push(HostnameInfo::Onion {
                    hostname: OnionHostname {
                        value: tor_addr.to_string(),
                        port: ports.non_ssl,
                        ssl_port: ports.ssl,
                    },
                });
                hostname_info.insert(*internal, bind_hostname_info);
            }
            binds.tor.insert(tor_addr.clone(), new_tor_bind);
        }
        for addr in binds.tor.keys() {
            if !keep_tor_addrs.contains(addr) {
                ctrl.tor.gc(Some(addr.clone()), None).await?;
            }
        }

        self.net_controller()?
            .db
            .mutate(|db| {
                host_for(db, &self.id, &id, host.kind)?
                    .as_hostname_info_mut()
                    .ser(&hostname_info)
            })
            .await?;
        Ok(())
    }

    pub async fn remove_all(mut self) -> Result<(), Error> {
        self.shutdown = true;
        if let Some(ctrl) = Weak::upgrade(&self.controller) {
            self.clear_bindings(Default::default()).await?;
            drop(ctrl);
            Ok(())
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

    pub fn get_lan_port(&self, host_id: HostId, internal_port: u16) -> Result<NetInfo, Error> {
        let host_id_binds = self.binds.get_key_value(&host_id);
        match host_id_binds {
            Some((_, binds)) => {
                if let Some((lan, _, _, _)) = binds.lan.get(&internal_port) {
                    Ok(*lan)
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
            tokio::spawn(async move { svc.remove_all().await.log_err() });
        }
    }
}
