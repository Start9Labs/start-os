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
use crate::net::host::binding::{BindId, BindOptions};
use crate::net::host::{host_for, Host, HostKind, Hosts};
use crate::net::network_interface::NetworkInterfaceController;
use crate::net::service_interface::{HostnameInfo, IpHostname, OnionHostname};
use crate::net::tor::TorController;
use crate::net::utils::ipv6_is_local;
use crate::net::vhost::{AlpnInfo, TargetInfo, VHostController};
use crate::prelude::*;
use crate::util::serde::MaybeUtf8String;
use crate::HOST_IP;

pub struct PreInitNetController {
    pub db: TypedPatchDb<Database>,
    tor: TorController,
    vhost: VHostController,
    pub net_iface: Arc<NetworkInterfaceController>,
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

        let vhost_target = TargetInfo {
            public: false,
            acme: None,
            addr: ([127, 0, 0, 1], 80).into(),
            connect_ssl: Err(AlpnInfo::Specified(vec![
                MaybeUtf8String("http/1.1".into()),
                MaybeUtf8String("h2".into()),
            ])),
        };

        for hostname in self.server_hostnames.iter().cloned() {
            self.os_bindings
                .push(self.vhost.add(hostname, 443, vhost_target.clone())?);
        }

        // Tor
        self.os_bindings.push(self.vhost.add(
            Some(InternedString::from_display(
                &tor_key.public().get_onion_address(),
            )),
            443,
            vhost_target,
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
    forwards: BTreeMap<u16, (SocketAddr, Arc<()>)>,
    vhosts: BTreeMap<(Option<InternedString>, u16), (TargetInfo, Arc<()>)>,
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
        crate::dbg!("bind", &kind, &id, internal_port, &options);
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
        let mut forwards: BTreeMap<u16, SocketAddr> = BTreeMap::new();
        let mut vhosts: BTreeMap<(Option<InternedString>, u16), TargetInfo> = BTreeMap::new();
        let mut tor: BTreeMap<OnionAddressV3, (TorSecretKeyV3, OrdMap<u16, SocketAddr>)> =
            BTreeMap::new();
        let mut hostname_info: BTreeMap<u16, Vec<HostnameInfo>> = BTreeMap::new();
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
            if bind.net.assigned_port.is_some() || bind.net.assigned_ssl_port.is_some() {
                let mut hostnames = BTreeSet::new();
                if let Some(ssl) = &bind.options.add_ssl {
                    let external = bind
                        .net
                        .assigned_ssl_port
                        .or_not_found("assigned ssl port")?;
                    let addr = (self.ip, *port).into();
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
                        vhosts.insert(
                            (hostname, external),
                            TargetInfo {
                                public: bind.net.public,
                                acme: None,
                                addr,
                                connect_ssl: connect_ssl.clone(),
                            },
                        );
                    }
                    for address in host.addresses() {
                        match address {
                            HostAddress::Onion { address } => {
                                let hostname = InternedString::from_display(&address);
                                if hostnames.insert(hostname.clone()) {
                                    vhosts.insert(
                                        (Some(hostname), external),
                                        TargetInfo {
                                            public: false,
                                            acme: None,
                                            addr,
                                            connect_ssl: connect_ssl.clone(),
                                        },
                                    );
                                }
                            }
                            HostAddress::Domain {
                                address,
                                public,
                                acme,
                            } => {
                                if hostnames.insert(address.clone()) {
                                    let address = Some(address.clone());
                                    if ssl.preferred_external_port == 443 {
                                        if public && bind.net.public {
                                            vhosts.insert(
                                                (address.clone(), 5443),
                                                TargetInfo {
                                                    public: false,
                                                    acme: acme.clone(),
                                                    addr,
                                                    connect_ssl: connect_ssl.clone(),
                                                },
                                            );
                                        }
                                        vhosts.insert(
                                            (address.clone(), 443),
                                            TargetInfo {
                                                public: public && bind.net.public,
                                                acme,
                                                addr,
                                                connect_ssl: connect_ssl.clone(),
                                            },
                                        );
                                    } else {
                                        vhosts.insert(
                                            (address.clone(), external),
                                            TargetInfo {
                                                public: public && bind.net.public,
                                                acme,
                                                addr,
                                                connect_ssl: connect_ssl.clone(),
                                            },
                                        );
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
                        let external = bind.net.assigned_port.or_not_found("assigned lan port")?;
                        forwards.insert(external, (self.ip, *port).into());
                    }
                }
                let mut bind_hostname_info: Vec<HostnameInfo> =
                    hostname_info.remove(port).unwrap_or_default();
                for (interface, public, ip_info) in
                    net_ifaces.iter().filter_map(|(interface, info)| {
                        if let Some(ip_info) = &info.ip_info {
                            Some((interface, info.public(), ip_info))
                        } else {
                            None
                        }
                    })
                {
                    if !public {
                        bind_hostname_info.push(HostnameInfo::Ip {
                            network_interface_id: interface.clone(),
                            public: false,
                            hostname: IpHostname::Local {
                                value: InternedString::from_display(&{
                                    let hostname = &hostname;
                                    lazy_format!("{hostname}.local")
                                }),
                                port: bind.net.assigned_port,
                                ssl_port: bind.net.assigned_ssl_port,
                            },
                        });
                    }
                    for address in host.addresses() {
                        if let HostAddress::Domain {
                            address,
                            public: domain_public,
                            ..
                        } = address
                        {
                            if !public || (domain_public && bind.net.public) {
                                if bind
                                    .options
                                    .add_ssl
                                    .as_ref()
                                    .map_or(false, |ssl| ssl.preferred_external_port == 443)
                                {
                                    bind_hostname_info.push(HostnameInfo::Ip {
                                        network_interface_id: interface.clone(),
                                        public: public && domain_public && bind.net.public, // TODO: check if port forward is active
                                        hostname: IpHostname::Domain {
                                            domain: address.clone(),
                                            subdomain: None,
                                            port: None,
                                            ssl_port: Some(443),
                                        },
                                    });
                                } else {
                                    bind_hostname_info.push(HostnameInfo::Ip {
                                        network_interface_id: interface.clone(),
                                        public,
                                        hostname: IpHostname::Domain {
                                            domain: address.clone(),
                                            subdomain: None,
                                            port: bind.net.assigned_port,
                                            ssl_port: bind.net.assigned_ssl_port,
                                        },
                                    });
                                }
                            }
                        }
                    }
                    if !public || bind.net.public {
                        if let Some(wan_ip) = ip_info.wan_ip.filter(|_| public) {
                            bind_hostname_info.push(HostnameInfo::Ip {
                                network_interface_id: interface.clone(),
                                public,
                                hostname: IpHostname::Ipv4 {
                                    value: wan_ip,
                                    port: bind.net.assigned_port,
                                    ssl_port: bind.net.assigned_ssl_port,
                                },
                            });
                        }
                        for ipnet in &ip_info.subnets {
                            match ipnet {
                                IpNet::V4(net) => {
                                    if !public {
                                        bind_hostname_info.push(HostnameInfo::Ip {
                                            network_interface_id: interface.clone(),
                                            public,
                                            hostname: IpHostname::Ipv4 {
                                                value: net.addr(),
                                                port: bind.net.assigned_port,
                                                ssl_port: bind.net.assigned_ssl_port,
                                            },
                                        });
                                    }
                                }
                                IpNet::V6(net) => {
                                    bind_hostname_info.push(HostnameInfo::Ip {
                                        network_interface_id: interface.clone(),
                                        public: public && !ipv6_is_local(net.addr()),
                                        hostname: IpHostname::Ipv6 {
                                            value: net.addr(),
                                            scope_id: ip_info.scope_id,
                                            port: bind.net.assigned_port,
                                            ssl_port: bind.net.assigned_ssl_port,
                                        },
                                    });
                                }
                            }
                        }
                    }
                }
                hostname_info.insert(*port, bind_hostname_info);
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

        for tor_addr in host.onions.iter() {
            let key = peek
                .as_private()
                .as_key_store()
                .as_onion()
                .get_key(tor_addr)?;
            tor.insert(key.public().get_onion_address(), (key, tor_binds.clone()));
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
        }

        let all = binds
            .forwards
            .keys()
            .chain(forwards.keys())
            .copied()
            .collect::<BTreeSet<_>>();
        for external in all {
            let mut prev = binds.forwards.remove(&external);
            if let Some(internal) = forwards.remove(&external) {
                prev = prev.filter(|(i, _)| i == &internal);
                binds.forwards.insert(
                    external,
                    if let Some(prev) = prev {
                        prev
                    } else {
                        (internal, ctrl.forward.add(external, internal).await?)
                    },
                );
            } else {
                if let Some((_, rc)) = prev {
                    drop(rc);
                    ctrl.forward.gc(external).await?;
                }
            }
        }

        let all = binds
            .vhosts
            .keys()
            .chain(vhosts.keys())
            .cloned()
            .collect::<BTreeSet<_>>();
        for key in all {
            let mut prev = binds.vhosts.remove(&key);
            if let Some(target) = vhosts.remove(&key) {
                prev = prev.filter(|(t, _)| t == &target);
                binds.vhosts.insert(
                    key.clone(),
                    if let Some(prev) = prev {
                        prev
                    } else {
                        (target.clone(), ctrl.vhost.add(key.0, key.1, target)?)
                    },
                );
            } else {
                if let Some((_, rc)) = prev {
                    drop(rc);
                    ctrl.vhost.gc(key.0, key.1);
                }
            }
        }

        let all = binds
            .tor
            .keys()
            .chain(tor.keys())
            .cloned()
            .collect::<BTreeSet<_>>();
        for onion in all {
            let mut prev = binds.tor.remove(&onion);
            if let Some((key, tor_binds)) = tor.remove(&onion) {
                prev = prev.filter(|(b, _)| b == &tor_binds);
                binds.tor.insert(
                    onion,
                    if let Some(prev) = prev {
                        prev
                    } else {
                        let rcs = ctrl
                            .tor
                            .add(key, tor_binds.iter().map(|(k, v)| (*k, *v)).collect())
                            .await?;
                        (tor_binds, rcs)
                    },
                );
            } else {
                if let Some((_, rc)) = prev {
                    drop(rc);
                    ctrl.tor.gc(Some(onion), None).await?;
                }
            }
        }

        ctrl.db
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
