use std::collections::{BTreeMap, BTreeSet};
use std::net::{Ipv4Addr, SocketAddr, SocketAddrV4};
use std::sync::{Arc, Weak};

use color_eyre::eyre::eyre;
use imbl::{OrdMap, vector};
use imbl_value::InternedString;
use ipnet::IpNet;
use models::{GatewayId, HostId, OptionExt, PackageId};
use tokio::sync::Mutex;
use tokio::task::JoinHandle;
use tokio_rustls::rustls::ClientConfig as TlsClientConfig;
use tracing::instrument;

use crate::HOST_IP;
use crate::db::model::Database;
use crate::db::model::public::NetworkInterfaceType;
use crate::error::ErrorCollection;
use crate::hostname::Hostname;
use crate::net::dns::DnsController;
use crate::net::forward::{InterfacePortForwardController, START9_BRIDGE_IFACE};
use crate::net::gateway::{
    AndFilter, DynInterfaceFilter, IdFilter, InterfaceFilter, NetworkInterfaceController, OrFilter,
    PublicFilter, SecureFilter, TypeFilter,
};
use crate::net::host::address::HostAddress;
use crate::net::host::binding::{AddSslOptions, BindId, BindOptions};
use crate::net::host::{Host, Hosts, host_for};
use crate::net::service_interface::{GatewayInfo, HostnameInfo, IpHostname, OnionHostname};
use crate::net::socks::SocksController;
use crate::net::tor::{OnionAddress, TorController, TorSecretKey};
use crate::net::utils::ipv6_is_local;
use crate::net::vhost::{AlpnInfo, DynVHostTarget, ProxyTarget, VHostController};
use crate::prelude::*;
use crate::service::effects::callbacks::ServiceCallbacks;
use crate::util::serde::MaybeUtf8String;

pub struct NetController {
    pub(crate) db: TypedPatchDb<Database>,
    pub(super) tor: TorController,
    pub(super) vhost: VHostController,
    pub(super) tls_client_config: Arc<TlsClientConfig>,
    pub(crate) net_iface: Arc<NetworkInterfaceController>,
    pub(super) dns: DnsController,
    pub(super) forward: InterfacePortForwardController,
    pub(super) socks: SocksController,
    pub(super) server_hostnames: Vec<Option<InternedString>>,
    pub(crate) callbacks: Arc<ServiceCallbacks>,
}

impl NetController {
    pub async fn init(
        db: TypedPatchDb<Database>,
        hostname: &Hostname,
        socks_listen: SocketAddr,
    ) -> Result<Self, Error> {
        let net_iface = Arc::new(NetworkInterfaceController::new(db.clone()));
        let tor = TorController::new()?;
        let socks = SocksController::new(socks_listen, tor.clone())?;
        let crypto_provider = Arc::new(tokio_rustls::rustls::crypto::ring::default_provider());
        let tls_client_config = Arc::new(crate::net::tls::client_config(
            crypto_provider.clone(),
            [&*db
                .peek()
                .await
                .as_private()
                .as_key_store()
                .as_local_certs()
                .as_root_cert()
                .de()?
                .0],
        )?);
        Ok(Self {
            db: db.clone(),
            tor,
            vhost: VHostController::new(db.clone(), net_iface.clone(), crypto_provider),
            tls_client_config,
            dns: DnsController::init(db, &net_iface.watcher).await?,
            forward: InterfacePortForwardController::new(net_iface.watcher.subscribe()),
            net_iface,
            socks,
            server_hostnames: vec![
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
            ],
            callbacks: Arc::new(ServiceCallbacks::default()),
        })
    }

    #[instrument(skip_all)]
    pub async fn create_service(
        self: &Arc<Self>,
        package: PackageId,
        ip: Ipv4Addr,
    ) -> Result<NetService, Error> {
        let dns = self.dns.add_service(Some(package.clone()), ip)?;

        let res = NetService::new(NetServiceData {
            id: Some(package),
            ip,
            _dns: dns,
            controller: Arc::downgrade(self),
            binds: BTreeMap::new(),
        })?;
        res.clear_bindings(Default::default()).await?;
        Ok(res)
    }

    pub async fn os_bindings(self: &Arc<Self>) -> Result<NetService, Error> {
        let dns = self.dns.add_service(None, HOST_IP.into())?;

        let service = NetService::new(NetServiceData {
            id: None,
            ip: [127, 0, 0, 1].into(),
            _dns: dns,
            controller: Arc::downgrade(self),
            binds: BTreeMap::new(),
        })?;
        service.clear_bindings(Default::default()).await?;
        service
            .bind(
                HostId::default(),
                80,
                BindOptions {
                    preferred_external_port: 80,
                    add_ssl: Some(AddSslOptions {
                        preferred_external_port: 443,
                        add_x_forwarded_headers: false,
                        alpn: Some(AlpnInfo::Specified(vec![
                            MaybeUtf8String("h2".into()),
                            MaybeUtf8String("http/1.1".into()),
                        ])),
                    }),
                    secure: None,
                },
            )
            .await?;

        Ok(service)
    }
}

#[derive(Default, Debug)]
struct HostBinds {
    forwards: BTreeMap<u16, (SocketAddrV4, DynInterfaceFilter, Arc<()>)>,
    vhosts: BTreeMap<(Option<InternedString>, u16), (ProxyTarget, Arc<()>)>,
    private_dns: BTreeMap<InternedString, Arc<()>>,
    tor: BTreeMap<OnionAddress, (OrdMap<u16, SocketAddr>, Vec<Arc<()>>)>,
}

pub struct NetServiceData {
    id: Option<PackageId>,
    ip: Ipv4Addr,
    _dns: Arc<()>,
    controller: Weak<NetController>,
    binds: BTreeMap<HostId, HostBinds>,
}
impl NetServiceData {
    fn net_controller(&self) -> Result<Arc<NetController>, Error> {
        Weak::upgrade(&self.controller).ok_or_else(|| {
            Error::new(
                eyre!("NetController is shutdown"),
                crate::ErrorKind::Network,
            )
        })
    }

    async fn clear_bindings(
        &mut self,
        ctrl: &NetController,
        except: BTreeSet<BindId>,
    ) -> Result<(), Error> {
        if let Some(pkg_id) = &self.id {
            let hosts = ctrl
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
                .await
                .result?;
            let mut errors = ErrorCollection::new();
            for (id, host) in hosts.0 {
                errors.handle(self.update(ctrl, id, host).await);
            }
            errors.into_result()
        } else {
            let host = ctrl
                .db
                .mutate(|db| {
                    let host = db
                        .as_public_mut()
                        .as_server_info_mut()
                        .as_network_mut()
                        .as_host_mut();
                    host.as_bindings_mut().mutate(|b| {
                        for (internal_port, info) in b {
                            if !except.contains(&BindId {
                                id: HostId::default(),
                                internal_port: *internal_port,
                            }) {
                                info.disable();
                            }
                        }
                        Ok(())
                    })?;
                    host.de()
                })
                .await
                .result?;
            self.update(ctrl, HostId::default(), host).await
        }
    }

    async fn update(&mut self, ctrl: &NetController, id: HostId, host: Host) -> Result<(), Error> {
        let mut forwards: BTreeMap<u16, (SocketAddrV4, DynInterfaceFilter)> = BTreeMap::new();
        let mut vhosts: BTreeMap<(Option<InternedString>, u16), ProxyTarget> = BTreeMap::new();
        let mut private_dns: BTreeSet<InternedString> = BTreeSet::new();
        let mut tor: BTreeMap<OnionAddress, (TorSecretKey, OrdMap<u16, SocketAddr>)> =
            BTreeMap::new();
        let mut hostname_info: BTreeMap<u16, Vec<HostnameInfo>> = BTreeMap::new();
        let binds = self.binds.entry(id.clone()).or_default();

        let peek = ctrl.db.peek().await;

        // LAN
        let server_info = peek.as_public().as_server_info();
        let net_ifaces = ctrl.net_iface.watcher.ip_info();
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
                            ProxyTarget {
                                filter: bind.net.clone().into_dyn(),
                                acme: None,
                                addr,
                                add_x_forwarded_headers: ssl.add_x_forwarded_headers,
                                connect_ssl: connect_ssl
                                    .clone()
                                    .map(|_| ctrl.tls_client_config.clone()),
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
                                        ProxyTarget {
                                            filter: OrFilter(
                                                TypeFilter(NetworkInterfaceType::Loopback),
                                                IdFilter(GatewayId::from(InternedString::from(
                                                    START9_BRIDGE_IFACE,
                                                ))),
                                            )
                                            .into_dyn(),
                                            acme: None,
                                            addr,
                                            add_x_forwarded_headers: ssl.add_x_forwarded_headers,
                                            connect_ssl: connect_ssl
                                                .clone()
                                                .map(|_| ctrl.tls_client_config.clone()),
                                        },
                                    ); // TODO: wrap onion ssl stream directly in tor ctrl
                                }
                            }
                            HostAddress::Domain {
                                address,
                                public,
                                private,
                            } => {
                                if hostnames.insert(address.clone()) {
                                    let address = Some(address.clone());
                                    if ssl.preferred_external_port == 443 {
                                        if let Some(public) = &public {
                                            vhosts.insert(
                                                (address.clone(), 5443),
                                                ProxyTarget {
                                                    filter: AndFilter(
                                                        bind.net.clone(),
                                                        AndFilter(
                                                            IdFilter(public.gateway.clone()),
                                                            PublicFilter { public: false },
                                                        ),
                                                    )
                                                    .into_dyn(),
                                                    acme: public.acme.clone(),
                                                    addr,
                                                    add_x_forwarded_headers: ssl
                                                        .add_x_forwarded_headers,
                                                    connect_ssl: connect_ssl
                                                        .clone()
                                                        .map(|_| ctrl.tls_client_config.clone()),
                                                },
                                            );
                                            vhosts.insert(
                                                (address.clone(), 443),
                                                ProxyTarget {
                                                    filter: AndFilter(
                                                        bind.net.clone(),
                                                        if private {
                                                            OrFilter(
                                                                IdFilter(public.gateway.clone()),
                                                                PublicFilter { public: false },
                                                            )
                                                            .into_dyn()
                                                        } else {
                                                            AndFilter(
                                                                IdFilter(public.gateway.clone()),
                                                                PublicFilter { public: true },
                                                            )
                                                            .into_dyn()
                                                        },
                                                    )
                                                    .into_dyn(),
                                                    acme: public.acme.clone(),
                                                    addr,
                                                    add_x_forwarded_headers: ssl
                                                        .add_x_forwarded_headers,
                                                    connect_ssl: connect_ssl
                                                        .clone()
                                                        .map(|_| ctrl.tls_client_config.clone()),
                                                },
                                            );
                                        } else {
                                            vhosts.insert(
                                                (address.clone(), 443),
                                                ProxyTarget {
                                                    filter: AndFilter(
                                                        bind.net.clone(),
                                                        PublicFilter { public: false },
                                                    )
                                                    .into_dyn(),
                                                    acme: None,
                                                    addr,
                                                    add_x_forwarded_headers: ssl
                                                        .add_x_forwarded_headers,
                                                    connect_ssl: connect_ssl
                                                        .clone()
                                                        .map(|_| ctrl.tls_client_config.clone()),
                                                },
                                            );
                                        }
                                    } else {
                                        if let Some(public) = public {
                                            vhosts.insert(
                                                (address.clone(), external),
                                                ProxyTarget {
                                                    filter: AndFilter(
                                                        bind.net.clone(),
                                                        if private {
                                                            OrFilter(
                                                                IdFilter(public.gateway.clone()),
                                                                PublicFilter { public: false },
                                                            )
                                                            .into_dyn()
                                                        } else {
                                                            IdFilter(public.gateway.clone())
                                                                .into_dyn()
                                                        },
                                                    )
                                                    .into_dyn(),
                                                    acme: public.acme.clone(),
                                                    addr,
                                                    add_x_forwarded_headers: ssl
                                                        .add_x_forwarded_headers,
                                                    connect_ssl: connect_ssl
                                                        .clone()
                                                        .map(|_| ctrl.tls_client_config.clone()),
                                                },
                                            );
                                        } else {
                                            vhosts.insert(
                                                (address.clone(), external),
                                                ProxyTarget {
                                                    filter: AndFilter(
                                                        bind.net.clone(),
                                                        PublicFilter { public: false },
                                                    )
                                                    .into_dyn(),
                                                    acme: None,
                                                    addr,
                                                    add_x_forwarded_headers: ssl
                                                        .add_x_forwarded_headers,
                                                    connect_ssl: connect_ssl
                                                        .clone()
                                                        .map(|_| ctrl.tls_client_config.clone()),
                                                },
                                            );
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                if bind
                    .options
                    .secure
                    .map_or(true, |s| !(s.ssl && bind.options.add_ssl.is_some()))
                {
                    let external = bind.net.assigned_port.or_not_found("assigned lan port")?;
                    forwards.insert(
                        external,
                        (
                            SocketAddrV4::new(self.ip, *port),
                            AndFilter(
                                SecureFilter {
                                    secure: bind.options.secure.is_some(),
                                },
                                bind.net.clone(),
                            )
                            .into_dyn(),
                        ),
                    );
                }
                let mut bind_hostname_info: Vec<HostnameInfo> =
                    hostname_info.remove(port).unwrap_or_default();
                for (gateway_id, info) in net_ifaces
                    .iter()
                    .filter(|(_, info)| {
                        info.ip_info.as_ref().map_or(false, |i| {
                            !matches!(i.device_type, Some(NetworkInterfaceType::Bridge))
                        })
                    })
                    .filter(|(id, info)| bind.net.filter(id, info))
                {
                    let gateway = GatewayInfo {
                        id: gateway_id.clone(),
                        name: info
                            .name
                            .clone()
                            .or_else(|| info.ip_info.as_ref().map(|i| i.name.clone()))
                            .unwrap_or_else(|| gateway_id.clone().into()),
                        public: info.public(),
                    };
                    let port = bind.net.assigned_port.filter(|_| {
                        bind.options.secure.map_or(false, |s| {
                            !(s.ssl && bind.options.add_ssl.is_some()) || info.secure()
                        })
                    });
                    if !info.public()
                        && info.ip_info.as_ref().map_or(false, |i| {
                            i.device_type != Some(NetworkInterfaceType::Wireguard)
                        })
                    {
                        bind_hostname_info.push(HostnameInfo::Ip {
                            gateway: gateway.clone(),
                            public: false,
                            hostname: IpHostname::Local {
                                value: InternedString::from_display(&{
                                    let hostname = &hostname;
                                    lazy_format!("{hostname}.local")
                                }),
                                port,
                                ssl_port: bind.net.assigned_ssl_port,
                            },
                        });
                    }
                    for address in host.addresses() {
                        if let HostAddress::Domain {
                            address,
                            public,
                            private,
                        } = address
                        {
                            if public.is_none() {
                                private_dns.insert(address.clone());
                            }
                            let private = private && !info.public();
                            let public =
                                public.as_ref().map_or(false, |p| &p.gateway == gateway_id);
                            if public || private {
                                if bind
                                    .options
                                    .add_ssl
                                    .as_ref()
                                    .map_or(false, |ssl| ssl.preferred_external_port == 443)
                                {
                                    bind_hostname_info.push(HostnameInfo::Ip {
                                        gateway: gateway.clone(),
                                        public,
                                        hostname: IpHostname::Domain {
                                            value: address.clone(),
                                            port: None,
                                            ssl_port: Some(443),
                                        },
                                    });
                                } else {
                                    bind_hostname_info.push(HostnameInfo::Ip {
                                        gateway: gateway.clone(),
                                        public,
                                        hostname: IpHostname::Domain {
                                            value: address.clone(),
                                            port,
                                            ssl_port: bind.net.assigned_ssl_port,
                                        },
                                    });
                                }
                            }
                        }
                    }
                    if let Some(ip_info) = &info.ip_info {
                        let public = info.public();
                        if let Some(wan_ip) = ip_info.wan_ip {
                            bind_hostname_info.push(HostnameInfo::Ip {
                                gateway: gateway.clone(),
                                public: true,
                                hostname: IpHostname::Ipv4 {
                                    value: wan_ip,
                                    port,
                                    ssl_port: bind.net.assigned_ssl_port,
                                },
                            });
                        }
                        for ipnet in &ip_info.subnets {
                            match ipnet {
                                IpNet::V4(net) => {
                                    if !public {
                                        bind_hostname_info.push(HostnameInfo::Ip {
                                            gateway: gateway.clone(),
                                            public,
                                            hostname: IpHostname::Ipv4 {
                                                value: net.addr(),
                                                port,
                                                ssl_port: bind.net.assigned_ssl_port,
                                            },
                                        });
                                    }
                                }
                                IpNet::V6(net) => {
                                    bind_hostname_info.push(HostnameInfo::Ip {
                                        gateway: gateway.clone(),
                                        public: public && !ipv6_is_local(net.addr()),
                                        hostname: IpHostname::Ipv6 {
                                            value: net.addr(),
                                            scope_id: ip_info.scope_id,
                                            port,
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
            tor.insert(key.onion_address(), (key, tor_binds.clone()));
            for (internal, ports) in &tor_hostname_ports {
                let mut bind_hostname_info = hostname_info.remove(internal).unwrap_or_default();
                bind_hostname_info.push(HostnameInfo::Onion {
                    hostname: OnionHostname {
                        value: InternedString::from_display(tor_addr),
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
            if let Some((internal, filter)) = forwards.remove(&external) {
                prev = prev.filter(|(i, f, _)| i == &internal && *f == filter);
                binds.forwards.insert(
                    external,
                    if let Some(prev) = prev {
                        prev
                    } else {
                        (
                            internal,
                            filter.clone(),
                            ctrl.forward.add(external, filter, internal).await?,
                        )
                    },
                );
            }
        }
        ctrl.forward.gc().await?;

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
                        (
                            target.clone(),
                            ctrl.vhost.add(key.0, key.1, DynVHostTarget::new(target))?,
                        )
                    },
                );
            } else {
                if let Some((_, rc)) = prev {
                    drop(rc);
                    ctrl.vhost.gc(key.0, key.1);
                }
            }
        }

        let mut rm = BTreeSet::new();
        binds.private_dns.retain(|fqdn, _| {
            if private_dns.remove(fqdn) {
                true
            } else {
                rm.insert(fqdn.clone());
                false
            }
        });
        for fqdn in private_dns {
            binds
                .private_dns
                .insert(fqdn.clone(), ctrl.dns.add_private_domain(fqdn)?);
        }
        ctrl.dns.gc_private_domains(&rm)?;

        let all = binds
            .tor
            .keys()
            .chain(tor.keys())
            .cloned()
            .collect::<BTreeSet<_>>();
        for onion in all {
            let mut prev = binds.tor.remove(&onion);
            if let Some((key, tor_binds)) = tor.remove(&onion).filter(|(_, b)| !b.is_empty()) {
                prev = prev.filter(|(b, _)| b == &tor_binds);
                binds.tor.insert(
                    onion,
                    if let Some(prev) = prev {
                        prev
                    } else {
                        let service = ctrl.tor.service(key)?;
                        let rcs = service.proxy_all(tor_binds.iter().map(|(k, v)| (*k, *v)));
                        (tor_binds, rcs)
                    },
                );
            } else {
                if let Some((_, rc)) = prev {
                    drop(rc);
                    ctrl.tor.gc(Some(onion)).await?;
                }
            }
        }

        let res = ctrl
            .db
            .mutate(|db| {
                host_for(db, self.id.as_ref(), &id)?
                    .as_hostname_info_mut()
                    .ser(&hostname_info)
            })
            .await;
        res.result?;
        if let Some(pkg_id) = self.id.as_ref() {
            if res.revision.is_some() {
                if let Some(cbs) = ctrl.callbacks.get_host_info(&(pkg_id.clone(), id)) {
                    cbs.call(vector![]).await?;
                }
            }
        }
        Ok(())
    }

    async fn update_all(&mut self) -> Result<(), Error> {
        let ctrl = self.net_controller()?;
        if let Some(id) = self.id.clone() {
            for (host_id, host) in ctrl
                .db
                .peek()
                .await
                .as_public()
                .as_package_data()
                .as_idx(&id)
                .or_not_found(&id)?
                .as_hosts()
                .as_entries()?
            {
                tracing::info!("Updating host {host_id} for {id}");
                self.update(&*ctrl, host_id.clone(), host.de()?).await?;
                tracing::info!("Updated host {host_id} for {id}");
            }
        } else {
            tracing::info!("Updating host for Main UI");
            self.update(
                &*ctrl,
                HostId::default(),
                ctrl.db
                    .peek()
                    .await
                    .as_public()
                    .as_server_info()
                    .as_network()
                    .as_host()
                    .de()?,
            )
            .await?;
            tracing::info!("Updated host for Main UI");
        }
        Ok(())
    }
}

pub struct NetService {
    shutdown: bool,
    data: Arc<Mutex<NetServiceData>>,
    sync_task: JoinHandle<()>,
}
impl NetService {
    fn dummy() -> Self {
        Self {
            shutdown: true,
            data: Arc::new(Mutex::new(NetServiceData {
                id: None,
                ip: Ipv4Addr::new(0, 0, 0, 0),
                _dns: Default::default(),
                controller: Default::default(),
                binds: BTreeMap::new(),
            })),
            sync_task: tokio::spawn(futures::future::ready(())),
        }
    }

    fn new(data: NetServiceData) -> Result<Self, Error> {
        let mut ip_info = data.net_controller()?.net_iface.watcher.subscribe();
        let data = Arc::new(Mutex::new(data));
        let thread_data = data.clone();
        let sync_task = tokio::spawn(async move {
            loop {
                if let Err(e) = thread_data.lock().await.update_all().await {
                    tracing::error!("Failed to update network info: {e}");
                    tracing::debug!("{e:?}");
                }
                ip_info.changed().await;
            }
        });
        Ok(Self {
            shutdown: false,
            data,
            sync_task,
        })
    }

    pub async fn bind(
        &self,
        id: HostId,
        internal_port: u16,
        options: BindOptions,
    ) -> Result<(), Error> {
        let mut data = self.data.lock().await;
        let pkg_id = &data.id;
        let ctrl = data.net_controller()?;
        let host = ctrl
            .db
            .mutate(|db| {
                let mut ports = db.as_private().as_available_ports().de()?;
                let host = host_for(db, pkg_id.as_ref(), &id)?;
                host.add_binding(&mut ports, internal_port, options)?;
                let host = host.de()?;
                db.as_private_mut().as_available_ports_mut().ser(&ports)?;
                Ok(host)
            })
            .await
            .result?;
        data.update(&*ctrl, id, host).await
    }

    pub async fn clear_bindings(&self, except: BTreeSet<BindId>) -> Result<(), Error> {
        let mut data = self.data.lock().await;
        let ctrl = data.net_controller()?;
        data.clear_bindings(&*ctrl, except).await
    }

    pub async fn update(&self, id: HostId, host: Host) -> Result<(), Error> {
        let mut data = self.data.lock().await;
        let ctrl = data.net_controller()?;
        data.update(&*ctrl, id, host).await
    }

    pub async fn sync_host(&self, id: HostId) -> Result<(), Error> {
        let mut data = self.data.lock().await;
        let ctrl = data.net_controller()?;
        let host = host_for(&mut ctrl.db.peek().await, data.id.as_ref(), &id)?.de()?;
        data.update(&*ctrl, id, host).await
    }

    pub async fn remove_all(mut self) -> Result<(), Error> {
        self.sync_task.abort();
        let mut data = self.data.lock().await;
        if let Some(ctrl) = Weak::upgrade(&data.controller) {
            self.shutdown = true;
            data.clear_bindings(&*ctrl, Default::default()).await?;

            drop(ctrl);
            Ok(())
        } else {
            self.shutdown = true;
            tracing::warn!("NetService dropped after NetController is shutdown");
            Err(Error::new(
                eyre!("NetController is shutdown"),
                crate::ErrorKind::Network,
            ))
        }
    }

    pub async fn get_ip(&self) -> Ipv4Addr {
        self.data.lock().await.ip
    }
}

impl Drop for NetService {
    fn drop(&mut self) {
        if !self.shutdown {
            let svc = std::mem::replace(self, Self::dummy());
            tokio::spawn(async move { svc.remove_all().await.log_err() });
        }
    }
}
