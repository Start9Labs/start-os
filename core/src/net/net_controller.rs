use std::collections::{BTreeMap, BTreeSet};
use std::net::{IpAddr, Ipv4Addr, SocketAddr, SocketAddrV4};
use std::sync::{Arc, Weak};

use color_eyre::eyre::eyre;
use imbl_value::InternedString;
use nix::net::if_::if_nametoindex;
use patch_db::json_ptr::JsonPointer;
use tokio::process::Command;
use tokio::sync::Mutex;
use tokio::task::JoinHandle;
use tokio_rustls::rustls::ClientConfig as TlsClientConfig;
use tracing::instrument;

use crate::db::model::Database;
use crate::hostname::ServerHostname;
use crate::net::dns::DnsController;
use crate::net::forward::{
    ForwardRequirements, InterfacePortForwardController, START9_BRIDGE_IFACE, add_iptables_rule,
};
use crate::net::gateway::NetworkInterfaceController;
use crate::net::host::address::HostAddress;
use crate::net::host::binding::{AddSslOptions, BindId, BindOptions};
use crate::net::host::{Host, Hosts, host_for};
use crate::net::service_interface::HostnameMetadata;
use crate::net::socks::SocksController;
use crate::net::vhost::{AlpnInfo, DynVHostTarget, ProxyTarget, VHostController};
use crate::prelude::*;
use crate::service::effects::callbacks::ServiceCallbacks;
use crate::util::Invoke;
use crate::util::serde::MaybeUtf8String;
use crate::util::sync::Watch;
use crate::{GatewayId, HOST_IP, HostId, OptionExt, PackageId};

pub struct NetController {
    pub(crate) db: TypedPatchDb<Database>,
    pub(super) vhost: VHostController,
    pub(super) tls_client_config: Arc<TlsClientConfig>,
    pub(crate) net_iface: Arc<NetworkInterfaceController>,
    pub(super) dns: DnsController,
    pub(super) forward: InterfacePortForwardController,
    pub(super) socks: SocksController,
    pub(crate) callbacks: Arc<ServiceCallbacks>,
}

impl NetController {
    pub async fn init(db: TypedPatchDb<Database>, socks_listen: SocketAddr) -> Result<Self, Error> {
        let net_iface = Arc::new(NetworkInterfaceController::new(db.clone()));
        let socks = SocksController::new(socks_listen)?;
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
        add_iptables_rule(
            false,
            false,
            &[
                "FORWARD",
                "-i",
                START9_BRIDGE_IFACE,
                "-m",
                "state",
                "--state",
                "NEW",
                "-j",
                "ACCEPT",
            ],
        )
        .await?;
        Ok(Self {
            db: db.clone(),
            vhost: VHostController::new(db.clone(), net_iface.clone(), crypto_provider),
            tls_client_config,
            dns: DnsController::init(db, &net_iface.watcher).await?,
            forward: InterfacePortForwardController::new(net_iface.watcher.subscribe()),
            net_iface,
            socks,
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
    forwards: BTreeMap<u16, (SocketAddrV4, ForwardRequirements, Arc<()>)>,
    vhosts: BTreeMap<(Option<InternedString>, u16), (ProxyTarget, Arc<()>)>,
    private_dns: BTreeMap<InternedString, Arc<()>>,
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

    async fn update(&mut self, ctrl: &NetController, id: HostId, host: Host) -> Result<(), Error> {
        let mut forwards: BTreeMap<u16, (SocketAddrV4, ForwardRequirements)> = BTreeMap::new();
        let mut vhosts: BTreeMap<(Option<InternedString>, u16), ProxyTarget> = BTreeMap::new();
        let mut private_dns: BTreeSet<InternedString> = BTreeSet::new();
        let binds = self.binds.entry(id.clone()).or_default();

        let net_ifaces = ctrl.net_iface.watcher.ip_info();
        let host_addresses: Vec<_> = host.addresses().collect();

        // Collect private DNS entries (domains without public config)
        for HostAddress {
            address, public, ..
        } in &host_addresses
        {
            if public.is_none() {
                private_dns.insert(address.clone());
            }
        }

        // ── Build controller entries from enabled addresses ──
        for (port, bind) in host.bindings.iter() {
            if !bind.enabled {
                continue;
            }
            if bind.net.assigned_port.is_none() && bind.net.assigned_ssl_port.is_none() {
                continue;
            }

            let enabled_addresses = bind.addresses.enabled();
            let addr: SocketAddr = (self.ip, *port).into();

            // SSL vhosts
            if let Some(ssl) = &bind.options.add_ssl {
                let connect_ssl = if let Some(alpn) = ssl.alpn.clone() {
                    Err(alpn)
                } else if bind.options.secure.as_ref().map_or(false, |s| s.ssl) {
                    Ok(())
                } else {
                    Err(AlpnInfo::Reflect)
                };

                if let Some(assigned_ssl_port) = bind.net.assigned_ssl_port {
                    // Collect private IPs from enabled private addresses' gateways
                    let server_private_ips: BTreeSet<IpAddr> = enabled_addresses
                        .iter()
                        .filter(|a| !a.public)
                        .flat_map(|a| a.metadata.gateways())
                        .filter_map(|gw| net_ifaces.get(gw).and_then(|info| info.ip_info.as_ref()))
                        .flat_map(|ip_info| ip_info.subnets.iter().map(|s| s.addr()))
                        .collect();

                    // Collect public gateways from enabled public IP addresses
                    let server_public_gateways: BTreeSet<GatewayId> = enabled_addresses
                        .iter()
                        .filter(|a| a.public && a.metadata.is_ip())
                        .flat_map(|a| a.metadata.gateways())
                        .cloned()
                        .collect();

                    // * vhost (on assigned_ssl_port)
                    if !server_private_ips.is_empty() || !server_public_gateways.is_empty() {
                        vhosts.insert(
                            (None, assigned_ssl_port),
                            ProxyTarget {
                                public: server_public_gateways.clone(),
                                private: server_private_ips.clone(),
                                acme: None,
                                addr,
                                add_x_forwarded_headers: ssl.add_x_forwarded_headers,
                                connect_ssl: connect_ssl
                                    .clone()
                                    .map(|_| ctrl.tls_client_config.clone()),
                            },
                        );
                    }
                }

                // Domain vhosts: group by (domain, ssl_port), merge public/private sets
                for addr_info in &enabled_addresses {
                    if !addr_info.ssl {
                        continue;
                    }
                    match &addr_info.metadata {
                        HostnameMetadata::PublicDomain { .. }
                        | HostnameMetadata::PrivateDomain { .. } => {}
                        _ => continue,
                    }
                    let domain = &addr_info.hostname;
                    let domain_ssl_port = addr_info.port.unwrap_or(443);
                    let key = (Some(domain.clone()), domain_ssl_port);
                    let target = vhosts.entry(key).or_insert_with(|| ProxyTarget {
                        public: BTreeSet::new(),
                        private: BTreeSet::new(),
                        acme: host_addresses
                            .iter()
                            .find(|a| a.address == *domain)
                            .and_then(|a| a.public.as_ref())
                            .and_then(|p| p.acme.clone()),
                        addr,
                        add_x_forwarded_headers: ssl.add_x_forwarded_headers,
                        connect_ssl: connect_ssl.clone().map(|_| ctrl.tls_client_config.clone()),
                    });
                    if addr_info.public {
                        for gw in addr_info.metadata.gateways() {
                            target.public.insert(gw.clone());
                        }
                    } else {
                        for gw in addr_info.metadata.gateways() {
                            if let Some(info) = net_ifaces.get(gw) {
                                if let Some(ip_info) = &info.ip_info {
                                    for subnet in &ip_info.subnets {
                                        target.private.insert(subnet.addr());
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // Non-SSL forwards
            if bind
                .options
                .secure
                .map_or(true, |s| !(s.ssl && bind.options.add_ssl.is_some()))
            {
                let external = bind.net.assigned_port.or_not_found("assigned lan port")?;
                let fwd_public: BTreeSet<GatewayId> = enabled_addresses
                    .iter()
                    .filter(|a| a.public)
                    .flat_map(|a| a.metadata.gateways())
                    .cloned()
                    .collect();
                let fwd_private: BTreeSet<IpAddr> = enabled_addresses
                    .iter()
                    .filter(|a| !a.public)
                    .flat_map(|a| a.metadata.gateways())
                    .filter_map(|gw| net_ifaces.get(gw).and_then(|i| i.ip_info.as_ref()))
                    .flat_map(|ip| ip.subnets.iter().map(|s| s.addr()))
                    .collect();
                forwards.insert(
                    external,
                    (
                        SocketAddrV4::new(self.ip, *port),
                        ForwardRequirements {
                            public_gateways: fwd_public,
                            private_ips: fwd_private,
                            secure: bind.options.secure.is_some(),
                        },
                    ),
                );
            }
        }

        // ── Phase 3: Reconcile ──
        let all = binds
            .forwards
            .keys()
            .chain(forwards.keys())
            .copied()
            .collect::<BTreeSet<_>>();
        for external in all {
            let mut prev = binds.forwards.remove(&external);
            if let Some((internal, reqs)) = forwards.remove(&external) {
                prev = prev.filter(|(i, r, _)| i == &internal && *r == reqs);
                binds.forwards.insert(
                    external,
                    if let Some(prev) = prev {
                        prev
                    } else {
                        (
                            internal,
                            reqs.clone(),
                            ctrl.forward
                                .add(
                                    external,
                                    reqs,
                                    internal,
                                    net_ifaces
                                        .iter()
                                        .find_map(|(_, i)| {
                                            i.ip_info.as_ref().and_then(|i| {
                                                i.subnets.iter().find(|i| {
                                                    i.contains(&IpAddr::from(*internal.ip()))
                                                })
                                            })
                                        })
                                        .map(|s| s.prefix_len())
                                        .unwrap_or(32),
                                )
                                .await?,
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

        Ok(())
    }
}

pub struct NetService {
    shutdown: bool,
    data: Arc<Mutex<NetServiceData>>,
    sync_task: JoinHandle<()>,
    synced: Watch<u64>,
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
            synced: Watch::new(0u64),
        }
    }

    fn new(data: NetServiceData) -> Result<Self, Error> {
        let ctrl = data.net_controller()?;
        let pkg_id = data.id.clone();
        let db = ctrl.db.clone();
        drop(ctrl);

        let synced = Watch::new(0u64);
        let synced_writer = synced.clone();

        let ip = data.ip;
        let data = Arc::new(Mutex::new(data));
        let thread_data = data.clone();
        let sync_task = tokio::spawn(async move {
            if let Some(ref id) = pkg_id {
                let ptr: JsonPointer = format!("/public/packageData/{}/hosts", id).parse().unwrap();
                let mut watch = db.watch(ptr).await.typed::<Hosts>();

                // Outbound gateway enforcement
                let service_ip = ip.to_string();
                // Purge any stale rules from a previous instance
                loop {
                    if Command::new("ip")
                        .arg("rule")
                        .arg("del")
                        .arg("from")
                        .arg(&service_ip)
                        .arg("priority")
                        .arg("100")
                        .invoke(ErrorKind::Network)
                        .await
                        .is_err()
                    {
                        break;
                    }
                }
                let mut outbound_sub = db
                    .subscribe(
                        format!("/public/packageData/{}/outboundGateway", id)
                            .parse::<JsonPointer<_, _>>()
                            .unwrap(),
                    )
                    .await;
                let ctrl_for_ip = thread_data.lock().await.net_controller().ok();
                let mut ip_info_watch = ctrl_for_ip
                    .as_ref()
                    .map(|c| c.net_iface.watcher.subscribe());
                if let Some(ref mut w) = ip_info_watch {
                    w.mark_seen();
                }
                drop(ctrl_for_ip);
                let mut current_outbound_table: Option<u32> = None;

                loop {
                    let (hosts_changed, outbound_changed) = tokio::select! {
                        res = watch.changed() => {
                            if let Err(e) = res {
                                tracing::error!("DB watch disconnected for {id}: {e}");
                                break;
                            }
                            (true, false)
                        }
                        _ = outbound_sub.recv() => (false, true),
                        _ = async {
                            if let Some(ref mut w) = ip_info_watch {
                                w.changed().await;
                            } else {
                                std::future::pending::<()>().await;
                            }
                        } => (false, true),
                    };

                    // Handle host updates
                    if hosts_changed {
                        if let Err(e) = async {
                            let hosts = watch.peek()?.de()?;
                            let mut data = thread_data.lock().await;
                            let ctrl = data.net_controller()?;
                            for (host_id, host) in hosts.0 {
                                data.update(&*ctrl, host_id, host).await?;
                            }
                            Ok::<_, Error>(())
                        }
                        .await
                        {
                            tracing::error!("Failed to update network info for {id}: {e}");
                            tracing::debug!("{e:?}");
                        }
                    }

                    // Handle outbound gateway changes
                    if outbound_changed {
                        if let Err(e) = async {
                            // Remove old rule if any
                            if let Some(old_table) = current_outbound_table.take() {
                                let old_table_str = old_table.to_string();
                                let _ = Command::new("ip")
                                    .arg("rule")
                                    .arg("del")
                                    .arg("from")
                                    .arg(&service_ip)
                                    .arg("lookup")
                                    .arg(&old_table_str)
                                    .arg("priority")
                                    .arg("100")
                                    .invoke(ErrorKind::Network)
                                    .await;
                            }
                            // Read current outbound gateway from DB
                            let outbound_gw: Option<GatewayId> = db
                                .peek()
                                .await
                                .as_public()
                                .as_package_data()
                                .as_idx(id)
                                .map(|p| p.as_outbound_gateway().de().ok())
                                .flatten()
                                .flatten();
                            if let Some(gw_id) = outbound_gw {
                                // Look up table ID for this gateway
                                if let Some(table_id) = if_nametoindex(gw_id.as_str())
                                    .map(|idx| 1000 + idx)
                                    .log_err()
                                {
                                    let table_str = table_id.to_string();
                                    Command::new("ip")
                                        .arg("rule")
                                        .arg("add")
                                        .arg("from")
                                        .arg(&service_ip)
                                        .arg("lookup")
                                        .arg(&table_str)
                                        .arg("priority")
                                        .arg("100")
                                        .invoke(ErrorKind::Network)
                                        .await
                                        .log_err();
                                    current_outbound_table = Some(table_id);
                                }
                            }
                            Ok::<_, Error>(())
                        }
                        .await
                        {
                            tracing::error!("Failed to update outbound gateway for {id}: {e}");
                            tracing::debug!("{e:?}");
                        }
                    }

                    synced_writer.send_modify(|v| *v += 1);
                }

                // Cleanup outbound rule on task exit
                if let Some(table_id) = current_outbound_table {
                    let table_str = table_id.to_string();
                    let _ = Command::new("ip")
                        .arg("rule")
                        .arg("del")
                        .arg("from")
                        .arg(&service_ip)
                        .arg("lookup")
                        .arg(&table_str)
                        .arg("priority")
                        .arg("100")
                        .invoke(ErrorKind::Network)
                        .await;
                }
            } else {
                let ptr: JsonPointer = "/public/serverInfo/network/host".parse().unwrap();
                let mut watch = db.watch(ptr).await.typed::<Host>();
                loop {
                    if let Err(e) = watch.changed().await {
                        tracing::error!("DB watch disconnected for Main UI: {e}");
                        break;
                    }
                    if let Err(e) = async {
                        let host = watch.peek()?.de()?;
                        let mut data = thread_data.lock().await;
                        let ctrl = data.net_controller()?;
                        data.update(&*ctrl, HostId::default(), host).await?;
                        Ok::<_, Error>(())
                    }
                    .await
                    {
                        tracing::error!("Failed to update network info for Main UI: {e}");
                        tracing::debug!("{e:?}");
                    }
                    synced_writer.send_modify(|v| *v += 1);
                }
            }
        });

        Ok(Self {
            shutdown: false,
            data,
            sync_task,
            synced,
        })
    }

    pub async fn bind(
        &self,
        id: HostId,
        internal_port: u16,
        options: BindOptions,
    ) -> Result<(), Error> {
        let (ctrl, pkg_id) = {
            let data = self.data.lock().await;
            (data.net_controller()?, data.id.clone())
        };
        ctrl.db
            .mutate(|db| {
                let gateways = db
                    .as_public()
                    .as_server_info()
                    .as_network()
                    .as_gateways()
                    .de()?;
                let hostname = ServerHostname::load(db.as_public().as_server_info())?;
                let mut ports = db.as_private().as_available_ports().de()?;
                let host = host_for(db, pkg_id.as_ref(), &id)?;
                host.add_binding(&mut ports, internal_port, options)?;
                host.update_addresses(&hostname, &gateways, &ports)?;
                db.as_private_mut().as_available_ports_mut().ser(&ports)?;
                Ok(())
            })
            .await
            .result
    }

    pub async fn clear_bindings(&self, except: BTreeSet<BindId>) -> Result<(), Error> {
        let (ctrl, pkg_id) = {
            let data = self.data.lock().await;
            (data.net_controller()?, data.id.clone())
        };
        ctrl.db
            .mutate(|db| {
                let gateways = db
                    .as_public()
                    .as_server_info()
                    .as_network()
                    .as_gateways()
                    .de()?;
                let hostname = ServerHostname::load(db.as_public().as_server_info())?;
                let ports = db.as_private().as_available_ports().de()?;
                if let Some(ref pkg_id) = pkg_id {
                    for (host_id, host) in db
                        .as_public_mut()
                        .as_package_data_mut()
                        .as_idx_mut(pkg_id)
                        .or_not_found(pkg_id)?
                        .as_hosts_mut()
                        .as_entries_mut()?
                    {
                        host.as_bindings_mut().mutate(|b| {
                            for (internal_port, info) in b.iter_mut() {
                                if !except.contains(&BindId {
                                    id: host_id.clone(),
                                    internal_port: *internal_port,
                                }) {
                                    info.disable();
                                }
                            }
                            Ok(())
                        })?;
                        host.update_addresses(&hostname, &gateways, &ports)?;
                    }
                } else {
                    let host = db
                        .as_public_mut()
                        .as_server_info_mut()
                        .as_network_mut()
                        .as_host_mut();
                    host.as_bindings_mut().mutate(|b| {
                        for (internal_port, info) in b.iter_mut() {
                            if !except.contains(&BindId {
                                id: HostId::default(),
                                internal_port: *internal_port,
                            }) {
                                info.disable();
                            }
                        }
                        Ok(())
                    })?;
                    host.update_addresses(&hostname, &gateways, &ports)?;
                }
                Ok(())
            })
            .await
            .result
    }

    pub async fn sync_host(&self, _id: HostId) -> Result<(), Error> {
        let current = self.synced.peek(|v| *v);
        let mut w = self.synced.clone();
        w.wait_for(|v| *v > current).await;
        Ok(())
    }

    pub async fn remove_all(mut self) -> Result<(), Error> {
        if Weak::upgrade(&self.data.lock().await.controller).is_none() {
            self.shutdown = true;
            tracing::warn!("NetService dropped after NetController is shutdown");
            return Err(Error::new(
                eyre!("NetController is shutdown"),
                crate::ErrorKind::Network,
            ));
        }
        let current = self.synced.peek(|v| *v);
        self.clear_bindings(Default::default()).await?;
        let mut w = self.synced.clone();
        w.wait_for(|v| *v > current).await;
        self.sync_task.abort();
        // Clean up any outbound gateway ip rules for this service
        let service_ip = self.data.lock().await.ip.to_string();
        loop {
            if Command::new("ip")
                .arg("rule")
                .arg("del")
                .arg("from")
                .arg(&service_ip)
                .arg("priority")
                .arg("100")
                .invoke(ErrorKind::Network)
                .await
                .is_err()
            {
                break;
            }
        }
        self.shutdown = true;
        Ok(())
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
