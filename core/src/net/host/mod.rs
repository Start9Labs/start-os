use std::collections::{BTreeMap, BTreeSet};
use std::net::{IpAddr, SocketAddrV4};
use std::panic::RefUnwindSafe;

use clap::Parser;
use imbl::OrdMap;
use imbl_value::InternedString;
use itertools::Itertools;
use patch_db::DestructureMut;
use rpc_toolkit::{Context, Empty, HandlerExt, OrEmpty, ParentHandler, from_fn_async};
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::context::RpcContext;
use crate::db::model::DatabaseModel;
use crate::db::model::public::{NetworkInterfaceInfo, NetworkInterfaceType};
use crate::hostname::ServerHostname;
use crate::net::forward::AvailablePorts;
use crate::net::host::address::{HostAddress, PublicDomainConfig, address_api};
use crate::net::host::binding::{
    BindInfo, BindOptions, BindingRanges, Bindings, RangeBindInfo, binding,
};
use crate::net::service_interface::{HostnameInfo, HostnameMetadata};
use crate::prelude::*;
use crate::{GatewayId, HostId, PackageId};

pub mod address;
pub mod binding;

#[derive(Debug, Default, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct Host {
    pub bindings: Bindings,
    #[serde(default)]
    pub binding_ranges: BindingRanges,
    pub public_domains: BTreeMap<InternedString, PublicDomainConfig>,
    pub private_domains: BTreeMap<InternedString, BTreeSet<GatewayId>>,
    /// COMPUTED: port forwarding rules needed on gateways for public addresses to work.
    #[serde(default)]
    pub port_forwards: BTreeSet<PortForward>,
}

fn default_port_forward_count() -> u16 {
    1
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct PortForward {
    #[ts(type = "string")]
    pub src: SocketAddrV4,
    #[ts(type = "string")]
    pub dst: SocketAddrV4,
    pub gateway: GatewayId,
    /// Number of contiguous ports covered by this forward (always >= 1).
    /// Set to >1 by [`crate::net::host::binding::RangeBindInfo`] entries to
    /// represent a single iptables port-range rule rather than N per-port
    /// rules. Defaults to 1 to keep the on-disk shape compatible with
    /// existing single-port `PortForward`s.
    #[serde(default = "default_port_forward_count")]
    pub count: u16,
}

impl AsRef<Host> for Host {
    fn as_ref(&self) -> &Host {
        self
    }
}
impl Host {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn addresses<'a>(&'a self) -> impl Iterator<Item = HostAddress> + 'a {
        self.public_domains
            .iter()
            .map(|(address, config)| HostAddress {
                address: address.clone(),
                public: Some(config.clone()),
                private: self.private_domains.get(address).cloned(),
            })
            .chain(
                self.private_domains
                    .iter()
                    .filter(|(domain, _)| !self.public_domains.contains_key(*domain))
                    .map(|(domain, gateways)| HostAddress {
                        address: domain.clone(),
                        public: None,
                        private: Some(gateways.clone()),
                    }),
            )
    }
}
impl Model<Host> {
    pub fn update_addresses(
        &mut self,
        mdns: &ServerHostname,
        gateways: &OrdMap<GatewayId, NetworkInterfaceInfo>,
        available_ports: &AvailablePorts,
    ) -> Result<(), Error> {
        let this = self.destructure_mut();

        // ips
        for (_, bind) in this.bindings.as_entries_mut()? {
            let net = bind.as_net().de()?;
            let opt = bind.as_options().de()?;

            // Preserve existing plugin-provided addresses across recomputation
            let mut available = bind.as_addresses().as_available().de()?;
            available.retain(|h| matches!(h.metadata, HostnameMetadata::Plugin { .. }));
            for (gid, g) in gateways {
                let Some(ip_info) = &g.ip_info else {
                    continue;
                };
                let gateway_secure = g.secure();
                for subnet in &ip_info.subnets {
                    let host = InternedString::from_display(&subnet.addr());
                    let metadata = if subnet.addr().is_ipv4() {
                        HostnameMetadata::Ipv4 {
                            gateway: gid.clone(),
                        }
                    } else {
                        HostnameMetadata::Ipv6 {
                            gateway: gid.clone(),
                            scope_id: ip_info.scope_id,
                        }
                    };
                    if let Some(port) = net.assigned_port.filter(|_| {
                        opt.secure
                            .map_or(gateway_secure, |s| !(s.ssl && opt.add_ssl.is_some()))
                    }) {
                        available.insert(HostnameInfo {
                            ssl: opt.secure.map_or(false, |s| s.ssl),
                            public: false,
                            hostname: host.clone(),
                            port: Some(port),
                            metadata: metadata.clone(),
                        });
                    }
                    if let Some(port) = net.assigned_ssl_port {
                        available.insert(HostnameInfo {
                            ssl: true,
                            public: false,
                            hostname: host.clone(),
                            port: Some(port),
                            metadata,
                        });
                    }
                }
                if let Some(wan_ip) = &ip_info.wan_ip {
                    let host = InternedString::from_display(&wan_ip);
                    let metadata = HostnameMetadata::Ipv4 {
                        gateway: gid.clone(),
                    };
                    if let Some(port) = net.assigned_port.filter(|_| {
                        opt.secure.map_or(
                            false, // the public internet is never secure
                            |s| !(s.ssl && opt.add_ssl.is_some()),
                        )
                    }) {
                        available.insert(HostnameInfo {
                            ssl: opt.secure.map_or(false, |s| s.ssl),
                            public: true,
                            hostname: host.clone(),
                            port: Some(port),
                            metadata: metadata.clone(),
                        });
                    }
                    if let Some(port) = net.assigned_ssl_port {
                        available.insert(HostnameInfo {
                            ssl: true,
                            public: true,
                            hostname: host.clone(),
                            port: Some(port),
                            metadata,
                        });
                    }
                }
            }

            // mdns
            let mdns_host = mdns.local_domain_name();
            let mdns_gateways: BTreeSet<GatewayId> = gateways
                .iter()
                .filter(|(_, g)| {
                    matches!(
                        g.ip_info.as_ref().and_then(|i| i.device_type),
                        Some(NetworkInterfaceType::Ethernet | NetworkInterfaceType::Wireless)
                    )
                })
                .map(|(id, _)| id.clone())
                .collect();
            if let Some(port) = net.assigned_port.filter(|_| {
                opt.secure
                    .map_or(true, |s| !(s.ssl && opt.add_ssl.is_some()))
            }) {
                let mdns_gateways = if opt.secure.is_some() {
                    mdns_gateways.clone()
                } else {
                    mdns_gateways
                        .iter()
                        .filter(|g| gateways.get(*g).map_or(false, |g| g.secure()))
                        .cloned()
                        .collect()
                };
                if !mdns_gateways.is_empty() {
                    available.insert(HostnameInfo {
                        ssl: opt.secure.map_or(false, |s| s.ssl),
                        public: false,
                        hostname: mdns_host.clone(),
                        port: Some(port),
                        metadata: HostnameMetadata::Mdns {
                            gateways: mdns_gateways,
                        },
                    });
                }
            }
            if let Some(port) = net.assigned_ssl_port {
                available.insert(HostnameInfo {
                    ssl: true,
                    public: false,
                    hostname: mdns_host,
                    port: Some(port),
                    metadata: HostnameMetadata::Mdns {
                        gateways: mdns_gateways,
                    },
                });
            }

            // public domains
            for (domain, info) in this.public_domains.de()? {
                let metadata = HostnameMetadata::PublicDomain {
                    gateway: info.gateway.clone(),
                };
                if let Some(port) = net.assigned_port.filter(|_| {
                    opt.secure.map_or(
                        false, // the public internet is never secure
                        |s| !(s.ssl && opt.add_ssl.is_some()),
                    )
                }) {
                    available.insert(HostnameInfo {
                        ssl: opt.secure.map_or(false, |s| s.ssl),
                        public: true,
                        hostname: domain.clone(),
                        port: Some(port),
                        metadata: metadata.clone(),
                    });
                }
                if let Some(mut port) = net.assigned_ssl_port {
                    if let Some(preferred) = opt
                        .add_ssl
                        .as_ref()
                        .map(|s| s.preferred_external_port)
                        .filter(|p| available_ports.is_ssl(*p))
                    {
                        port = preferred;
                    }
                    available.insert(HostnameInfo {
                        ssl: true,
                        public: true,
                        hostname: domain,
                        port: Some(port),
                        metadata,
                    });
                } else if opt.secure.map_or(false, |s| s.ssl)
                    && opt.add_ssl.is_none()
                    && available_ports.is_ssl(opt.preferred_external_port)
                    && net.assigned_port != Some(opt.preferred_external_port)
                {
                    // Service handles its own TLS and the preferred port is
                    // allocated as SSL — add an address for passthrough vhost.
                    available.insert(HostnameInfo {
                        ssl: true,
                        public: true,
                        hostname: domain,
                        port: Some(opt.preferred_external_port),
                        metadata,
                    });
                }
            }

            // private domains
            for (domain, domain_gateways) in this.private_domains.de()? {
                if let Some(port) = net.assigned_port.filter(|_| {
                    opt.secure
                        .map_or(true, |s| !(s.ssl && opt.add_ssl.is_some()))
                }) {
                    let gateways = if opt.secure.is_some() {
                        domain_gateways.clone()
                    } else {
                        domain_gateways
                            .iter()
                            .cloned()
                            .filter(|g| gateways.get(g).map_or(false, |g| g.secure()))
                            .collect()
                    };
                    available.insert(HostnameInfo {
                        ssl: opt.secure.map_or(false, |s| s.ssl),
                        public: false,
                        hostname: domain.clone(),
                        port: Some(port),
                        metadata: HostnameMetadata::PrivateDomain { gateways },
                    });
                }
                if let Some(mut port) = net.assigned_ssl_port {
                    if let Some(preferred) = opt
                        .add_ssl
                        .as_ref()
                        .map(|s| s.preferred_external_port)
                        .filter(|p| available_ports.is_ssl(*p))
                    {
                        port = preferred;
                    }
                    available.insert(HostnameInfo {
                        ssl: true,
                        public: false,
                        hostname: domain,
                        port: Some(port),
                        metadata: HostnameMetadata::PrivateDomain {
                            gateways: domain_gateways,
                        },
                    });
                } else if opt.secure.map_or(false, |s| s.ssl)
                    && opt.add_ssl.is_none()
                    && available_ports.is_ssl(opt.preferred_external_port)
                    && net.assigned_port != Some(opt.preferred_external_port)
                {
                    available.insert(HostnameInfo {
                        ssl: true,
                        public: false,
                        hostname: domain,
                        port: Some(opt.preferred_external_port),
                        metadata: HostnameMetadata::PrivateDomain {
                            gateways: domain_gateways,
                        },
                    });
                }
            }
            bind.as_addresses_mut().as_available_mut().ser(&available)?;
        }

        // compute port forwards from available public addresses
        let bindings: Bindings = this.bindings.de()?;
        let mut port_forwards = BTreeSet::new();
        for bind in bindings.values() {
            for addr in bind.addresses.enabled() {
                if !addr.public {
                    continue;
                }
                let Some(port) = addr.port else {
                    continue;
                };
                let gw_id = match &addr.metadata {
                    HostnameMetadata::Ipv4 { gateway }
                    | HostnameMetadata::PublicDomain { gateway } => gateway,
                    _ => continue,
                };
                let Some(gw_info) = gateways.get(gw_id) else {
                    continue;
                };
                let Some(ip_info) = &gw_info.ip_info else {
                    continue;
                };
                let Some(wan_ip) = ip_info.wan_ip else {
                    continue;
                };
                for subnet in &ip_info.subnets {
                    let IpAddr::V4(addr) = subnet.addr() else {
                        continue;
                    };
                    port_forwards.insert(PortForward {
                        src: SocketAddrV4::new(wan_ip, port),
                        dst: SocketAddrV4::new(addr, port),
                        gateway: gw_id.clone(),
                        count: 1,
                    });
                }
            }
        }

        // Port-range bindings: forward each range to every IPv4 gateway with
        // a WAN IP. Ranges have no derived addresses (no HTTP-style routing),
        // so they get forwarded unconditionally for every gateway that can
        // route to them.
        let binding_ranges: BindingRanges = this.binding_ranges.de()?;
        for (&internal_start, range) in binding_ranges.iter() {
            if !range.enabled {
                continue;
            }
            for (gw_id, gw_info) in gateways {
                let Some(ip_info) = &gw_info.ip_info else {
                    continue;
                };
                let Some(wan_ip) = ip_info.wan_ip else {
                    continue;
                };
                for subnet in &ip_info.subnets {
                    let IpAddr::V4(lan_ip) = subnet.addr() else {
                        continue;
                    };
                    port_forwards.insert(PortForward {
                        src: SocketAddrV4::new(wan_ip, range.external_start_port),
                        dst: SocketAddrV4::new(lan_ip, internal_start),
                        gateway: gw_id.clone(),
                        count: range.number_of_ports,
                    });
                }
            }
        }

        this.port_forwards.ser(&port_forwards)?;

        Ok(())
    }
}

#[derive(Debug, Default, Deserialize, Serialize, HasModel, TS)]
#[model = "Model<Self>"]
#[ts(export)]
pub struct Hosts(pub BTreeMap<HostId, Host>);

impl Map for Hosts {
    type Key = HostId;
    type Value = Host;
    fn key_str(key: &Self::Key) -> Result<impl AsRef<str>, Error> {
        Ok(key)
    }
    fn key_string(key: &Self::Key) -> Result<InternedString, Error> {
        Ok(key.clone().into())
    }
}

pub fn host_for<'a>(
    db: &'a mut DatabaseModel,
    package_id: Option<&PackageId>,
    host_id: &HostId,
) -> Result<&'a mut Model<Host>, Error> {
    let Some(package_id) = package_id else {
        return Ok(db
            .as_public_mut()
            .as_server_info_mut()
            .as_network_mut()
            .as_host_mut());
    };
    fn host_info<'a>(
        db: &'a mut DatabaseModel,
        package_id: &PackageId,
    ) -> Result<&'a mut Model<Hosts>, Error> {
        Ok::<_, Error>(
            db.as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(package_id)
                .or_not_found(package_id)?
                .as_hosts_mut(),
        )
    }
    host_info(db, package_id)?.upsert(host_id, || Ok(Host::new()))
}

pub fn all_hosts(db: &mut DatabaseModel) -> impl Iterator<Item = Result<&mut Model<Host>, Error>> {
    use patch_db::DestructureMut;
    let destructured = db.as_public_mut().destructure_mut();
    [Ok(destructured.server_info.as_network_mut().as_host_mut())]
        .into_iter()
        .chain(
            [destructured.package_data.as_entries_mut()]
                .into_iter()
                .flatten_ok()
                .map(|entry| entry.and_then(|(_, v)| v.as_hosts_mut().as_entries_mut()))
                .flatten_ok()
                .map_ok(|(_, v)| v),
        )
}

impl Model<Host> {
    pub fn add_binding(
        &mut self,
        available_ports: &mut AvailablePorts,
        internal_port: u16,
        options: BindOptions,
    ) -> Result<(), Error> {
        self.as_bindings_mut().mutate(|b| {
            let info = if let Some(info) = b.remove(&internal_port) {
                info.update(available_ports, options)?
            } else {
                BindInfo::new(available_ports, options)?
            };
            b.insert(internal_port, info);
            Ok(())
        })
    }

    /// Add or re-affirm a contiguous port-range binding. Range bindings
    /// require `internal_start_port == external_start_port` (the iptables
    /// DNAT preserves the port number across the range) and must allocate
    /// every external port in the range up-front — partial collisions are a
    /// hard error.
    pub fn add_binding_range(
        &mut self,
        available_ports: &mut AvailablePorts,
        internal_start_port: u16,
        external_start_port: u16,
        number_of_ports: u16,
    ) -> Result<(), Error> {
        if number_of_ports == 0 {
            return Err(Error::new(
                eyre!("numberOfPorts must be at least 1"),
                ErrorKind::InvalidRequest,
            ));
        }
        if internal_start_port != external_start_port {
            return Err(Error::new(
                eyre!(
                    "bindPortRange requires internalStartPort == externalStartPort \
                     (got internal={internal_start_port} external={external_start_port})"
                ),
                ErrorKind::InvalidRequest,
            ));
        }
        self.as_binding_ranges_mut().mutate(|ranges| {
            match ranges.get(&internal_start_port) {
                // Idempotent re-bind: a package may call bindPortRange on every
                // setupMain pass. An unchanged range keeps its allocation; the
                // insert below just re-affirms (re-enables) it.
                Some(existing)
                    if existing.external_start_port == external_start_port
                        && existing.number_of_ports == number_of_ports => {}
                // New, resized, or moved range: free any prior allocation, then
                // claim the new one. The free uses an inclusive end —
                // `start + count` would overflow u16 for a range ending at 65535.
                maybe_existing => {
                    if let Some(existing) = maybe_existing {
                        available_ports.free(
                            existing.external_start_port
                                ..=existing.external_start_port + existing.number_of_ports - 1,
                        );
                    }
                    available_ports.try_alloc_range(external_start_port, number_of_ports)?;
                }
            }
            ranges.insert(
                internal_start_port,
                RangeBindInfo {
                    enabled: true,
                    external_start_port,
                    number_of_ports,
                },
            );
            Ok(())
        })
    }
}

#[derive(Deserialize, Serialize, Parser)]
#[group(skip)]
pub struct RequiresPackageId {
    #[arg(help = "help.arg.package-id")]
    package: PackageId,
}

#[derive(Deserialize, Serialize, Parser)]
#[group(skip)]
pub struct RequiresHostId {
    #[arg(help = "help.arg.host-id")]
    host: HostId,
}

pub trait HostApiKind: 'static {
    type Params: Send + Sync + 'static;
    type InheritedParams: Send + Sync + 'static;
    type Inheritance: RefUnwindSafe + OrEmpty<Self::Inheritance> + Send + Sync + 'static;
    fn inheritance(params: Self::Params, inherited: Self::InheritedParams) -> Self::Inheritance;
    fn host_for<'a>(
        inheritance: &Self::Inheritance,
        db: &'a mut DatabaseModel,
    ) -> Result<&'a mut Model<Host>, Error>;
}
pub struct ForPackage;
impl HostApiKind for ForPackage {
    type Params = RequiresHostId;
    type InheritedParams = PackageId;
    type Inheritance = (PackageId, HostId);
    fn inheritance(
        RequiresHostId { host }: Self::Params,
        package: Self::InheritedParams,
    ) -> Self::Inheritance {
        (package, host)
    }
    fn host_for<'a>(
        (package, host): &Self::Inheritance,
        db: &'a mut DatabaseModel,
    ) -> Result<&'a mut Model<Host>, Error> {
        host_for(db, Some(package), host)
    }
}
pub struct ForServer;
impl HostApiKind for ForServer {
    type Params = Empty;
    type InheritedParams = Empty;
    type Inheritance = Empty;
    fn inheritance(_: Self::Params, _: Self::InheritedParams) -> Self::Inheritance {
        Empty {}
    }
    fn host_for<'a>(
        _: &Self::Inheritance,
        db: &'a mut DatabaseModel,
    ) -> Result<&'a mut Model<Host>, Error> {
        host_for(db, None, &HostId::default())
    }
}

pub fn host_api<C: Context>() -> ParentHandler<C, RequiresPackageId> {
    ParentHandler::<C, RequiresPackageId>::new()
        .subcommand(
            "list",
            from_fn_async(list_hosts)
                .with_inherited(|RequiresPackageId { package }, _| package)
                .with_custom_display_fn(|_, ids| {
                    for id in ids {
                        println!("{id}")
                    }
                    Ok(())
                })
                .with_about("about.list-host-ids-for-service"),
        )
        .subcommand(
            "address",
            address_api::<C, ForPackage>()
                .with_inherited(|RequiresPackageId { package }, _| package)
                .with_about("about.commands-host-addresses"),
        )
        .subcommand(
            "binding",
            binding::<C, ForPackage>()
                .with_inherited(|RequiresPackageId { package }, _| package)
                .with_about("about.commands-host-bindings"),
        )
}

pub fn server_host_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::<C>::new()
        .subcommand(
            "address",
            address_api::<C, ForServer>().with_about("about.commands-host-addresses"),
        )
        .subcommand(
            "binding",
            binding::<C, ForServer>().with_about("about.commands-host-bindings"),
        )
}

pub async fn list_hosts(
    ctx: RpcContext,
    _: Empty,
    package: PackageId,
) -> Result<BTreeSet<HostId>, Error> {
    ctx.db
        .peek()
        .await
        .into_public()
        .into_package_data()
        .into_idx(&package)
        .or_not_found(&package)?
        .into_hosts()
        .keys()
}
