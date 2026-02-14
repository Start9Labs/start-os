use exver::{PreReleaseSegment, VersionRange};

use super::v0_3_5::V0_3_0_COMPAT;
use super::{VersionT, v0_4_0_alpha_19};
use crate::prelude::*;

lazy_static::lazy_static! {
    static ref V0_4_0_alpha_20: exver::Version = exver::Version::new(
        [0, 4, 0],
        [PreReleaseSegment::String("alpha".into()), 20.into()]
    );
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Version;

impl VersionT for Version {
    type Previous = v0_4_0_alpha_19::Version;
    type PreUpRes = ();

    async fn pre_up(self) -> Result<Self::PreUpRes, Error> {
        Ok(())
    }
    fn semver(self) -> exver::Version {
        V0_4_0_alpha_20.clone()
    }
    fn compat(self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    #[instrument(skip_all)]
    fn up(self, db: &mut Value, _: Self::PreUpRes) -> Result<Value, Error> {
        // Remove onions and tor-related fields from server host
        if let Some(host) = db
            .get_mut("public")
            .and_then(|p| p.get_mut("serverInfo"))
            .and_then(|s| s.get_mut("network"))
            .and_then(|n| n.get_mut("host"))
            .and_then(|h| h.as_object_mut())
        {
            host.remove("onions");
        }

        // Remove onions from all package hosts
        if let Some(packages) = db
            .get_mut("public")
            .and_then(|p| p.get_mut("packageData"))
            .and_then(|p| p.as_object_mut())
        {
            for (_, package) in packages.iter_mut() {
                if let Some(hosts) = package.get_mut("hosts").and_then(|h| h.as_object_mut()) {
                    for (_, host) in hosts.iter_mut() {
                        if let Some(host_obj) = host.as_object_mut() {
                            host_obj.remove("onions");
                        }
                    }
                }
            }
        }

        // Remove onion store from private keyStore
        if let Some(key_store) = db
            .get_mut("private")
            .and_then(|p| p.get_mut("keyStore"))
            .and_then(|k| k.as_object_mut())
        {
            key_store.remove("onion");
        }

        // Migrate server host: remove hostnameInfo, add addresses to bindings, clean net
        migrate_host(
            db.get_mut("public")
                .and_then(|p| p.get_mut("serverInfo"))
                .and_then(|s| s.get_mut("network"))
                .and_then(|n| n.get_mut("host")),
        );

        // Migrate all package hosts
        if let Some(packages) = db
            .get_mut("public")
            .and_then(|p| p.get_mut("packageData"))
            .and_then(|p| p.as_object_mut())
        {
            for (_, package) in packages.iter_mut() {
                if let Some(hosts) = package.get_mut("hosts").and_then(|h| h.as_object_mut()) {
                    for (_, host) in hosts.iter_mut() {
                        migrate_host(Some(host));
                    }
                }
            }
        }

        // Migrate availablePorts from IdPool format to BTreeMap<u16, bool>
        // Rebuild from actual assigned ports in all bindings
        migrate_available_ports(db);

        Ok(Value::Null)
    }
    fn down(self, _db: &mut Value) -> Result<(), Error> {
        Ok(())
    }
}

fn collect_ports_from_host(host: Option<&Value>, ports: &mut Value) {
    let Some(bindings) = host
        .and_then(|h| h.get("bindings"))
        .and_then(|b| b.as_object())
    else {
        return;
    };
    for (_, binding) in bindings.iter() {
        if let Some(net) = binding.get("net") {
            if let Some(port) = net.get("assignedPort").and_then(|p| p.as_u64()) {
                if let Some(obj) = ports.as_object_mut() {
                    obj.insert(port.to_string().into(), Value::from(false));
                }
            }
            if let Some(port) = net.get("assignedSslPort").and_then(|p| p.as_u64()) {
                if let Some(obj) = ports.as_object_mut() {
                    obj.insert(port.to_string().into(), Value::from(true));
                }
            }
        }
    }
}

fn migrate_available_ports(db: &mut Value) {
    let mut new_ports: Value = serde_json::json!({}).into();

    // Collect from server host
    let server_host = db
        .get("public")
        .and_then(|p| p.get("serverInfo"))
        .and_then(|s| s.get("network"))
        .and_then(|n| n.get("host"))
        .cloned();
    collect_ports_from_host(server_host.as_ref(), &mut new_ports);

    // Collect from all package hosts
    if let Some(packages) = db
        .get("public")
        .and_then(|p| p.get("packageData"))
        .and_then(|p| p.as_object())
    {
        for (_, package) in packages.iter() {
            if let Some(hosts) = package.get("hosts").and_then(|h| h.as_object()) {
                for (_, host) in hosts.iter() {
                    collect_ports_from_host(Some(host), &mut new_ports);
                }
            }
        }
    }

    // Replace private.availablePorts
    if let Some(private) = db.get_mut("private").and_then(|p| p.as_object_mut()) {
        private.insert("availablePorts".into(), new_ports);
    }
}

fn migrate_host(host: Option<&mut Value>) {
    let Some(host) = host.and_then(|h| h.as_object_mut()) else {
        return;
    };

    // Remove hostnameInfo from host
    host.remove("hostnameInfo");

    // Migrate privateDomains from array to object (BTreeSet -> BTreeMap<_, BTreeSet<GatewayId>>)
    if let Some(private_domains) = host.get("privateDomains").and_then(|v| v.as_array()).cloned() {
        let mut new_pd: Value = serde_json::json!({}).into();
        for domain in private_domains {
            if let Some(d) = domain.as_str() {
                if let Some(obj) = new_pd.as_object_mut() {
                    obj.insert(d.into(), serde_json::json!([]).into());
                }
            }
        }
        host.insert("privateDomains".into(), new_pd);
    }

    // For each binding: add "addresses" field, remove gateway-level fields from "net"
    if let Some(bindings) = host.get_mut("bindings").and_then(|b| b.as_object_mut()) {
        for (_, binding) in bindings.iter_mut() {
            if let Some(binding_obj) = binding.as_object_mut() {
                // Add addresses if not present
                if !binding_obj.contains_key("addresses") {
                    binding_obj.insert(
                        "addresses".into(),
                        serde_json::json!({
                            "enabled": [],
                            "disabled": [],
                            "available": []
                        })
                        .into(),
                    );
                }

                // Remove gateway-level privateDisabled/publicEnabled from net
                if let Some(net) = binding_obj.get_mut("net").and_then(|n| n.as_object_mut()) {
                    net.remove("privateDisabled");
                    net.remove("publicEnabled");
                }
            }
        }
    }
}
