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

        Ok(Value::Null)
    }
    fn down(self, _db: &mut Value) -> Result<(), Error> {
        Ok(())
    }
}

fn migrate_host(host: Option<&mut Value>) {
    let Some(host) = host.and_then(|h| h.as_object_mut()) else {
        return;
    };

    // Remove hostnameInfo from host
    host.remove("hostnameInfo");

    // For each binding: add "addresses" field, remove gateway-level fields from "net"
    if let Some(bindings) = host.get_mut("bindings").and_then(|b| b.as_object_mut()) {
        for (_, binding) in bindings.iter_mut() {
            if let Some(binding_obj) = binding.as_object_mut() {
                // Add addresses if not present
                if !binding_obj.contains_key("addresses") {
                    binding_obj.insert(
                        "addresses".into(),
                        serde_json::json!({
                            "privateDisabled": [],
                            "publicEnabled": [],
                            "possible": []
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
