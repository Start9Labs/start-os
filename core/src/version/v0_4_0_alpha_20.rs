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

        // Remove onion entries from hostnameInfo in server host
        remove_onion_hostname_info(
            db.get_mut("public")
                .and_then(|p| p.get_mut("serverInfo"))
                .and_then(|s| s.get_mut("network"))
                .and_then(|n| n.get_mut("host")),
        );

        // Remove onion entries from hostnameInfo in all package hosts
        if let Some(packages) = db
            .get_mut("public")
            .and_then(|p| p.get_mut("packageData"))
            .and_then(|p| p.as_object_mut())
        {
            for (_, package) in packages.iter_mut() {
                if let Some(hosts) = package.get_mut("hosts").and_then(|h| h.as_object_mut()) {
                    for (_, host) in hosts.iter_mut() {
                        remove_onion_hostname_info(Some(host));
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

        Ok(Value::Null)
    }
    fn down(self, _db: &mut Value) -> Result<(), Error> {
        Ok(())
    }
}

fn remove_onion_hostname_info(host: Option<&mut Value>) {
    if let Some(hostname_info) = host
        .and_then(|h| h.get_mut("hostnameInfo"))
        .and_then(|h| h.as_object_mut())
    {
        for (_, infos) in hostname_info.iter_mut() {
            if let Some(arr) = infos.as_array_mut() {
                arr.retain(|info| info.get("kind").and_then(|k| k.as_str()) != Some("onion"));
            }
        }
    }
}
