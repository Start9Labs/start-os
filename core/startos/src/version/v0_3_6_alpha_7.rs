use exver::{PreReleaseSegment, VersionRange};
use imbl_value::json;

use super::v0_3_5::V0_3_0_COMPAT;
use super::{v0_3_6_alpha_6, VersionT};
use crate::prelude::*;

lazy_static::lazy_static! {
    static ref V0_3_6_alpha_7: exver::Version = exver::Version::new(
        [0, 3, 6],
        [PreReleaseSegment::String("alpha".into()), 7.into()]
    );
}

#[derive(Clone, Debug)]
pub struct Version;

impl VersionT for Version {
    type Previous = v0_3_6_alpha_6::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> exver::Version {
        V0_3_6_alpha_7.clone()
    }
    fn compat(&self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    fn up(&self, db: &mut Value) -> Result<(), Error> {
        let wifi = json!({
            "infterface": db["server-info"]["wifi"]["interface"],
            "ssids": db["server-info"]["wifi"]["ssids"],
            "selected": db["server-info"]["wifi"]["selected"],
            "last_region": db["server-info"]["wifi"]["last-region"],
        });

        let ip_info = {
            let mut ip_info = json!({});
            let empty = Default::default();
            for (k, v) in db["server-info"]["ip-info"].as_object().unwrap_or(&empty) {
                let k: &str = k.as_ref();
                ip_info[k] = json!({
                    "ipv4Range": v["ipv4-range"],
                    "ipv6Range": v["ipv6-range"],
                    "ipv4": v["ipv4"],
                    "ipv6": v["ipv6"],
                });
            }
            ip_info
        };

        let status_info = json!({
            "backupProgress": db["server-info"]["status-info"]["backup-progress"],
            "updated": db["server-info"]["status-info"]["updated"],
            "updateProgress": db["server-info"]["status-info"]["update-progress"],
            "shuttingDown": db["server-info"]["status-info"]["shutting-down"],
            "restarting": db["server-info"]["status-info"]["restarting"],
        });
        let server_info = {
            let mut server_info = json!({
                "arch": db["server-info"]["arch"],
                "platform": db["server-info"]["platform"],
                "id": db["server-info"]["id"],
                "hostname": db["server-info"]["hostname"],
                "version": db["server-info"]["version"],
                "lastBackup": db["server-info"]["last-backup"],
                "eosVersionCompat": db["server-info"]["eos-version-compat"],
                "lanAddress": db["server-info"]["lan-address"],
            });

            // Maybe we do this like the Public::init does
            server_info["onionAddress"] = db["server-info"]["onion-address"].clone();
            server_info["torAddress"] = db["server-info"]["tor-address"].clone();
            server_info["ipInfo"] = ip_info;
            server_info["statusInfo"] = status_info;
            server_info["wifi"] = wifi;
            server_info["unreadNotificationCount"] =
                db["server-info"]["unread-notification-count"].clone();
            server_info["passwordHash"] = db["server-info"]["password-hash"].clone();
            server_info["pubkey"] = db["server-info"]["pubkey"].clone();
            server_info["caFingerprint"] = db["server-info"]["ca-fingerprint"].clone();
            server_info["ntpSynced"] = db["server-info"]["ntp-synced"].clone();
            server_info["zram"] = db["server-info"]["zram"].clone();
            server_info["governor"] = db["server-info"]["governor"].clone();
            // This one should always be empty, doesn't exist in the previous. And the smtp is all single word key
            server_info["smtp"] = db["server-info"]["smtp"].clone();
            server_info
        };

        let public = json!({
            "serverInfo": server_info,
            // TODO We do something with the package data for the migration, like reinstall them?
            "packageData": json!({}),
            "ui": db["ui"],
        });

        // TODO Need to figure out how to extract the old private data
        let private = json!({});
        let next = json!({
            "public": public,
            "private": private,
        });

        *db = next;

        Ok(())
    }
    fn down(&self, _db: &mut Value) -> Result<(), Error> {
        Ok(())
    }
}
