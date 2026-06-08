use exver::{PreReleaseSegment, VersionRange};
use imbl_value::json;

use super::v0_3_5::V0_3_0_COMPAT;
use super::{VersionT, v0_4_0_beta_9};
use crate::prelude::*;

lazy_static::lazy_static! {
    static ref V0_4_0_beta_10: exver::Version = exver::Version::new(
        [0, 4, 0],
        [PreReleaseSegment::String("beta".into()), 10.into()]
    );
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Version;

impl VersionT for Version {
    type Previous = v0_4_0_beta_9::Version;
    type PreUpRes = ();

    async fn pre_up(self) -> Result<Self::PreUpRes, Error> {
        Ok(())
    }
    fn semver(self) -> exver::Version {
        V0_4_0_beta_10.clone()
    }
    fn compat(self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    #[instrument(skip_all)]
    fn up(self, db: &mut Value, _: Self::PreUpRes) -> Result<Value, Error> {
        // The Root CA fingerprint was formatted with `{:X}` (no width), dropping
        // the leading zero of any byte < 0x10 (e.g. `A3:3:D2` instead of
        // `A3:03:D2`), so the value shown in the UI didn't match what devices
        // report. Each `{:X}` byte is 1 char (< 0x10) or 2 chars, so left-padding
        // every 1-char segment to two digits exactly reconstructs the `{:02X}`
        // form. Idempotent: already-correct (all-2-char) fingerprints are unchanged.
        let Some(server_info) = db["public"]["serverInfo"].as_object_mut() else {
            return Err(Error::new(
                eyre!("db.public.serverInfo is not an object"),
                ErrorKind::Database,
            ));
        };
        let Some(fingerprint) = server_info.get("caFingerprint").and_then(|v| v.as_str()) else {
            return Err(Error::new(
                eyre!("db.public.serverInfo.caFingerprint is not a string"),
                ErrorKind::Database,
            ));
        };
        let repaired = fingerprint
            .split(':')
            .map(|seg| {
                if seg.len() < 2 {
                    format!("0{seg}")
                } else {
                    seg.to_owned()
                }
            })
            .collect::<Vec<_>>()
            .join(":");
        server_info.insert("caFingerprint".into(), json!(repaired));
        Ok(Value::Null)
    }
    fn down(self, _db: &mut Value) -> Result<(), Error> {
        Ok(())
    }
}
