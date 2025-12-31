use exver::{PreReleaseSegment, VersionRange};

use super::v0_3_5::V0_3_0_COMPAT;
use super::{VersionT, v0_4_0_alpha_16};
use crate::db::model::public::AcmeSettings;
use crate::net::acme::AcmeProvider;
use crate::prelude::*;

lazy_static::lazy_static! {
    static ref V0_4_0_alpha_17: exver::Version = exver::Version::new(
        [0, 4, 0],
        [PreReleaseSegment::String("alpha".into()), 17.into()]
    );
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Version;

impl VersionT for Version {
    type Previous = v0_4_0_alpha_16::Version;
    type PreUpRes = ();

    async fn pre_up(self) -> Result<Self::PreUpRes, Error> {
        Ok(())
    }
    fn semver(self) -> exver::Version {
        V0_4_0_alpha_17.clone()
    }
    fn compat(self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    #[instrument(skip_all)]
    fn up(self, db: &mut Value, _: Self::PreUpRes) -> Result<Value, Error> {
        let acme = db["public"]["serverInfo"]["network"]["acme"]
            .as_object_mut()
            .or_not_found("public.serverInfo.network.acme")?;
        let letsencrypt =
            InternedString::intern::<&str>("letsencrypt".parse::<AcmeProvider>()?.as_ref());
        if !acme.contains_key(&letsencrypt) {
            acme.insert(
                letsencrypt,
                to_value(&AcmeSettings {
                    contact: Vec::new(),
                })?,
            );
        }

        Ok(Value::Null)
    }
    fn down(self, _db: &mut Value) -> Result<(), Error> {
        Ok(())
    }
}
