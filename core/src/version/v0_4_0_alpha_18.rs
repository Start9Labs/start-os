use std::sync::Arc;

use exver::{PreReleaseSegment, VersionRange};

use super::v0_3_5::V0_3_0_COMPAT;
use super::{VersionT, v0_4_0_alpha_17};
use crate::context::RpcContext;
use crate::prelude::*;

lazy_static::lazy_static! {
    static ref V0_4_0_alpha_18: exver::Version = exver::Version::new(
        [0, 4, 0],
        [PreReleaseSegment::String("alpha".into()), 18.into()]
    );
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Version;

impl VersionT for Version {
    type Previous = v0_4_0_alpha_17::Version;
    type PreUpRes = ();

    async fn pre_up(self) -> Result<Self::PreUpRes, Error> {
        Ok(())
    }
    fn semver(self) -> exver::Version {
        V0_4_0_alpha_18.clone()
    }
    fn compat(self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    #[instrument(skip_all)]
    fn up(self, db: &mut Value, _: Self::PreUpRes) -> Result<Value, Error> {
        let lang = db["public"]["ui"]
            .as_object_mut()
            .map_or(Value::Null, |m| m.remove("language").unwrap_or_default());
        if let Some(lang) = lang.as_str() {
            let lang = match lang {
                "en" => "en_US",
                "de" => "de_DE",
                "es" => "es_ES",
                "fr" => "fr_FR",
                "pl" => "pl_PL",
                _ => return Ok(Value::Null),
            };

            let lang = Value::String(Arc::new(lang.into()));

            db["public"]["serverInfo"]["language"] = lang.clone();

            return Ok(lang);
        }
        Ok(Value::Null)
    }
    async fn post_up(self, _: &RpcContext, input: Value) -> Result<(), Error> {
        if let Some(language) = input.as_str() {
            crate::system::save_language(language).await?;
        }
        Ok(())
    }
    fn down(self, _db: &mut Value) -> Result<(), Error> {
        Ok(())
    }
}
