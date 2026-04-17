use exver::{PreReleaseSegment, VersionRange};
use imbl_value::json;

use super::v0_3_5::V0_3_0_COMPAT;
use super::{VersionT, v0_4_0_beta_5};
use crate::prelude::*;

lazy_static::lazy_static! {
    static ref V0_4_0_beta_6: exver::Version = exver::Version::new(
        [0, 4, 0],
        [PreReleaseSegment::String("beta".into()), 6.into()]
    );
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Version;

impl VersionT for Version {
    type Previous = v0_4_0_beta_5::Version;
    type PreUpRes = ();

    async fn pre_up(self) -> Result<Self::PreUpRes, Error> {
        Ok(())
    }
    fn semver(self) -> exver::Version {
        V0_4_0_beta_6.clone()
    }
    fn compat(self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    #[instrument(skip_all)]
    fn up(self, db: &mut Value, _: Self::PreUpRes) -> Result<Value, Error> {
        let Some(ui) = db["public"]["ui"].as_object_mut() else {
            return Err(Error::new(
                eyre!("db.public.ui is not an object"),
                ErrorKind::Database,
            ));
        };
        if !ui.contains_key("hiddenUpdates") {
            ui.insert("hiddenUpdates".into(), json!({}));
        }
        Ok(Value::Null)
    }
    fn down(self, _db: &mut Value) -> Result<(), Error> {
        Ok(())
    }
}
