use exver::{PreReleaseSegment, VersionRange};

use super::v0_3_5::V0_3_0_COMPAT;
use super::{v0_4_0_alpha_5, VersionT};
use crate::prelude::*;

lazy_static::lazy_static! {
    static ref V0_4_0_alpha_6: exver::Version = exver::Version::new(
        [0, 4, 0],
        [PreReleaseSegment::String("alpha".into()), 6.into()]
    );
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Version;

impl VersionT for Version {
    type Previous = v0_4_0_alpha_5::Version;
    type PreUpRes = ();

    async fn pre_up(self) -> Result<Self::PreUpRes, Error> {
        Ok(())
    }
    fn semver(self) -> exver::Version {
        V0_4_0_alpha_6.clone()
    }
    fn compat(self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    #[instrument]
    fn up(self, db: &mut Value, _: Self::PreUpRes) -> Result<Value, Error> {
        let ui = db["public"]["ui"]
            .as_object_mut()
            .or_not_found("public.ui")?;
        if ui.get("ackInstructions").is_none() {
            if let Some(old) = ui.remove("ack-instructions") {
                ui.insert("ackInstructions".into(), old);
            }
        }
        ui.remove("ackWelcome");
        Ok(Value::Null)
    }
    fn down(self, _db: &mut Value) -> Result<(), Error> {
        Ok(())
    }
}
