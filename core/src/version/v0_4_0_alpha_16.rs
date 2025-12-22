use exver::{PreReleaseSegment, VersionRange};

use super::v0_3_5::V0_3_0_COMPAT;
use super::{VersionT, v0_4_0_alpha_15};
use crate::prelude::*;
use crate::status::StatusInfo;

lazy_static::lazy_static! {
    static ref V0_4_0_alpha_16: exver::Version = exver::Version::new(
        [0, 4, 0],
        [PreReleaseSegment::String("alpha".into()), 16.into()]
    );
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Version;

impl VersionT for Version {
    type Previous = v0_4_0_alpha_15::Version;
    type PreUpRes = ();

    async fn pre_up(self) -> Result<Self::PreUpRes, Error> {
        Ok(())
    }
    fn semver(self) -> exver::Version {
        V0_4_0_alpha_16.clone()
    }
    fn compat(self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    #[instrument(skip_all)]
    fn up(self, db: &mut Value, _: Self::PreUpRes) -> Result<Value, Error> {
        for (_, pde) in db["public"]["packageData"]
            .as_object_mut()
            .into_iter()
            .flat_map(|x| x.iter_mut())
        {
            pde["statusInfo"] = to_value(&StatusInfo::default())?;
            if match pde["status"]["main"].as_str().unwrap_or_default() {
                "running" | "starting" | "restarting" => true,
                "backingUp"
                    if pde["status"]["main"]["onComplete"]
                        .as_bool()
                        .unwrap_or(false) =>
                {
                    true
                }
                "error"
                    if pde["status"]["main"]["onRebuild"]
                        .as_bool()
                        .unwrap_or(false) =>
                {
                    true
                }
                _ => false,
            } {
                pde["statusInfo"]["desired"]["main"] = to_value(&"running")?;
            }
        }

        Ok(Value::Null)
    }
    fn down(self, _db: &mut Value) -> Result<(), Error> {
        Ok(())
    }
}
