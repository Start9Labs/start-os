use exver::{PreReleaseSegment, VersionRange};

use super::v0_3_5::V0_3_0_COMPAT;
use super::{v0_3_6_alpha_5, VersionT};
use crate::notifications::{notify, NotificationLevel};
use crate::prelude::*;

lazy_static::lazy_static! {
    static ref V0_3_6_alpha_6: exver::Version = exver::Version::new(
        [0, 3, 6],
        [PreReleaseSegment::String("alpha".into()), 6.into()]
    );
}

#[derive(Default, Clone, Copy, Debug)]
pub struct Version;

impl VersionT for Version {
    type Previous = v0_3_6_alpha_5::Version;
    type PreUpRes = ();
    fn semver(self) -> exver::Version {
        V0_3_6_alpha_6.clone()
    }
    fn compat(self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    async fn pre_up(self) -> Result<Self::PreUpRes, Error> {
        Ok(())
    }
    fn up(self, db: &mut Value, _: Self::PreUpRes) -> Result<(), Error> {
        Ok(())
    }
    async fn post_up<'a>(self, ctx: &'a crate::context::RpcContext) -> Result<(), Error> {
        let message_update = include_str!("update_details/v0_3_6.md").to_string();

        ctx.db
            .mutate(|db| {
                notify(
                    db,
                    None,
                    NotificationLevel::Success,
                    "Welcome to StartOS 0.3.6!".to_string(),
                    "Click \"View Details\" to learn all about the new version".to_string(),
                    message_update,
                )?;
                Ok(())
            })
            .await?;
        Ok(())
    }
    fn down(self, _db: &mut Value) -> Result<(), Error> {
        Ok(())
    }
}
