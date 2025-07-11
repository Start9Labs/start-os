use exver::{PreReleaseSegment, VersionRange};
use imbl_value::json;

use super::v0_3_5::V0_3_0_COMPAT;
use super::{v0_3_6_alpha_18, VersionT};
use crate::context::RpcContext;
use crate::notifications::{notify, NotificationLevel};
use crate::prelude::*;

lazy_static::lazy_static! {
    static ref V0_4_0_alpha_0: exver::Version = exver::Version::new(
        [0, 4, 0],
        [PreReleaseSegment::String("alpha".into()), 0.into()]
    );
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Version;

impl VersionT for Version {
    type Previous = v0_3_6_alpha_18::Version;
    type PreUpRes = ();

    async fn pre_up(self) -> Result<Self::PreUpRes, Error> {
        Ok(())
    }
    fn semver(self) -> exver::Version {
        V0_4_0_alpha_0.clone()
    }
    fn compat(self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    fn up(self, db: &mut Value, _: Self::PreUpRes) -> Result<Value, Error> {
        let host = db["public"]["serverInfo"]["host"].clone();
        let mut wifi = db["public"]["serverInfo"]["wifi"].clone();
        wifi["enabled"] = Value::Bool(!wifi["selected"].is_null());
        let mut network_interfaces = db["public"]["serverInfo"]["networkInterfaces"].clone();
        for (k, v) in network_interfaces
            .as_object_mut()
            .into_iter()
            .flat_map(|m| m.iter_mut())
        {
            v["inbound"] = v["public"].clone();
            if v["ipInfo"].is_object() {
                v["ipInfo"]["name"] = Value::String((&**k).to_owned().into());
            }
        }
        let acme = db["public"]["serverInfo"]["acme"].clone();
        db["public"]["serverInfo"]["network"] = json!({
            "host": host,
            "wifi": wifi,
            "networkInterfaces": network_interfaces,
            "acme": acme,
        });
        Ok(Value::Null)
    }
    async fn post_up(self, ctx: &RpcContext, _input: Value) -> Result<(), Error> {
        let message_update = include_str!("update_details/v0_4_0.md").to_string();

        ctx.db
            .mutate(|db| {
                notify(
                    db,
                    None,
                    NotificationLevel::Success,
                    "Welcome to StartOS 0.4.0!".to_string(),
                    "Click \"View Details\" to learn all about the new version".to_string(),
                    message_update,
                )?;
                Ok(())
            })
            .await
            .result?;
        Ok(())
    }
    fn down(self, _db: &mut Value) -> Result<(), Error> {
        Ok(())
    }
}
