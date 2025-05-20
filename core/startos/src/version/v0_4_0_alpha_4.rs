use exver::{PreReleaseSegment, VersionRange};

use super::v0_3_5::V0_3_0_COMPAT;
use super::{v0_4_0_alpha_3, VersionT};
use crate::context::RpcContext;
use crate::prelude::*;
use crate::util::io::create_file_mod;

lazy_static::lazy_static! {
    static ref V0_4_0_alpha_4: exver::Version = exver::Version::new(
        [0, 4, 0],
        [PreReleaseSegment::String("alpha".into()), 4.into()]
    );
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Version;

impl VersionT for Version {
    type Previous = v0_4_0_alpha_3::Version;
    type PreUpRes = ();

    async fn pre_up(self) -> Result<Self::PreUpRes, Error> {
        Ok(())
    }
    fn semver(self) -> exver::Version {
        V0_4_0_alpha_4.clone()
    }
    fn compat(self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    fn up(self, db: &mut Value, _: Self::PreUpRes) -> Result<(), Error> {
        db["public"]["serverInfo"]
            .as_object_mut()
            .or_not_found("public.serverInfo")?
            .insert("kiosk".into(), Value::Bool(true));
        Ok(())
    }
    async fn post_up(self, _ctx: &RpcContext) -> Result<(), Error> {
        use tokio::io::AsyncWriteExt;

        if tokio::fs::metadata("/media/startos/config/overlay/etc/shadow")
            .await
            .is_ok()
        {
            let mut hash = None;
            let shadow_contents = tokio::fs::read_to_string("/etc/shadow").await?;
            let mut shadow_file =
                create_file_mod("/media/startos/config/overlay/etc/shadow", 0o640).await?;
            for line in shadow_contents.lines() {
                match line.split_once(":") {
                    Some((user, rest)) if user == "start9" || user == "kiosk" => {
                        let (h, rest) = rest.split_once(":").ok_or_else(|| {
                            Error::new(eyre!("malformed /etc/shadow"), ErrorKind::ParseSysInfo)
                        })?;
                        if user == "start9" {
                            hash = Some(h.to_owned());
                        }
                        let h = hash.as_deref().unwrap_or(h);
                        shadow_file
                            .write_all(format!("{user}:{h}:{rest}\n").as_bytes())
                            .await?;
                    }
                    _ => {
                        shadow_file.write_all(line.as_bytes()).await?;
                        shadow_file.write_all(b"\n").await?;
                    }
                }
            }
            shadow_file.sync_all().await?;
            tokio::fs::copy("/media/startos/config/overlay/etc/shadow", "/etc/shadow").await?;
        }
        Ok(())
    }
    fn down(self, _db: &mut Value) -> Result<(), Error> {
        Ok(())
    }
}
