use async_trait::async_trait;
use emver::VersionRange;
use models::ResultExt;
use sqlx::PgPool;

use super::v0_3_4::V0_3_0_COMPAT;
use super::{v0_3_4_3, VersionT};
use crate::prelude::*;

const V0_3_4_4: emver::Version = emver::Version::new(0, 3, 4, 4);

#[derive(Clone, Debug)]
pub struct Version;

#[async_trait]
impl VersionT for Version {
    type Previous = v0_3_4_3::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> emver::Version {
        V0_3_4_4
    }
    fn compat(&self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    async fn up(&self, db: PatchDb, _secrets: &PgPool) -> Result<(), Error> {
        db.mutate(|v| {
            let tor_address_lens = v.as_server_info_mut().as_tor_address_mut();
            let mut tor_addr = tor_address_lens.de()?;
            tor_addr
                .set_scheme("https")
                .map_err(|_| eyre!("unable to update url scheme to https"))
                .with_kind(crate::ErrorKind::ParseUrl)?;
            tor_address_lens.ser(&tor_addr)
        })
        .await?;
        Ok(())
    }
    async fn down(&self, _db: PatchDb, _secrets: &PgPool) -> Result<(), Error> {
        Ok(())
    }
}
