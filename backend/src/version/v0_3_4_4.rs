use async_trait::async_trait;
use emver::VersionRange;
use models::ResultExt;

use super::v0_3_0::V0_3_0_COMPAT;
use super::*;

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
        &*V0_3_0_COMPAT
    }
    async fn up<Db: DbHandle>(&self, db: &mut Db, _secrets: &PgPool) -> Result<(), Error> {
        let mut tor_addr = db
            .mutate(|v| {
                let mut tor_address_lens = v.as_server_info_mut().as_tor_address_mut();
                let mut tor_addr = tor_address_lens.de();
                tor_addr
                    .set_scheme("https")
                    .map_err(|_| eyre!("unable to update url scheme to https"))
                    .with_kind(crate::ErrorKind::ParseUrl)?;
                tor_address_lens.ser(tor_addr);
            })
            .await?;
        Ok(())
    }
    async fn down<Db: DbHandle>(&self, _db: &mut Db, _secrets: &PgPool) -> Result<(), Error> {
        Ok(())
    }
}
