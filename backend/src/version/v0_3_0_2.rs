use std::path::Path;

use emver::VersionRange;
use tokio::process::Command;

use super::*;
use crate::disk::quirks::{fetch_quirks, save_quirks, update_quirks};
use crate::disk::BOOT_RW_PATH;
use crate::update::query_mounted_label;
use crate::util::Invoke;

const V0_3_0_2: emver::Version = emver::Version::new(0, 3, 0, 2);

pub struct Version;
#[async_trait]
impl VersionT for Version {
    type Previous = v0_3_0_1::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> emver::Version {
        V0_3_0_2
    }
    fn compat(&self) -> &'static VersionRange {
        &*v0_3_0::V0_3_0_COMPAT
    }
    async fn up<Db: DbHandle>(&self, _db: &mut Db) -> Result<(), Error> {
        Ok(())
    }
    async fn down<Db: DbHandle>(&self, _db: &mut Db) -> Result<(), Error> {
        Ok(())
    }
}
