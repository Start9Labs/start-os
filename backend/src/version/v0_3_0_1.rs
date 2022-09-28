use std::path::Path;

use emver::VersionRange;
use tokio::process::Command;

use super::*;
use crate::disk::BOOT_RW_PATH;
use crate::update::query_mounted_label;
use crate::util::Invoke;

const V0_3_0_1: emver::Version = emver::Version::new(0, 3, 0, 1);

#[derive(Debug, Clone)]
pub struct Version;
#[async_trait]
impl VersionT for Version {
    type Previous = v0_3_0::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> emver::Version {
        V0_3_0_1
    }
    fn compat(&self) -> &'static VersionRange {
        &*v0_3_0::V0_3_0_COMPAT
    }
    async fn up<Db: DbHandle>(&self, _db: &mut Db) -> Result<(), Error> {
        let (_, current) = query_mounted_label().await?;
        Command::new("sed")
            .arg("-i")
            .arg(&format!(
                "s/PARTUUID=cb15ae4d-\\(03\\|04\\)/PARTUUID={}/g",
                current.0.part_uuid()
            ))
            .arg(Path::new(BOOT_RW_PATH).join("cmdline.txt.orig"))
            .invoke(crate::ErrorKind::Filesystem)
            .await?;
        Ok(())
    }
    async fn down<Db: DbHandle>(&self, _db: &mut Db) -> Result<(), Error> {
        Ok(())
    }
}
