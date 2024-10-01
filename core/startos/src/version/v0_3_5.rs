use exver::{ExtendedVersion, VersionRange};

use super::VersionT;
use crate::db::model::Database;
use crate::prelude::*;
use crate::version::Current;

lazy_static::lazy_static! {
    pub static ref V0_3_0_COMPAT: VersionRange = VersionRange::and(
        VersionRange::anchor(
            exver::GTE,
            ExtendedVersion::new(
                exver::Version::new([0, 3, 0], []),
                exver::Version::default(),
            ),
        ),
        VersionRange::anchor(
            exver::LTE,
            ExtendedVersion::new(
                Current::new().semver(),
                exver::Version::default(),
            )
        ),
    );
    static ref V0_3_5: exver::Version = exver::Version::new([0, 3, 5], []);
}

#[derive(Clone, Debug)]
pub struct Version;

impl VersionT for Version {
    type Previous = Self;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> exver::Version {
        V0_3_5.clone()
    }
    fn compat(&self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    async fn up(&self, _db: &TypedPatchDb<Database>) -> Result<(), Error> {
        Ok(())
    }
    async fn down(&self, _db: &TypedPatchDb<Database>) -> Result<(), Error> {
        Ok(())
    }
}
