use exver::VersionRange;

use super::v0_3_5::V0_3_0_COMPAT;
use super::{v0_3_5, VersionT};
use crate::db::model::Database;
use crate::prelude::*;

lazy_static::lazy_static! {
    static ref V0_3_5_1: exver::Version = exver::Version::new([0, 3, 5, 1], []);
}

#[derive(Clone, Debug)]
pub struct Version;

impl VersionT for Version {
    type Previous = v0_3_5::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> exver::Version {
        V0_3_5_1.clone()
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
