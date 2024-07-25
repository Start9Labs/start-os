use exver::{PreReleaseSegment, VersionRange};

use super::v0_3_5::V0_3_0_COMPAT;
use super::{v0_3_5_2, VersionT};
use crate::db::model::Database;
use crate::prelude::*;

lazy_static::lazy_static! {
    static ref V0_3_6_alpha_0: exver::Version = exver::Version::new(
        [0, 3, 6],
        [PreReleaseSegment::String("alpha".into()), 0.into()]
    );
}

#[derive(Clone, Debug)]
pub struct Version;

impl VersionT for Version {
    type Previous = v0_3_5_2::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> exver::Version {
        V0_3_6_alpha_0.clone()
    }
    fn compat(&self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    async fn up(&self, _db: &TypedPatchDb<Database>) -> Result<(), Error> {
        Err(Error::new(eyre!("unimplemented"), ErrorKind::Unknown))
    }
    async fn down(&self, _db: &TypedPatchDb<Database>) -> Result<(), Error> {
        Ok(())
    }
}