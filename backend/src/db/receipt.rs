use patch_db::{DbHandle, LockTargetId, Verifier};

use crate::Error;

use async_trait::async_trait;

#[async_trait]
pub trait Receipt<Ids>: Sized {
    type SetupFn: FnOnce(&Verifier) -> Result<Self, Error>;
    async fn new<Db: DbHandle>(db: &mut Db, ids: Ids) -> Result<Self, Error>;
    fn setup(locks: &mut Vec<LockTargetId>, ids: Ids) -> Self::SetupFn;
}
