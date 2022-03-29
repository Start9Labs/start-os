use std::collections::BTreeMap;

use patch_db::LockReceipt;

use crate::{
    dependencies::{DepInfo, Dependencies},
    s9pk::manifest::PackageId,
    status::Status,
};

use super::model::CurrentDependencyInfo;

pub trait CurrentDependentsLock: Send + Sync {
    fn current_dependents(
        &self,
    ) -> &LockReceipt<BTreeMap<PackageId, CurrentDependencyInfo>, String>;
}
pub trait PackageStatusLock: Send + Sync {
    fn status(&self) -> &LockReceipt<Status, String>;
}
pub trait DependenciesLock: Send + Sync {
    fn dependencies(&self) -> &LockReceipt<Dependencies, String>;
}
pub trait DependencyLock: Send + Sync {
    fn dependency(&self) -> &LockReceipt<DepInfo, (String, String)>;
}

/// Used in the dependencies healing, needed to have these four traits together for these
pub trait HealTransitiveLock:
    CurrentDependentsLock + PackageStatusLock + DependenciesLock + DependencyLock
{
}
