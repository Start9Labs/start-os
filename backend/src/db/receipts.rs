use std::collections::BTreeMap;

use emver::VersionRange;
use patch_db::LockReceipt;

use crate::{
    dependencies::{DepInfo, Dependencies, DependencyErrors},
    s9pk::manifest::{Manifest, PackageId},
    status::Status,
    util::Version,
};

use super::model::CurrentDependencyInfo;

pub trait DependencyErrorsAtReceipt: Send + Sync {
    fn dependency_errors(&self) -> &LockReceipt<DependencyErrors, String>;
}

// serverInfo/eosVersionCompat
pub trait VersionRangeReceipt: Send + Sync {
    fn version_range(&self) -> &LockReceipt<VersionRange, ()>;
}

// serverInfo/serverVersion
pub trait ServerVersionReceipt: Send + Sync {
    fn server_version(&self) -> &LockReceipt<Version, ()>;
}

// serverInfo/<id>/currentDependents
pub trait CurrentDependentsReceipt: Send + Sync {
    fn current_dependents(
        &self,
    ) -> &LockReceipt<BTreeMap<PackageId, CurrentDependencyInfo>, String>;
}
/// serverInfo/<id>/status
pub trait PackageStatusReceipt: Send + Sync {
    fn status(&self) -> &LockReceipt<Status, String>;
}

/// serverInfo/<id>/manifest/dependencies
pub trait DependenciesReceipt: Send + Sync {
    fn dependencies(&self) -> &LockReceipt<Dependencies, String>;
}

/// serverInfo/<id>/manifest/dependencies/<dep_id>
pub trait DependencyReceipt: Send + Sync {
    fn dependency(&self) -> &LockReceipt<DepInfo, (String, String)>;
}

/// serverInfo/<id>/manifest
pub trait ManifestReceipt: Send + Sync {
    fn manifest(&self) -> &LockReceipt<Manifest, String>;
}
/// Used in the dependencies healing, needed to have these four traits together for these
pub trait HealTransitiveReceipt:
    CurrentDependentsReceipt
    + PackageStatusReceipt
    + DependenciesReceipt
    + DependencyReceipt
    + ManifestReceipt
{
}
