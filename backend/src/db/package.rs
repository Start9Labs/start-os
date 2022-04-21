use patch_db::{DbHandle, LockReceipt, LockTargetId, LockType, Verifier};

use crate::s9pk::manifest::{Manifest, PackageId};
use crate::Error;

pub struct PackageReceipts {
    package_data: LockReceipt<super::model::AllPackageData, ()>,
}

impl PackageReceipts {
    pub async fn new<'a>(db: &'a mut impl DbHandle) -> Result<Self, Error> {
        let mut locks = Vec::new();

        let setup = Self::setup(&mut locks);
        Ok(setup(&db.lock_all(locks).await?)?)
    }

    pub fn setup(locks: &mut Vec<LockTargetId>) -> impl FnOnce(&Verifier) -> Result<Self, Error> {
        let package_data = crate::db::DatabaseModel::new()
            .package_data()
            .make_locker(LockType::Read)
            .add_to_keys(locks);
        move |skeleton_key| {
            Ok(Self {
                package_data: package_data.verify(&skeleton_key)?,
            })
        }
    }
}

pub async fn get_packages<Db: DbHandle>(
    db: &mut Db,
    receipts: &PackageReceipts,
) -> Result<Vec<PackageId>, Error> {
    let packages = receipts.package_data.get(db).await?;
    Ok(packages.0.keys().cloned().collect())
}

pub struct ManifestReceipts {
    manifest: LockReceipt<Manifest, String>,
}

impl ManifestReceipts {
    pub async fn new<'a>(db: &'a mut impl DbHandle, id: &PackageId) -> Result<Self, Error> {
        let mut locks = Vec::new();

        let setup = Self::setup(&mut locks, id);
        Ok(setup(&db.lock_all(locks).await?)?)
    }

    pub fn setup(
        locks: &mut Vec<LockTargetId>,
        id: &PackageId,
    ) -> impl FnOnce(&Verifier) -> Result<Self, Error> {
        let manifest = crate::db::DatabaseModel::new()
            .package_data()
            .star()
            .manifest()
            .make_locker(LockType::Read)
            .add_to_keys(locks);
        move |skeleton_key| {
            Ok(Self {
                manifest: manifest.verify(&skeleton_key)?,
            })
        }
    }
}

pub async fn get_manifest<Db: DbHandle>(
    db: &mut Db,
    pkg: &PackageId,
    receipts: &ManifestReceipts,
) -> Result<Option<Manifest>, Error> {
    Ok(receipts.manifest.get(db, pkg).await?)
}
