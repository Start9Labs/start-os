use patch_db::ModelExt;

use crate::prelude::*;
use crate::registry::RegistryDatabase;

mod m_00_package_signer_scope;
mod m_01_package_s9pk_array;

pub trait RegistryMigration {
    fn name(&self) -> &'static str;
    fn action(&self, db: &mut Value) -> Result<(), Error>;
}

pub const MIGRATIONS: &[&dyn RegistryMigration] = &[
    &m_00_package_signer_scope::PackageSignerScopeMigration,
    &m_01_package_s9pk_array::PackageS9pkArray,
];

#[instrument(skip_all)]
pub fn run_migrations(db: &mut Model<RegistryDatabase>) -> Result<(), Error> {
    let mut migrations = db.as_migrations().de().unwrap_or_default();
    for migration in MIGRATIONS {
        if !migrations.contains(migration.name()) {
            migration.action(ModelExt::as_value_mut(db))?;
            migrations.insert(migration.name().into());
        }
    }
    let mut db_deser = db.de()?;
    db_deser.migrations = migrations;
    db.ser(&db_deser)?;
    Ok(())
}
