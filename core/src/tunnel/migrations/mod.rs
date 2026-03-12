use patch_db::ModelExt;

use crate::prelude::*;
use crate::tunnel::db::TunnelDatabase;

mod m_00_port_forward_entry;

pub trait TunnelMigration {
    fn name(&self) -> &'static str {
        let val = std::any::type_name_of_val(self);
        val.rsplit_once("::").map_or(val, |v| v.1)
    }
    fn action(&self, db: &mut Value) -> Result<(), Error>;
}

pub const MIGRATIONS: &[&dyn TunnelMigration] = &[
    &m_00_port_forward_entry::PortForwardEntry,
];

#[instrument(skip_all)]
pub fn run_migrations(db: &mut Model<TunnelDatabase>) -> Result<(), Error> {
    let mut migrations = db.as_migrations().de().unwrap_or_default();
    for migration in MIGRATIONS {
        let name = migration.name();
        if !migrations.contains(name) {
            migration.action(ModelExt::as_value_mut(db))?;
            migrations.insert(name.into());
        }
    }
    let mut db_deser = db.de()?;
    db_deser.migrations = migrations;
    db.ser(&db_deser)?;
    Ok(())
}
