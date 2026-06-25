use imbl_value::json;

use super::TunnelMigration;
use crate::prelude::*;

pub struct PortForwardEntry;
impl TunnelMigration for PortForwardEntry {
    fn action(&self, db: &mut Value) -> Result<(), Error> {
        for (_, value) in db["portForwards"].as_object_mut().unwrap().iter_mut() {
            if value.is_string() {
                *value = json!({
                    "target": value.clone(),
                    "label": null,
                    "enabled": true,
                });
            }
        }
        Ok(())
    }
}
