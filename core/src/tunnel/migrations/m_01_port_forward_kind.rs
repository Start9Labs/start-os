use imbl_value::InternedString;

use super::TunnelMigration;
use crate::prelude::*;

pub struct PortForwardKind;
impl TunnelMigration for PortForwardKind {
    fn action(&self, db: &mut Value) -> Result<(), Error> {
        for (_, value) in db["portForwards"].as_object_mut().unwrap().iter_mut() {
            if let Some(obj) = value.as_object_mut() {
                if !obj.contains_key(&InternedString::intern("kind")) {
                    obj.insert(InternedString::intern("kind"), "dnat".into());
                }
            }
        }
        Ok(())
    }
}
