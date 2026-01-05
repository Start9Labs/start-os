use imbl::vector;
use imbl_value::json;

use super::RegistryMigration;
use crate::prelude::*;

pub struct PackageS9pkArray;
impl RegistryMigration for PackageS9pkArray {
    fn name(&self) -> &'static str {
        "PackageS9pkArray"
    }
    fn action(&self, db: &mut Value) -> Result<(), Error> {
        for (_, info) in db["index"]["package"]["packages"]
            .as_object_mut()
            .unwrap()
            .iter_mut()
        {
            for (_, info) in info["versions"].as_object_mut().unwrap().iter_mut() {
                let hw_req = info["hardwareRequirements"].take();
                let mut s9pk = info["s9pk"].take();
                s9pk["urls"] = Value::Array(vector![s9pk["url"].take()]);
                info["s9pks"] = Value::Array(vector![Value::Array(vector![hw_req, s9pk])]);
            }
        }

        Ok(())
    }
}
