use helpers::{Callback, OsApi};
use models::PackageId;

use crate::manager::Manager;
use crate::Error;

#[async_trait::async_trait]
impl OsApi for Manager {
    async fn get_service_config(
        &self,
        id: PackageId,
        path: &str,
        callback: Callback,
    ) -> Result<serde_json::Value, Error> {
        todo!()
    }
}
