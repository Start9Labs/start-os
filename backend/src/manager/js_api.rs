use helpers::{AddressSchema, Callback, OsApi};
use models::{InterfaceId, PackageId};
use sqlx::Acquire;

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
    async fn bind(
        &self,
        internal_port: u16,
        address_schema: AddressSchema,
    ) -> Result<helpers::Address, Error> {
        let ip = todo!("IP");
        let mut svc = self
            .seed
            .ctx
            .net_controller
            .create_service(self.seed.manifest.id.clone(), ip)
            .await?;
        let mut secrets = self.seed.ctx.secret_store.acquire().await?;
        let mut tx = secrets.begin().await?;

        match address_schema {
            AddressSchema::Onion {
                name,
                external_port,
            } => {
                svc.add_tor(&mut tx, name, external_port, internal_port)
                    .await?;
            }
            AddressSchema::Local {
                name,
                external_port,
            } => {
                svc.add_lan(&mut tx, name, external_port, internal_port, false)
                    .await?;
            }
            AddressSchema::ForwardPort { external_port } => todo!(),
            AddressSchema::Clearnet {
                name,
                external_port,
            } => todo!(),
        }
        tx.commit().await?;
        Ok(todo!("Address"))
    }
}
