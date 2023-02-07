use color_eyre::{eyre::eyre, Report};
use helpers::{AddressSchema, Callback, OsApi};
use models::PackageId;
use sqlx::Acquire;

use crate::{manager::Manager, net::keys::Key};

use super::try_get_running_ip;

#[async_trait::async_trait]
impl OsApi for Manager {
    async fn get_service_config(
        &self,
        id: PackageId,
        path: &str,
        callback: Callback,
    ) -> Result<serde_json::Value, Report> {
        todo!("BLUJ")
    }
    async fn bind(
        &self,
        internal_port: u16,
        address_schema: AddressSchema,
    ) -> Result<helpers::Address, Report> {
        let ip = try_get_running_ip(&self.seed)
            .await?
            .ok_or_else(|| eyre!("No ip available"))?;
        let mut svc = self
            .seed
            .ctx
            .net_controller
            .create_service(self.seed.manifest.id.clone(), ip)
            .await
            .map_err(|e| eyre!("Could not get to net controller: {e:?}"))?;
        let mut secrets = self.seed.ctx.secret_store.acquire().await?;
        let mut tx = secrets.begin().await?;
        let key;

        match address_schema {
            AddressSchema::Onion {
                name,
                external_port,
            } => {
                svc.add_tor(&mut tx, name.clone(), external_port, internal_port)
                    .await
                    .map_err(|e| eyre!("Could not add to tor: {e:?}"))?;
                key = Key::for_interface(&mut tx, Some((self.seed.manifest.id.clone(), name)))
                    .await
                    .map_err(|e| eyre!("Could not get network name: {e:?}"))?
                    .tor_address()
                    .to_string();
            }
            AddressSchema::Local {
                name,
                external_port,
            } => {
                svc.add_lan(&mut tx, name.clone(), external_port, internal_port, false)
                    .await
                    .map_err(|e| eyre!("Could not add to local: {e:?}"))?;
                key = Key::for_interface(&mut tx, Some((self.seed.manifest.id.clone(), name)))
                    .await
                    .map_err(|e| eyre!("Could not get network name: {e:?}"))?
                    .local_address();
            }
            AddressSchema::ForwardPort { external_port } => todo!(),
            AddressSchema::Clearnet {
                name,
                external_port,
            } => todo!(),
        }
        tx.commit().await?;
        Ok(helpers::Address(key))
    }
}
