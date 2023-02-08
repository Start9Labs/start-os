use color_eyre::{eyre::eyre, Report};
use helpers::{AddressSchemaLocal, AddressSchemaOnion, Callback, OsApi};
use models::{InterfaceId, PackageId};
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
    async fn bind_local(
        &self,
        internal_port: u16,
        address_schema: AddressSchemaLocal,
    ) -> Result<helpers::Address, Report> {
        let ip = try_get_running_ip(&self.seed)
            .await?
            .ok_or_else(|| eyre!("No ip available"))?;
        let AddressSchemaLocal { id, external_port } = address_schema;
        let mut svc = self
            .seed
            .ctx
            .net_controller
            .create_service(self.seed.manifest.id.clone(), ip)
            .await
            .map_err(|e| eyre!("Could not get to net controller: {e:?}"))?;
        let mut secrets = self.seed.ctx.secret_store.acquire().await?;
        let mut tx = secrets.begin().await?;

        svc.add_lan(&mut tx, id.clone(), external_port, internal_port, false)
            .await
            .map_err(|e| eyre!("Could not add to local: {e:?}"))?;
        let key = Key::for_interface(&mut tx, Some((self.seed.manifest.id.clone(), id)))
            .await
            .map_err(|e| eyre!("Could not get network name: {e:?}"))?
            .local_address();

        tx.commit().await?;
        Ok(helpers::Address(key))
    }
    async fn bind_onion(
        &self,
        internal_port: u16,
        address_schema: AddressSchemaOnion,
    ) -> Result<helpers::Address, Report> {
        let AddressSchemaOnion { id, external_port } = address_schema;
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

        svc.add_tor(&mut tx, id.clone(), external_port, internal_port)
            .await
            .map_err(|e| eyre!("Could not add to tor: {e:?}"))?;
        let key = Key::for_interface(&mut tx, Some((self.seed.manifest.id.clone(), id)))
            .await
            .map_err(|e| eyre!("Could not get network name: {e:?}"))?
            .tor_address()
            .to_string();
        tx.commit().await?;
        Ok(helpers::Address(key))
    }
    async fn unbind_onion(&self, id: InterfaceId, external: u16) -> Result<(), Report> {
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

        svc.remove_tor(id, external)
            .await
            .map_err(|e| eyre!("Could not add to tor: {e:?}"))?;
        Ok(())
    }
    async fn unbind_local(&self, id: InterfaceId, external: u16) -> Result<(), Report> {
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

        svc.remove_lan(id, external)
            .await
            .map_err(|e| eyre!("Could not add to local: {e:?}"))?;
        Ok(())
    }
}
