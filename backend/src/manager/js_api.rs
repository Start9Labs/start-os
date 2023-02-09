use color_eyre::{
    eyre::{bail, eyre},
    Report,
};
use helpers::{AddressSchemaLocal, AddressSchemaOnion, Callback, OsApi};
use models::{InterfaceId, PackageId};
use serde_json::Value;
use sqlx::Acquire;

use crate::{manager::Manager, net::keys::Key};

use super::try_get_running_ip;

struct ConfigMapping(serde_json::Map<String, Value>);
impl ConfigMapping {
    fn with_path(&self, config_path: &ConfigPath) -> &Value {
        let null_value = Value::Null;
        let mut value: &Value = self.0.get(&config_path.paths[0]).unwrap_or(&null_value);
        for path in config_path.paths.iter().skip(1) {
            value = &value[&path];
        }
        value
    }
}
struct ConfigPath {
    paths: Vec<String>,
}
impl ConfigPath {
    fn parse(value: &str) -> Self {
        let paths = value
            .split("/")
            .into_iter()
            .filter(|x| !x.is_empty())
            .map(|x| x.to_string())
            .collect();
        Self { paths }
    }
}

#[async_trait::async_trait]
impl OsApi for Manager {
    async fn get_service_config(
        &self,
        id: PackageId,
        path: &str,
        callback: Callback,
    ) -> Result<serde_json::Value, Report> {
        let found = match self
            .seed
            .manifest
            .dependencies
            .0
            .iter()
            .find(|x| x.0 == &id)
        {
            None => bail!("Cannot get a service that is not part of the dependencies"),
            Some(a) => a,
        };

        let config = match crate::config::get(self.seed.ctx.clone(), id.clone(), None)
            .await
            .map(|x| x.config)
        {
            Ok(Some(a)) => ConfigMapping(a),
            Ok(None) => bail!("No current config for the service"),
            Err(err) => bail!("Could not fetch the config. {err}"),
        };

        let path = ConfigPath::parse(path);

        let filtered_value = config.with_path(&path);
        Ok(filtered_value.clone())
    }
    // Get tor key - base 32

    // Certificate + Certificate key for interface

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
}
