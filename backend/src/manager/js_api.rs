use color_eyre::{
    eyre::{bail, eyre},
    Report,
};
use helpers::{AddressSchemaLocal, AddressSchemaOnion, Callback, OsApi};
use itertools::Itertools;
use jsonpath_lib::Compiled;
use models::{InterfaceId, PackageId};
use serde_json::Value;
use sqlx::Acquire;

use crate::{
    config::hook::ConfigHook,
    manager::{start_stop::StartStop, Manager},
    net::keys::Key,
};

use super::try_get_running_ip;

const NULL_VALUE: &Value = &Value::Null;

#[async_trait::async_trait]
impl OsApi for Manager {
    async fn get_service_config(
        &self,
        id: PackageId,
        path: &str,
        callback: Option<Callback>,
    ) -> Result<Vec<serde_json::Value>, Report> {
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
            Ok(Some(a)) => a,
            Ok(None) => bail!("No current config for the service"),
            Err(err) => bail!("Could not fetch the config. {err}"),
        };

        let path = Compiled::compile(path).map_err(|e| eyre!("{e}"))?;

        let filtered_values = path
            .select(&Value::Object(config))?
            .into_iter()
            .cloned()
            .collect_vec();

        if let Some(callback) = callback {
            self.seed
                .ctx
                .add_config_hook(
                    id,
                    ConfigHook {
                        path,
                        prev: filtered_values.clone(),
                        callback,
                    },
                )
                .await;
        }

        Ok(filtered_values)
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

        let addr = svc
            .add_lan(&mut tx, id.clone(), external_port, internal_port, false)
            .await
            .map_err(|e| eyre!("Could not add to local: {e:?}"))?;

        tx.commit().await?;
        Ok(helpers::Address(addr))
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

        let addr = svc
            .add_tor(&mut tx, id.clone(), external_port, internal_port)
            .await
            .map_err(|e| eyre!("Could not add to tor: {e:?}"))?;

        tx.commit().await?;
        Ok(helpers::Address(addr))
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

        svc.remove_tor(id, external)
            .await
            .map_err(|e| eyre!("Could not add to tor: {e:?}"))?;
        Ok(())
    }

    fn set_started(&self) -> Result<(), Report> {
        self.manage_container
            .current_state
            .send_modify(|x| *x = StartStop::Start);
        Ok(())
    }

    async fn restart(&self) -> Result<(), Report> {
        self.perform_restart().await;
        Ok(())
    }

    async fn start(&self) -> Result<(), Report> {
        self.manage_container
            .wait_for_desired(StartStop::Start)
            .await;
        Ok(())
    }

    async fn stop(&self) -> Result<(), Report> {
        self.manage_container
            .wait_for_desired(StartStop::Stop)
            .await;
        Ok(())
    }
}
