use color_eyre::eyre::{bail, eyre};
use color_eyre::Report;
use helpers::{AddressSchemaLocal, AddressSchemaOnion, Callback, OsApi};
use itertools::Itertools;
use jsonpath_lib::Compiled;
use models::{InterfaceId, PackageId};
use reqwest::Url;
use sqlx::Acquire;

use super::try_get_running_ip;
use crate::manager::Manager;
use crate::prelude::*;
use crate::{action::Action, manager::start_stop::StartStop};
use crate::{config::hook::ConfigHook, db::model::AddressInfo};

const NULL_VALUE: &Value = &Value::Null;

#[async_trait::async_trait]
impl OsApi for Manager {
    async fn get_service_config(
        &self,
        id: Option<PackageId>,
        path: Option<&str>,
        callback: Option<Callback>,
    ) -> Result<Vec<Value>, Report> {
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
            .map(|x| x.input)
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

    async fn get_service_local_address(
        &self,
        package_id: PackageId,
        interface_name: &str,
    ) -> Result<String, Report> {
        let db = self.seed.ctx.db.peek().await?;
        get_service_local_address(db, &package_id, interface_name)
    }
    async fn get_service_tor_address(
        &self,
        package_id: PackageId,
        interface_name: &str,
    ) -> Result<String, Report> {
        let db = self.seed.ctx.db.peek().await?;
        get_service_tor_address(db, &package_id, interface_name)
    }
    async fn get_service_port_forward(
        &self,
        package_id: PackageId,
        interface_name: &str,
    ) -> Result<String, Report> {
        todo!()
    }
    async fn export_address(
        &self,
        name: String,
        description: String,
        address: Vec<String>,
        id: String,
        ui: bool,
    ) -> Result<(), Report> {
        let package_id = &self.seed.manifest.id;
        self.seed
            .ctx
            .db
            .mutate(|db| {
                let mut address_info = db
                    .as_package_data_mut()
                    .as_idx_mut(package_id)
                    .or_not_found(package_id)?
                    .expect_as_installed_mut()?
                    .as_installed_mut()
                    .as_address_info_mut();
                address_info.remove(&id)?;
                address_info.insert(
                    &id,
                    &AddressInfo {
                        name,
                        description,
                        ui,
                        addresses: address.iter().map(Url::parse).collect()?,
                    },
                )?;

                Ok(())
            })
            .await
    }
    async fn remove_address(&self, id: String) -> Result<(), Report> {
        let package_id = &self.seed.manifest.id;
        self.seed
            .ctx
            .db
            .mutate(|db| {
                db.as_package_data_mut()
                    .as_idx_mut(package_id)
                    .or_not_found(package_id)?
                    .expect_as_installed_mut()?
                    .as_installed_mut()
                    .as_address_info_mut()
                    .remove(&id)?;
                Ok(())
            })
            .await
    }
    async fn export_action(
        &self,
        name: String,
        description: String,
        id: String,
        input: Value,
        group: Option<String>,
        warning: Option<String>,
    ) -> Result<(), Report> {
        let package_id = &self.seed.manifest.id;
        self.seed
            .ctx
            .db
            .mutate(|db| {
                let mut actions = db
                    .as_package_data_mut()
                    .as_idx_mut(package_id)
                    .or_not_found(package_id)?
                    .expect_as_installed_mut()?
                    .as_installed_mut()
                    .as_actions_mut();
                actions.remove(&id)?;
                actions.insert(
                    &id,
                    &Action {
                        name,
                        description,
                        input_spec: input,
                        group,
                        warning,
                    },
                )?;
                Ok(())
            })
            .await
    }
    async fn remove_action(&self, id: String) -> Result<(), Report> {
        let package_id = &self.seed.manifest.id;
        self.seed
            .ctx
            .db
            .mutate(|db| {
                db.as_package_data_mut()
                    .as_idx_mut(package_id)
                    .or_not_found(package_id)?
                    .expect_as_installed_mut()?
                    .as_installed_mut()
                    .as_actions_mut()
                    .remove(&id)?;
                Ok(())
            })
            .await
    }
    async fn get_configured(&self) -> Result<bool, Report> {
        todo!()
    }
    async fn set_configured(&self, configured: bool) -> Result<(), Report> {
        todo!()
    }
    async fn get_ssl_certificate(
        &self,
        id: String,
        algorithm: Algorithm,
    ) -> Result<(String, String, String), Report> {
        todo!()
    }
    async fn get_ssl_key(&self, id: String, algorithm: Algorithm) -> Result<String, Report> {
        todo!()
    }
}

pub fn get_service_local_address(
    db: Model<Database>,
    package_id: &PackageId,
    interface_name: &str,
) -> Result<String, Report> {
    let addresses = db
        .as_package_data()
        .as_idx(package_id)
        .or_not_found(package_id)?
        .expect_as_installed()?
        .as_installed()
        .as_address_info()
        .as_idx(interface_name)
        .or_not_found(interface_name)?
        .as_addresses()
        .de()?
        .into_iter()
        .find(|x| x.host_str().map(|x| x.ends_with(".local")).unwrap_or(false))
        .ok_or_else(|| eyre!("No local address found"))?;
    Ok(addresses.to_string())
}
pub fn get_service_tor_address(
    db: Model<Database>,
    package_id: &PackageId,
    interface_name: &str,
) -> Result<String, Report> {
    let addresses = db
        .as_package_data()
        .as_idx(&package_id)
        .or_not_found(&package_id)?
        .expect_as_installed()?
        .as_installed()
        .as_address_info()
        .as_idx(interface_name)
        .or_not_found(interface_name)?
        .as_addresses()
        .de()?
        .into_iter()
        .find(|x| x.host_str().map(|x| x.ends_with(".onion")).unwrap_or(false))
        .ok_or_else(|| eyre!("No local address found"))?;
    Ok(addresses.to_string())
}
