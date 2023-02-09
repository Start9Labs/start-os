use color_eyre::eyre::eyre;
use color_eyre::Report;
use models::PackageId;
use models::{Error, InterfaceId};
use serde_json::Value;

pub struct RuntimeDropped;

pub type Callback = Box<dyn Fn(Value) -> Result<(), RuntimeDropped> + Send + Sync + 'static>; // bool indicating if

fn method_not_available() -> Error {
    Error::new(
        eyre!("method not available"),
        models::ErrorKind::InvalidRequest,
    )
}
#[derive(serde::Deserialize, serde::Serialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct AddressSchemaOnion {
    pub id: InterfaceId,
    pub external_port: u16,
}
#[derive(serde::Deserialize, serde::Serialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct AddressSchemaLocal {
    pub id: InterfaceId,
    pub external_port: u16,
}

#[derive(serde::Deserialize, serde::Serialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Address(pub String);
#[derive(serde::Deserialize, serde::Serialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Domain;
#[derive(serde::Deserialize, serde::Serialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Name;

#[async_trait::async_trait]
#[allow(unused_variables)]
pub trait OsApi: Send + Sync + 'static {
    async fn get_service_config(
        &self,
        id: PackageId,
        path: &str,
        callback: Callback,
    ) -> Result<Value, Report>;

    async fn bind_local(
        &self,
        internal_port: u16,
        address_schema: AddressSchemaLocal,
    ) -> Result<Address, Report>;
    async fn bind_onion(
        &self,
        internal_port: u16,
        address_schema: AddressSchemaOnion,
    ) -> Result<Address, Report>;

    async fn unbind_local(&self, id: InterfaceId, external: u16) -> Result<(), Report>;
    async fn unbind_onion(&self, id: InterfaceId, external: u16) -> Result<(), Report>;
    fn set_started(&self);
    async fn restart(&self);
    async fn start(&self);
    async fn stop(&self);
}
