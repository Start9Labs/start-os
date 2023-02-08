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

    async fn unbind_local(&self, id: InterfaceId, external: u16) -> Result<(), Report> {
        todo!()
    }
    async fn unbind_onion(&self, id: InterfaceId, external: u16) -> Result<(), Report> {
        todo!()
    }
    async fn list_address(&self) -> Result<Vec<Address>, Report> {
        todo!()
    }
    async fn list_domains(&self) -> Result<Vec<Domain>, Report> {
        todo!()
    }
    async fn alloc_onion(&self, id: String) -> Result<Name, Report> {
        todo!()
    }
    async fn dealloc_onion(&self, id: String) -> Result<(), Report> {
        todo!()
    }
    async fn alloc_local(&self, id: String) -> Result<Name, Report> {
        todo!()
    }
    async fn dealloc_local(&self, id: String) -> Result<(), Report> {
        todo!()
    }
    async fn alloc_forward(&self, id: String) -> Result<u16, Report> {
        todo!()
    }
    async fn dealloc_forward(&self, id: String) -> Result<(), Report> {
        todo!()
    }
}
