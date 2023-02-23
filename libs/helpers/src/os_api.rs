use std::sync::Arc;

use color_eyre::eyre::eyre;
use color_eyre::Report;
use models::{InterfaceId, PackageId};
use serde_json::Value;
use tokio::sync::mpsc;

pub struct RuntimeDropped;

pub struct Callback {
    id: Arc<String>,
    sender: mpsc::UnboundedSender<(Arc<String>, Vec<Value>)>,
}
impl Callback {
    pub fn new(id: String, sender: mpsc::UnboundedSender<(Arc<String>, Vec<Value>)>) -> Self {
        Self {
            id: Arc::new(id),
            sender,
        }
    }
    pub fn is_listening(&self) -> bool {
        self.sender.is_closed()
    }
    pub fn call(&self, args: Vec<Value>) -> Result<(), RuntimeDropped> {
        self.sender
            .send((self.id.clone(), args))
            .map_err(|_| RuntimeDropped)
    }
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
        callback: Option<Callback>,
    ) -> Result<Vec<Value>, Report>;

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
    fn set_started(&self) -> Result<(), Report>;
    async fn restart(&self) -> Result<(), Report>;
    async fn start(&self) -> Result<(), Report>;
    async fn stop(&self) -> Result<(), Report>;
}
