use std::sync::Arc;

use color_eyre::Report;
use imbl_value::imbl::Vector;
use imbl_value::Value;
use models::{InterfaceId, PackageId};
use tokio::sync::mpsc;

pub struct RuntimeDropped;
#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum Algorithm {
    Ecdsa,
    Ed25519,
}

pub struct Callback {
    id: Arc<String>,
    sender: mpsc::UnboundedSender<(Arc<String>, Vector<Value>)>,
}
impl Callback {
    pub fn new(id: String, sender: mpsc::UnboundedSender<(Arc<String>, Vector<Value>)>) -> Self {
        Self {
            id: Arc::new(id),
            sender,
        }
    }
    pub fn is_listening(&self) -> bool {
        self.sender.is_closed()
    }
    pub fn call(&self, args: Vector<Value>) -> Result<(), RuntimeDropped> {
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
        id: Option<PackageId>,
        path: Option<&str>,
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
    async fn get_service_local_address(
        &self,
        pcakge_id: PackageId,
        interface_name: &str,
    ) -> Result<String, Report>;
    async fn get_service_tor_address(
        &self,
        pcakge_id: PackageId,
        interface_name: &str,
    ) -> Result<String, Report>;
    async fn get_service_port_forward(
        &self,
        pcakge_id: PackageId,
        interface_name: &str,
    ) -> Result<String, Report>;
    async fn export_address(
        &self,
        name: String,
        description: String,
        address: String,
        id: String,
        ui: bool,
    ) -> Result<String, Report>;
    async fn remove_address(&self, id: String) -> Result<(), Report>;
    async fn export_action(
        &self,
        name: String,
        description: String,
        id: String,
        input: Value,
        group: Option<String>,
    ) -> Result<(), Report>;
    async fn remove_action(&self, id: String) -> Result<(), Report>;
    async fn get_configured(&self) -> Result<bool, Report>;
    async fn set_configured(&self, configured: bool) -> Result<(), Report>;
    async fn get_ssl_certificate(
        &self,
        id: String,
        algorithm: Algorithm,
    ) -> Result<(String, String, String), Report>;
    async fn get_ssl_key(&self, id: String, algorithm: Algorithm) -> Result<String, Report>;
}
