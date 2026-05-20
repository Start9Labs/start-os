use std::collections::BTreeMap;

use chrono::{DateTime, Utc};
use ed25519_dalek::SigningKey;
use serde::{Deserialize, Serialize};
use ts_rs::TS;
use url::Url;

use crate::prelude::*;
use crate::rpc_continuations::Guid;
use crate::util::serde::Pem;

pub mod api;
pub mod dispatcher;
pub mod event;

pub use event::{
    OsVersionAddData, OsVersionRemoveData, PackageRemoveData, PackageVersionAddData, RegistryEvent,
    RegistryEventData,
};

#[derive(Debug, Clone, Default, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct WebhookLog {
    pub events: BTreeMap<Guid, WebhookEventRecord>,
}

#[derive(Debug, Clone, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct WebhookEventRecord {
    pub event: RegistryEvent,
    pub attempts: Vec<DeliveryAttempt>,
}

#[derive(Debug, Clone, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct DeliveryAttempt {
    #[ts(type = "string")]
    pub subscriber: Url,
    #[ts(type = "string")]
    pub at: DateTime<Utc>,
    pub status_code: Option<u16>,
    pub error: Option<String>,
    pub duration_ms: u64,
    pub replay: bool,
}

pub fn generate_signing_key() -> Pem<SigningKey> {
    Pem(SigningKey::generate(
        &mut ssh_key::rand_core::OsRng::default(),
    ))
}
