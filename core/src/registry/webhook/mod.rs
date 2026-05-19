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

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct WebhookSubscriber {
    pub url: Url,
}

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
pub struct RegistryEvent {
    pub id: Guid,
    pub topic: String,
    #[ts(type = "string")]
    pub occurred_at: DateTime<Utc>,
    #[ts(type = "any")]
    pub data: Value,
}

impl RegistryEvent {
    pub fn new(topic: impl Into<String>, data: Value) -> Self {
        Self {
            id: Guid::new(),
            topic: topic.into(),
            occurred_at: Utc::now(),
            data,
        }
    }
}

#[derive(Debug, Clone, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct DeliveryAttempt {
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
