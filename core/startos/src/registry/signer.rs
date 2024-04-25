use std::collections::HashMap;

use serde::{Deserialize, Serialize};
use ts_rs::TS;
use url::Url;

use crate::prelude::*;
use crate::util::serde::{Base64, Pem};

#[derive(Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct SignerInfo {
    pub name: String,
    pub contact: Vec<ContactInfo>,
    pub keys: Vec<SignerKey>,
}

#[derive(Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub enum SignerKey {
    Ed25519(Pem<ed25519_dalek::VerifyingKey>),
}

#[derive(Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
// TODO: better types
pub enum ContactInfo {
    Email(String),
    Matrix(String),
    Website(#[ts(type = "string")] Url),
}

#[derive(Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
#[ts(export)]
#[serde(tag = "algorithm")]
pub enum SignatureInfo {
    Blake3Ed5519(Blake3Ed25519),
}

#[derive(Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct Blake3Ed25519 {
    pub hash: Base64<[u8; 32]>,
    pub signatures: HashMap<Pem<ed25519_dalek::VerifyingKey>, Base64<[u8; 64]>>,
}
