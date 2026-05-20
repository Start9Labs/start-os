use std::path::Path;

use chrono::{DateTime, Utc};
use exver::{Version, VersionRange};
use serde::{Deserialize, Serialize};
use ts_rs::TS;
use url::Url;

use crate::PackageId;
use crate::registry::package::index::PackageMetadata;
use crate::rpc_continuations::Guid;
use crate::util::VersionString;
use crate::util::serde::Base64;

#[derive(Debug, Clone, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct PackageVersionAddData {
    pub package_id: PackageId,
    pub version: VersionString,
    pub is_first_version: bool,
    pub is_update: bool,
    #[ts(type = "string[]")]
    pub urls: Vec<Url>,
    pub metadata: PackageMetadata,
}

#[derive(Debug, Clone, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct PackageRemoveData {
    pub package_id: PackageId,
    pub version: Option<VersionString>,
    pub sighash: Option<Base64<[u8; 32]>>,
}

#[derive(Debug, Clone, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct OsVersionAddData {
    #[ts(type = "string")]
    pub version: Version,
    pub headline: String,
    pub release_notes: String,
    #[ts(type = "string")]
    pub source_version: VersionRange,
    pub is_update: bool,
}

#[derive(Debug, Clone, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct OsVersionRemoveData {
    #[ts(type = "string")]
    pub version: Version,
}

#[derive(Debug, Clone, Deserialize, Serialize, TS)]
#[serde(tag = "topic", content = "data")]
#[ts(export)]
pub enum RegistryEventData {
    #[serde(rename = "package.version.add")]
    PackageVersionAdd(PackageVersionAddData),
    #[serde(rename = "package.remove")]
    PackageRemove(PackageRemoveData),
    #[serde(rename = "os.version.add")]
    OsVersionAdd(OsVersionAddData),
    #[serde(rename = "os.version.remove")]
    OsVersionRemove(OsVersionRemoveData),
}

impl RegistryEventData {
    pub fn topic(&self) -> &'static str {
        match self {
            Self::PackageVersionAdd(_) => "package.version.add",
            Self::PackageRemove(_) => "package.remove",
            Self::OsVersionAdd(_) => "os.version.add",
            Self::OsVersionRemove(_) => "os.version.remove",
        }
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct RegistryEvent {
    pub id: Guid,
    pub occurred_at: DateTime<Utc>,
    #[serde(flatten)]
    pub data: RegistryEventData,
}

impl RegistryEvent {
    pub fn new(data: RegistryEventData) -> Self {
        Self {
            id: Guid::new(),
            occurred_at: Utc::now(),
            data,
        }
    }

    pub fn topic(&self) -> &'static str {
        self.data.topic()
    }
}

impl TS for RegistryEvent {
    type WithoutGenerics = Self;
    fn decl() -> String {
        format!("type {} = {}", Self::name(), Self::inline())
    }
    fn decl_concrete() -> String {
        Self::decl()
    }
    fn name() -> String {
        "RegistryEvent".into()
    }
    fn inline() -> String {
        "{ id: Guid, occurredAt: string } & RegistryEventData".into()
    }
    fn inline_flattened() -> String {
        Self::inline()
    }
    fn visit_dependencies(v: &mut impl ts_rs::TypeVisitor)
    where
        Self: 'static,
    {
        v.visit::<Guid>();
        v.visit::<RegistryEventData>();
    }
    fn output_path() -> Option<&'static Path> {
        Some(Path::new("RegistryEvent.ts"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn os_version_remove_round_trips_with_flat_envelope() {
        let event = RegistryEvent::new(RegistryEventData::OsVersionRemove(OsVersionRemoveData {
            version: "0.4.0".parse().unwrap(),
        }));
        let v = serde_json::to_value(&event).unwrap();
        let obj = v.as_object().expect("event serializes as object");
        assert!(obj.contains_key("id"));
        assert!(obj.contains_key("occurredAt"));
        assert_eq!(obj["topic"], serde_json::json!("os.version.remove"));
        assert_eq!(obj["data"]["version"], serde_json::json!("0.4.0"));
        let parsed: RegistryEvent = serde_json::from_value(v).unwrap();
        assert_eq!(parsed.topic(), "os.version.remove");
        match parsed.data {
            RegistryEventData::OsVersionRemove(d) => assert_eq!(d.version.to_string(), "0.4.0"),
            _ => panic!("wrong variant"),
        }
    }
}
