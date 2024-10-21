use std::collections::{BTreeMap, BTreeSet};

use chrono::{DateTime, Utc};
use exver::VersionRange;
use imbl_value::InternedString;
use models::{
    ActionId, DataUrl, HealthCheckId, HostId, PackageId, ReplayId, ServiceInterfaceId,
    VersionString,
};
use patch_db::json_ptr::JsonPointer;
use patch_db::HasModel;
use reqwest::Url;
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::net::host::Hosts;
use crate::net::service_interface::ServiceInterface;
use crate::prelude::*;
use crate::progress::FullProgress;
use crate::s9pk::manifest::Manifest;
use crate::status::MainStatus;
use crate::util::serde::{is_partial_of, Pem};

#[derive(Debug, Default, Deserialize, Serialize, TS)]
#[ts(export)]
pub struct AllPackageData(pub BTreeMap<PackageId, PackageDataEntry>);
impl Map for AllPackageData {
    type Key = PackageId;
    type Value = PackageDataEntry;
    fn key_str(key: &Self::Key) -> Result<impl AsRef<str>, Error> {
        Ok(key)
    }
    fn key_string(key: &Self::Key) -> Result<InternedString, Error> {
        Ok(key.clone().into())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ManifestPreference {
    Old,
    New,
}

#[derive(Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[serde(tag = "state")]
#[model = "Model<Self>"]
#[ts(export)]
pub enum PackageState {
    Installing(InstallingState),
    Restoring(InstallingState),
    Updating(UpdatingState),
    Installed(InstalledState),
    Removing(InstalledState),
}
impl PackageState {
    pub fn expect_installed(&self) -> Result<&InstalledState, Error> {
        match self {
            Self::Installed(a) => Ok(a),
            _ => Err(Error::new(
                eyre!(
                    "Package {} is not in installed state",
                    self.as_manifest(ManifestPreference::Old).id
                ),
                ErrorKind::InvalidRequest,
            )),
        }
    }
    pub fn expect_removing(&self) -> Result<&InstalledState, Error> {
        match self {
            Self::Removing(a) => Ok(a),
            _ => Err(Error::new(
                eyre!(
                    "Package {} is not in removing state",
                    self.as_manifest(ManifestPreference::Old).id
                ),
                ErrorKind::InvalidRequest,
            )),
        }
    }
    pub fn into_installing_info(self) -> Option<InstallingInfo> {
        match self {
            Self::Installing(InstallingState { installing_info })
            | Self::Restoring(InstallingState { installing_info }) => Some(installing_info),
            Self::Updating(UpdatingState {
                installing_info, ..
            }) => Some(installing_info),
            Self::Installed(_) | Self::Removing(_) => None,
        }
    }
    pub fn as_installing_info(&self) -> Option<&InstallingInfo> {
        match self {
            Self::Installing(InstallingState { installing_info })
            | Self::Restoring(InstallingState { installing_info }) => Some(installing_info),
            Self::Updating(UpdatingState {
                installing_info, ..
            }) => Some(installing_info),
            Self::Installed(_) | Self::Removing(_) => None,
        }
    }
    pub fn as_installing_info_mut(&mut self) -> Option<&mut InstallingInfo> {
        match self {
            Self::Installing(InstallingState { installing_info })
            | Self::Restoring(InstallingState { installing_info }) => Some(installing_info),
            Self::Updating(UpdatingState {
                installing_info, ..
            }) => Some(installing_info),
            Self::Installed(_) | Self::Removing(_) => None,
        }
    }
    pub fn into_manifest(self, preference: ManifestPreference) -> Manifest {
        match self {
            Self::Installing(InstallingState {
                installing_info: InstallingInfo { new_manifest, .. },
            })
            | Self::Restoring(InstallingState {
                installing_info: InstallingInfo { new_manifest, .. },
            }) => new_manifest,
            Self::Updating(UpdatingState { manifest, .. })
                if preference == ManifestPreference::Old =>
            {
                manifest
            }
            Self::Updating(UpdatingState {
                installing_info: InstallingInfo { new_manifest, .. },
                ..
            }) => new_manifest,
            Self::Installed(InstalledState { manifest })
            | Self::Removing(InstalledState { manifest }) => manifest,
        }
    }
    pub fn as_manifest(&self, preference: ManifestPreference) -> &Manifest {
        match self {
            Self::Installing(InstallingState {
                installing_info: InstallingInfo { new_manifest, .. },
            })
            | Self::Restoring(InstallingState {
                installing_info: InstallingInfo { new_manifest, .. },
            }) => new_manifest,
            Self::Updating(UpdatingState { manifest, .. })
                if preference == ManifestPreference::Old =>
            {
                manifest
            }
            Self::Updating(UpdatingState {
                installing_info: InstallingInfo { new_manifest, .. },
                ..
            }) => new_manifest,
            Self::Installed(InstalledState { manifest })
            | Self::Removing(InstalledState { manifest }) => manifest,
        }
    }
    pub fn as_manifest_mut(&mut self, preference: ManifestPreference) -> &mut Manifest {
        match self {
            Self::Installing(InstallingState {
                installing_info: InstallingInfo { new_manifest, .. },
            })
            | Self::Restoring(InstallingState {
                installing_info: InstallingInfo { new_manifest, .. },
            }) => new_manifest,
            Self::Updating(UpdatingState { manifest, .. })
                if preference == ManifestPreference::Old =>
            {
                manifest
            }
            Self::Updating(UpdatingState {
                installing_info: InstallingInfo { new_manifest, .. },
                ..
            }) => new_manifest,
            Self::Installed(InstalledState { manifest })
            | Self::Removing(InstalledState { manifest }) => manifest,
        }
    }
}
impl Model<PackageState> {
    pub fn expect_installed(&self) -> Result<&Model<InstalledState>, Error> {
        match self.as_match() {
            PackageStateMatchModelRef::Installed(a) => Ok(a),
            _ => Err(Error::new(
                eyre!(
                    "Package {} is not in installed state",
                    self.as_manifest(ManifestPreference::Old).as_id().de()?
                ),
                ErrorKind::InvalidRequest,
            )),
        }
    }
    pub fn into_installing_info(self) -> Option<Model<InstallingInfo>> {
        match self.into_match() {
            PackageStateMatchModel::Installing(s) | PackageStateMatchModel::Restoring(s) => {
                Some(s.into_installing_info())
            }
            PackageStateMatchModel::Updating(s) => Some(s.into_installing_info()),
            PackageStateMatchModel::Installed(_) | PackageStateMatchModel::Removing(_) => None,
            PackageStateMatchModel::Error(_) => None,
        }
    }
    pub fn as_installing_info(&self) -> Option<&Model<InstallingInfo>> {
        match self.as_match() {
            PackageStateMatchModelRef::Installing(s) | PackageStateMatchModelRef::Restoring(s) => {
                Some(s.as_installing_info())
            }
            PackageStateMatchModelRef::Updating(s) => Some(s.as_installing_info()),
            PackageStateMatchModelRef::Installed(_) | PackageStateMatchModelRef::Removing(_) => {
                None
            }
            PackageStateMatchModelRef::Error(_) => None,
        }
    }
    pub fn as_installing_info_mut(&mut self) -> Option<&mut Model<InstallingInfo>> {
        match self.as_match_mut() {
            PackageStateMatchModelMut::Installing(s) | PackageStateMatchModelMut::Restoring(s) => {
                Some(s.as_installing_info_mut())
            }
            PackageStateMatchModelMut::Updating(s) => Some(s.as_installing_info_mut()),
            PackageStateMatchModelMut::Installed(_) | PackageStateMatchModelMut::Removing(_) => {
                None
            }
            PackageStateMatchModelMut::Error(_) => None,
        }
    }
    pub fn into_manifest(self, preference: ManifestPreference) -> Model<Manifest> {
        match self.into_match() {
            PackageStateMatchModel::Installing(s) | PackageStateMatchModel::Restoring(s) => {
                s.into_installing_info().into_new_manifest()
            }
            PackageStateMatchModel::Updating(s) if preference == ManifestPreference::Old => {
                s.into_manifest()
            }
            PackageStateMatchModel::Updating(s) => s.into_installing_info().into_new_manifest(),
            PackageStateMatchModel::Installed(s) | PackageStateMatchModel::Removing(s) => {
                s.into_manifest()
            }
            PackageStateMatchModel::Error(_) => Value::Null.into(),
        }
    }
    pub fn as_manifest(&self, preference: ManifestPreference) -> &Model<Manifest> {
        match self.as_match() {
            PackageStateMatchModelRef::Installing(s) | PackageStateMatchModelRef::Restoring(s) => {
                s.as_installing_info().as_new_manifest()
            }
            PackageStateMatchModelRef::Updating(s) if preference == ManifestPreference::Old => {
                s.as_manifest()
            }
            PackageStateMatchModelRef::Updating(s) => s.as_installing_info().as_new_manifest(),
            PackageStateMatchModelRef::Installed(s) | PackageStateMatchModelRef::Removing(s) => {
                s.as_manifest()
            }
            PackageStateMatchModelRef::Error(_) => (&Value::Null).into(),
        }
    }
    pub fn as_manifest_mut(
        &mut self,
        preference: ManifestPreference,
    ) -> Result<&mut Model<Manifest>, Error> {
        Ok(match self.as_match_mut() {
            PackageStateMatchModelMut::Installing(s) | PackageStateMatchModelMut::Restoring(s) => {
                s.as_installing_info_mut().as_new_manifest_mut()
            }
            PackageStateMatchModelMut::Updating(s) if preference == ManifestPreference::Old => {
                s.as_manifest_mut()
            }
            PackageStateMatchModelMut::Updating(s) => {
                s.as_installing_info_mut().as_new_manifest_mut()
            }
            PackageStateMatchModelMut::Installed(s) | PackageStateMatchModelMut::Removing(s) => {
                s.as_manifest_mut()
            }
            PackageStateMatchModelMut::Error(_) => {
                return Err(Error::new(
                    eyre!("could not determine package state to get manifest"),
                    ErrorKind::Database,
                ))
            }
        })
    }
}

#[derive(Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct InstallingState {
    pub installing_info: InstallingInfo,
}

#[derive(Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct UpdatingState {
    pub manifest: Manifest,
    pub installing_info: InstallingInfo,
}

#[derive(Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct InstalledState {
    pub manifest: Manifest,
}

#[derive(Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct InstallingInfo {
    pub new_manifest: Manifest,
    pub progress: FullProgress,
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "kebab-case")]
pub enum AllowedStatuses {
    OnlyRunning,
    OnlyStopped,
    Any,
}

#[derive(Clone, Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
pub struct ActionMetadata {
    pub name: String,
    pub description: String,
    pub warning: Option<String>,
    #[serde(default)]
    pub visibility: ActionVisibility,
    pub allowed_statuses: AllowedStatuses,
    pub has_input: bool,
    pub group: Option<String>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "kebab-case")]
#[serde(rename_all_fields = "camelCase")]
pub enum ActionVisibility {
    Hidden,
    Disabled(String),
    Enabled,
}
impl Default for ActionVisibility {
    fn default() -> Self {
        Self::Enabled
    }
}

#[derive(Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct PackageDataEntry {
    pub state_info: PackageState,
    pub data_version: Option<VersionString>,
    pub status: MainStatus,
    #[ts(type = "string | null")]
    pub registry: Option<Url>,
    #[ts(type = "string")]
    pub developer_key: Pem<ed25519_dalek::VerifyingKey>,
    pub icon: DataUrl<'static>,
    #[ts(type = "string | null")]
    pub last_backup: Option<DateTime<Utc>>,
    pub current_dependencies: CurrentDependencies,
    pub actions: BTreeMap<ActionId, ActionMetadata>,
    #[ts(as = "BTreeMap::<String, ActionRequestEntry>")]
    pub requested_actions: BTreeMap<ReplayId, ActionRequestEntry>,
    pub service_interfaces: BTreeMap<ServiceInterfaceId, ServiceInterface>,
    pub hosts: Hosts,
    #[ts(type = "string[]")]
    pub store_exposed_dependents: Vec<JsonPointer>,
}
impl AsRef<PackageDataEntry> for PackageDataEntry {
    fn as_ref(&self) -> &PackageDataEntry {
        self
    }
}

#[derive(Debug, Clone, Default, Deserialize, Serialize, TS)]
#[ts(export)]
pub struct CurrentDependencies(pub BTreeMap<PackageId, CurrentDependencyInfo>);
impl CurrentDependencies {
    pub fn map(
        mut self,
        transform: impl Fn(
            BTreeMap<PackageId, CurrentDependencyInfo>,
        ) -> BTreeMap<PackageId, CurrentDependencyInfo>,
    ) -> Self {
        self.0 = transform(self.0);
        self
    }
}
impl Map for CurrentDependencies {
    type Key = PackageId;
    type Value = CurrentDependencyInfo;
    fn key_str(key: &Self::Key) -> Result<impl AsRef<str>, Error> {
        Ok(key)
    }
    fn key_string(key: &Self::Key) -> Result<InternedString, Error> {
        Ok(key.clone().into())
    }
}

#[derive(Clone, Debug, Deserialize, Serialize, TS, HasModel)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
pub struct CurrentDependencyInfo {
    #[ts(type = "string | null")]
    pub title: Option<InternedString>,
    pub icon: Option<DataUrl<'static>>,
    #[serde(flatten)]
    pub kind: CurrentDependencyKind,
    #[ts(type = "string")]
    pub version_range: VersionRange,
}

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "kebab-case")]
#[serde(tag = "kind")]
pub enum CurrentDependencyKind {
    Exists,
    #[serde(rename_all = "camelCase")]
    Running {
        #[serde(default)]
        #[ts(type = "string[]")]
        health_checks: BTreeSet<HealthCheckId>,
    },
}

#[derive(Clone, Debug, Deserialize, Serialize, TS, HasModel)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
#[model = "Model<Self>"]
pub struct ActionRequestEntry {
    pub request: ActionRequest,
    pub active: bool,
}

#[derive(Clone, Debug, Deserialize, Serialize, TS, HasModel)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
#[model = "Model<Self>"]
pub struct ActionRequest {
    pub package_id: PackageId,
    pub action_id: ActionId,
    #[serde(default)]
    pub severity: ActionSeverity,
    #[ts(optional)]
    pub reason: Option<String>,
    #[ts(optional)]
    pub when: Option<ActionRequestTrigger>,
    #[ts(optional)]
    pub input: Option<ActionRequestInput>,
}

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "kebab-case")]
#[ts(export)]
pub enum ActionSeverity {
    Critical,
    Important,
}
impl Default for ActionSeverity {
    fn default() -> Self {
        ActionSeverity::Important
    }
}

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct ActionRequestTrigger {
    #[serde(default)]
    pub once: bool,
    pub condition: ActionRequestCondition,
}

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "kebab-case")]
#[ts(export)]
pub enum ActionRequestCondition {
    InputNotMatches,
}

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "kebab-case")]
#[serde(tag = "kind")]
pub enum ActionRequestInput {
    Partial {
        #[ts(type = "Record<string, unknown>")]
        value: Value,
    },
}
impl ActionRequestInput {
    pub fn matches(&self, input: Option<&Value>) -> bool {
        match self {
            Self::Partial { value } => match input {
                None => false,
                Some(full) => is_partial_of(value, full),
            },
        }
    }
}

#[derive(Debug, Default, Deserialize, Serialize)]
pub struct InterfaceAddressMap(pub BTreeMap<HostId, InterfaceAddresses>);
impl Map for InterfaceAddressMap {
    type Key = HostId;
    type Value = InterfaceAddresses;
    fn key_str(key: &Self::Key) -> Result<impl AsRef<str>, Error> {
        Ok(key)
    }
    fn key_string(key: &Self::Key) -> Result<InternedString, Error> {
        Ok(key.clone().into())
    }
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
pub struct InterfaceAddresses {
    pub tor_address: Option<String>,
    pub lan_address: Option<String>,
}
