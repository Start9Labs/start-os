use std::collections::BTreeMap;
use std::ops::Deref;

use clap::{Parser, ValueEnum};
use emver::{Version, VersionRange};
use itertools::Itertools;
use models::PackageId;
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::prelude::*;
use crate::registry::context::RegistryContext;
use crate::registry::device_info::DeviceInfo;
use crate::registry::package::index::{PackageIndex, PackageVersionInfo};
use crate::util::VersionString;

#[derive(
    Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Deserialize, Serialize, TS, ValueEnum,
)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub enum PackageDetailLevel {
    Short,
    Full,
}
impl Default for PackageDetailLevel {
    fn default() -> Self {
        Self::Short
    }
}

#[derive(Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct PackageInfoShort {
    pub release_notes: String,
}

#[derive(Debug, Deserialize, Serialize, TS, Parser)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
#[ts(export)]
pub struct GetPackageParams {
    pub id: Option<PackageId>,
    #[ts(type = "string | null")]
    pub version: Option<VersionRange>,
    #[ts(type = "string | null")]
    pub source_version: Option<Version>,
    #[ts(skip)]
    #[arg(skip)]
    #[serde(rename = "__device_info")]
    pub device_info: Option<DeviceInfo>,
    pub other_versions: Option<PackageDetailLevel>,
}

#[derive(Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct GetPackageResponse {
    pub best: BTreeMap<VersionString, PackageVersionInfo>,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[ts(optional)]
    pub other_versions: Option<BTreeMap<VersionString, PackageInfoShort>>,
}

#[derive(Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct GetPackageResponseFull {
    pub best: BTreeMap<VersionString, PackageVersionInfo>,
    pub other_versions: BTreeMap<VersionString, PackageVersionInfo>,
}

pub type GetPackagesResponse = BTreeMap<PackageId, GetPackageResponse>;
pub type GetPackagesResponseFull = BTreeMap<PackageId, GetPackageResponseFull>;

fn get_matching_models<'a>(
    db: &'a Model<PackageIndex>,
    GetPackageParams {
        id,
        version,
        source_version,
        device_info,
        other_versions,
    }: &GetPackageParams,
) -> Result<Vec<(PackageId, Version, &'a Model<PackageVersionInfo>)>, Error> {
    if let Some(id) = id {
        if let Some(pkg) = db.as_packages().as_idx(id) {
            vec![(id.clone(), pkg)]
        } else {
            vec![]
        }
    } else {
        db.as_packages().as_entries()?
    }
    .iter()
    .map(|(k, v)| {
        Ok(v.as_versions()
            .as_entries()?
            .into_iter()
            .map(|(v, info)| {
                Ok::<_, Error>(
                    if version
                        .as_ref()
                        .map_or(true, |version| v.satisfies(version))
                        && source_version.as_ref().map_or(Ok(true), |source_version| {
                            Ok::<_, Error>(
                                source_version.satisfies(
                                    &info
                                        .as_source_version()
                                        .de()?
                                        .unwrap_or(VersionRange::any()),
                                ),
                            )
                        })?
                    {
                        Some((k.clone(), Version::from(v), info))
                    } else {
                        None
                    },
                )
            })
            .flatten_ok())
    })
    .flatten_ok()
    .map(|res| res.and_then(|a| a))
    .collect()
}

pub async fn get_package_rpc(
    ctx: RegistryContext,
    params: GetPackageParams,
) -> Result<Value, Error> {
    use patch_db::ModelExt;

    let peek = ctx.db.peek().await;
    let mut best: BTreeMap<PackageId, BTreeMap<VersionString, &Model<PackageVersionInfo>>> =
        Default::default();
    let mut other: BTreeMap<PackageId, BTreeMap<VersionString, &Model<PackageVersionInfo>>> =
        Default::default();
    for (id, version, info) in get_matching_models(&peek.as_index().as_package(), &params)? {
        let mut package_best = best.remove(&id).unwrap_or_default();
        let mut package_other = other.remove(&id).unwrap_or_default();
        for worse_version in package_best
            .keys()
            .filter(|k| ***k < version)
            .cloned()
            .collect_vec()
        {
            if let Some(info) = package_best.remove(&worse_version) {
                package_other.insert(worse_version, info);
            }
        }
        if package_best.keys().all(|k| !(**k > version)) {
            package_best.insert(version.into(), info);
        }
        best.insert(id.clone(), package_best);
        if params.other_versions.is_some() {
            other.insert(id.clone(), package_other);
        }
    }
    if let Some(id) = params.id {
        let best = best
            .remove(&id)
            .unwrap_or_default()
            .into_iter()
            .map(|(k, v)| v.de().map(|v| (k, v)))
            .try_collect()?;
        let other = other.remove(&id).unwrap_or_default();
        match params.other_versions {
            None => to_value(&GetPackageResponse {
                best,
                other_versions: None,
            }),
            Some(PackageDetailLevel::Short) => to_value(&GetPackageResponse {
                best,
                other_versions: Some(
                    other
                        .into_iter()
                        .map(|(k, v)| from_value(v.as_value().clone()).map(|v| (k, v)))
                        .try_collect()?,
                ),
            }),
            Some(PackageDetailLevel::Full) => to_value(&GetPackageResponseFull {
                best,
                other_versions: other
                    .into_iter()
                    .map(|(k, v)| v.de().map(|v| (k, v)))
                    .try_collect()?,
            }),
        }
    } else {
        match params.other_versions {
            None => to_value(
                &best
                    .into_iter()
                    .map(|(id, best)| {
                        Ok::<_, Error>((
                            id,
                            GetPackageResponse {
                                best: best
                                    .into_iter()
                                    .map(|(k, v)| v.de().map(|v| (k, v)))
                                    .try_collect()?,
                                other_versions: None,
                            },
                        ))
                    })
                    .try_collect::<_, GetPackagesResponse, _>()?,
            ),
            Some(PackageDetailLevel::Short) => to_value(
                &best
                    .into_iter()
                    .map(|(id, best)| {
                        let other = other.remove(&id).unwrap_or_default();
                        Ok::<_, Error>((
                            id,
                            GetPackageResponse {
                                best: best
                                    .into_iter()
                                    .map(|(k, v)| v.de().map(|v| (k, v)))
                                    .try_collect()?,
                                other_versions: Some(
                                    other
                                        .into_iter()
                                        .map(|(k, v)| {
                                            from_value(v.as_value().clone()).map(|v| (k, v))
                                        })
                                        .try_collect()?,
                                ),
                            },
                        ))
                    })
                    .try_collect::<_, GetPackagesResponse, _>()?,
            ),
            Some(PackageDetailLevel::Full) => to_value(
                &best
                    .into_iter()
                    .map(|(id, best)| {
                        let other = other.remove(&id).unwrap_or_default();
                        Ok::<_, Error>((
                            id,
                            GetPackageResponseFull {
                                best: best
                                    .into_iter()
                                    .map(|(k, v)| v.de().map(|v| (k, v)))
                                    .try_collect()?,
                                other_versions: other
                                    .into_iter()
                                    .map(|(k, v)| v.de().map(|v| (k, v)))
                                    .try_collect()?,
                            },
                        ))
                    })
                    .try_collect::<_, GetPackagesResponseFull, _>()?,
            ),
        }
    }
}
