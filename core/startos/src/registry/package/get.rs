use std::collections::{BTreeMap, BTreeSet};

use clap::{Parser, ValueEnum};
use emver::{Version, VersionRange};
use imbl_value::InternedString;
use itertools::Itertools;
use models::PackageId;
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::prelude::*;
use crate::registry::context::RegistryContext;
use crate::registry::device_info::DeviceInfo;
use crate::registry::os::version;
use crate::registry::package::index::{PackageIndex, PackageVersionInfo};
use crate::util::serde::{display_serializable, WithIoFormat};
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
impl PackageInfoShort {
    pub fn extend_table(&self, table: &mut prettytable::Table) {
        use prettytable::*;

        table.add_row(row![bc -> "RELEASE NOTES"]);
        todo!()
    }
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
    #[ts(type = "string[]")]
    pub categories: BTreeSet<InternedString>,
    pub best: BTreeMap<VersionString, PackageVersionInfo>,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[ts(optional)]
    pub other_versions: Option<BTreeMap<VersionString, PackageInfoShort>>,
}
impl GetPackageResponse {
    pub fn tables(&self) -> prettytable::Table {
        use prettytable::*;

        let mut table = Table::new();

        let empty = BTreeMap::new();
        let versions: BTreeSet<_> = self
            .best
            .keys()
            .chain(self.other_versions.as_ref().unwrap_or(&empty).keys())
            .collect();

        for version in versions {
            if let Some(info) = self.best.get(version) {
                table.add_row(row![bcFg => &format!("v{version}")]);
                info.extend_table(&mut table);
            } else if let Some(info) = self.other_versions.as_ref().and_then(|o| o.get(version)) {
                table.add_row(row![bc => &format!("v{version}")]);
                info.extend_table(&mut table);
            }
        }

        table
    }
}

#[derive(Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct GetPackageResponseFull {
    #[ts(type = "string[]")]
    pub categories: BTreeSet<InternedString>,
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
        ..
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
                        && device_info
                            .as_ref()
                            .map_or(Ok(true), |device_info| info.works_for_device(device_info))?
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

pub async fn get_package(ctx: RegistryContext, params: GetPackageParams) -> Result<Value, Error> {
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
        let categories = peek
            .as_index()
            .as_package()
            .as_packages()
            .as_idx(&id)
            .map(|p| p.as_categories().de())
            .transpose()?
            .unwrap_or_default();
        let best = best
            .remove(&id)
            .unwrap_or_default()
            .into_iter()
            .map(|(k, v)| v.de().map(|v| (k, v)))
            .try_collect()?;
        let other = other.remove(&id).unwrap_or_default();
        match params.other_versions {
            None => to_value(&GetPackageResponse {
                categories,
                best,
                other_versions: None,
            }),
            Some(PackageDetailLevel::Short) => to_value(&GetPackageResponse {
                categories,
                best,
                other_versions: Some(
                    other
                        .into_iter()
                        .map(|(k, v)| from_value(v.as_value().clone()).map(|v| (k, v)))
                        .try_collect()?,
                ),
            }),
            Some(PackageDetailLevel::Full) => to_value(&GetPackageResponseFull {
                categories,
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
                        let categories = peek
                            .as_index()
                            .as_package()
                            .as_packages()
                            .as_idx(&id)
                            .map(|p| p.as_categories().de())
                            .transpose()?
                            .unwrap_or_default();
                        Ok::<_, Error>((
                            id,
                            GetPackageResponse {
                                categories,
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
                        let categories = peek
                            .as_index()
                            .as_package()
                            .as_packages()
                            .as_idx(&id)
                            .map(|p| p.as_categories().de())
                            .transpose()?
                            .unwrap_or_default();
                        let other = other.remove(&id).unwrap_or_default();
                        Ok::<_, Error>((
                            id,
                            GetPackageResponse {
                                categories,
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
                        let categories = peek
                            .as_index()
                            .as_package()
                            .as_packages()
                            .as_idx(&id)
                            .map(|p| p.as_categories().de())
                            .transpose()?
                            .unwrap_or_default();
                        let other = other.remove(&id).unwrap_or_default();
                        Ok::<_, Error>((
                            id,
                            GetPackageResponseFull {
                                categories,
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

pub fn display_package_info(
    params: WithIoFormat<GetPackageParams>,
    info: Value,
) -> Result<(), Error> {
    use prettytable::*;

    if let Some(format) = params.format {
        display_serializable(format, info);
        return Ok(());
    }

    // table.print_tty(false).unwrap();
    todo!()
}
