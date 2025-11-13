use std::collections::BTreeMap;
use std::path::Path;

use imbl_value::InternedString;
use models::PackageId;
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::prelude::*;
use crate::util::PathOrUrl;
use crate::Error;

#[test]
fn export_bindings_dependencies() {
    use crate::service::effects::dependency::{CheckDependenciesResult, DependencyRequirement};

    const OUT_DIR: &str = "./bindings/dependencies";

    PackageId::export_all_to(OUT_DIR).unwrap();
    DependencyRequirement::export_all_to(OUT_DIR).unwrap();
    CheckDependenciesResult::export_all_to(OUT_DIR).unwrap();
}

#[derive(Clone, Debug, Default, Deserialize, Serialize, HasModel, TS)]
#[model = "Model<Self>"]
pub struct Dependencies(pub BTreeMap<PackageId, DepInfo>);
impl Map for Dependencies {
    type Key = PackageId;
    type Value = DepInfo;
    fn key_str(key: &Self::Key) -> Result<impl AsRef<str>, Error> {
        Ok(key)
    }
    fn key_string(key: &Self::Key) -> Result<imbl_value::InternedString, Error> {
        Ok(key.clone().into())
    }
}

#[derive(Clone, Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
pub struct DepInfo {
    pub description: Option<String>,
    pub optional: bool,
    #[serde(flatten)]
    pub metadata: Option<MetadataSrc>,
}
impl TS for DepInfo {
    type WithoutGenerics = Self;
    fn decl() -> String {
        format!("type {} = {}", Self::name(), Self::inline_flattened())
    }
    fn decl_concrete() -> String {
        Self::decl()
    }
    fn name() -> String {
        "DepInfo".into()
    }
    fn inline() -> String {
        "{ description: string | null, optional: boolean } & MetadataSrc".into()
    }
    fn inline_flattened() -> String {
        Self::inline()
    }
    fn visit_dependencies(v: &mut impl ts_rs::TypeVisitor)
    where
        Self: 'static,
    {
        v.visit::<MetadataSrc>()
    }
    fn output_path() -> Option<&'static std::path::Path> {
        Some(Path::new("DepInfo.ts"))
    }
}

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
pub enum MetadataSrc {
    Metadata(Metadata),
    S9pk(Option<PathOrUrl>), // backwards compatibility
}

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct Metadata {
    pub title: InternedString,
    pub icon: PathOrUrl,
}

#[derive(Clone, Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
pub struct DependencyMetadata {
    #[ts(type = "string")]
    pub title: InternedString,
}
