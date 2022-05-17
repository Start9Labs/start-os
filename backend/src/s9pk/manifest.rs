use std::borrow::Borrow;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use color_eyre::eyre::eyre;
use patch_db::HasModel;
use serde::{Deserialize, Serialize, Serializer};
use url::Url;

use crate::action::Actions;
use crate::backup::BackupActions;
use crate::config::action::ConfigActions;
use crate::dependencies::Dependencies;
use crate::id::{Id, InvalidId, SYSTEM_ID};
use crate::migration::Migrations;
use crate::net::interface::Interfaces;
use crate::procedure::PackageProcedure;
use crate::status::health_check::HealthChecks;
use crate::util::Version;
use crate::version::{Current, VersionT};
use crate::volume::Volumes;
use crate::Error;

pub const SYSTEM_PACKAGE_ID: PackageId<&'static str> = PackageId(SYSTEM_ID);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PackageId<S: AsRef<str> = String>(Id<S>);
impl<'a> PackageId<&'a str> {
    pub fn owned(&self) -> PackageId {
        PackageId(self.0.owned())
    }
}
impl FromStr for PackageId {
    type Err = InvalidId;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(PackageId(Id::try_from(s.to_owned())?))
    }
}
impl From<PackageId> for String {
    fn from(value: PackageId) -> Self {
        value.0.into()
    }
}
impl<S: AsRef<str>> From<Id<S>> for PackageId<S> {
    fn from(id: Id<S>) -> Self {
        PackageId(id)
    }
}
impl<S: AsRef<str>> std::ops::Deref for PackageId<S> {
    type Target = S;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
impl<S: AsRef<str>> AsRef<PackageId<S>> for PackageId<S> {
    fn as_ref(&self) -> &PackageId<S> {
        self
    }
}
impl<S: AsRef<str>> std::fmt::Display for PackageId<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.0)
    }
}
impl<S: AsRef<str>> AsRef<str> for PackageId<S> {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}
impl<S: AsRef<str>> Borrow<str> for PackageId<S> {
    fn borrow(&self) -> &str {
        self.0.as_ref()
    }
}
impl<S: AsRef<str>> AsRef<Path> for PackageId<S> {
    fn as_ref(&self) -> &Path {
        self.0.as_ref().as_ref()
    }
}
impl<'de, S> Deserialize<'de> for PackageId<S>
where
    S: AsRef<str>,
    Id<S>: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        Ok(PackageId(Deserialize::deserialize(deserializer)?))
    }
}
impl<S> Serialize for PackageId<S>
where
    S: AsRef<str>,
{
    fn serialize<Ser>(&self, serializer: Ser) -> Result<Ser::Ok, Ser::Error>
    where
        Ser: Serializer,
    {
        Serialize::serialize(&self.0, serializer)
    }
}

fn current_version() -> Version {
    Current::new().semver().into()
}

#[derive(Clone, Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct Manifest {
    #[serde(default = "current_version")]
    pub eos_version: Version,
    pub id: PackageId,
    pub title: String,
    #[model]
    pub version: Version,
    pub description: Description,
    #[serde(default)]
    pub assets: Assets,
    #[serde(default)]
    pub build: Option<Vec<String>>,
    pub release_notes: String,
    pub license: String, // type of license
    pub wrapper_repo: Url,
    pub upstream_repo: Url,
    pub support_site: Option<Url>,
    pub marketing_site: Option<Url>,
    pub donation_url: Option<Url>,
    #[serde(default)]
    pub alerts: Alerts,
    #[model]
    pub main: PackageProcedure,
    pub health_checks: HealthChecks,
    #[model]
    pub config: Option<ConfigActions>,
    #[model]
    pub properties: Option<PackageProcedure>,
    #[model]
    pub volumes: Volumes,
    // #[serde(default)]
    pub interfaces: Interfaces,
    // #[serde(default)]
    #[model]
    pub backup: BackupActions,
    #[serde(default)]
    #[model]
    pub migrations: Migrations,
    #[serde(default)]
    pub actions: Actions,
    // #[serde(default)]
    // pub permissions: Permissions,
    #[serde(default)]
    #[model]
    pub dependencies: Dependencies,
}

impl Manifest {
    pub fn package_procedures(&self) -> impl Iterator<Item = &PackageProcedure> {
        use std::iter::once;
        let main = once(&self.main);
        let cfg_get = self.config.as_ref().map(|a| &a.get).into_iter();
        let cfg_set = self.config.as_ref().map(|a| &a.set).into_iter();
        let props = self.properties.iter();
        let backups = vec![&self.backup.create, &self.backup.restore].into_iter();
        let migrations = self
            .migrations
            .to
            .values()
            .chain(self.migrations.from.values());
        let actions = self.actions.0.values().map(|a| &a.implementation);
        main.chain(cfg_get)
            .chain(cfg_set)
            .chain(props)
            .chain(backups)
            .chain(migrations)
            .chain(actions)
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct Assets {
    #[serde(default)]
    pub license: Option<PathBuf>,
    #[serde(default)]
    pub instructions: Option<PathBuf>,
    #[serde(default)]
    pub icon: Option<PathBuf>,
    #[serde(default)]
    pub docker_images: Option<PathBuf>,
    #[serde(default)]
    pub assets: Option<PathBuf>,
    #[serde(default)]
    pub scripts: Option<PathBuf>,
}
impl Assets {
    pub fn license_path(&self) -> &Path {
        self.license
            .as_ref()
            .map(|a| a.as_path())
            .unwrap_or(Path::new("LICENSE.md"))
    }
    pub fn instructions_path(&self) -> &Path {
        self.instructions
            .as_ref()
            .map(|a| a.as_path())
            .unwrap_or(Path::new("INSTRUCTIONS.md"))
    }
    pub fn icon_path(&self) -> &Path {
        self.icon
            .as_ref()
            .map(|a| a.as_path())
            .unwrap_or(Path::new("icon.png"))
    }
    pub fn icon_type(&self) -> &str {
        self.icon
            .as_ref()
            .and_then(|icon| icon.extension())
            .and_then(|ext| ext.to_str())
            .unwrap_or("png")
    }
    pub fn docker_images_path(&self) -> &Path {
        self.docker_images
            .as_ref()
            .map(|a| a.as_path())
            .unwrap_or(Path::new("image.tar"))
    }
    pub fn assets_path(&self) -> &Path {
        self.assets
            .as_ref()
            .map(|a| a.as_path())
            .unwrap_or(Path::new("assets"))
    }
    pub fn scripts_path(&self) -> &Path {
        self.scripts
            .as_ref()
            .map(|a| a.as_path())
            .unwrap_or(Path::new("scripts"))
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Description {
    pub short: String,
    pub long: String,
}
impl Description {
    pub fn validate(&self) -> Result<(), Error> {
        if self.short.chars().skip(160).next().is_some() {
            return Err(Error::new(
                eyre!("Short description must be 160 characters or less."),
                crate::ErrorKind::ValidateS9pk,
            ));
        }
        if self.long.chars().skip(5000).next().is_some() {
            return Err(Error::new(
                eyre!("Long description must be 5000 characters or less."),
                crate::ErrorKind::ValidateS9pk,
            ));
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct Alerts {
    pub install: Option<String>,
    pub uninstall: Option<String>,
    pub restore: Option<String>,
    pub start: Option<String>,
    pub stop: Option<String>,
}
