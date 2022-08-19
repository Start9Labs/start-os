use std::path::{Path, PathBuf};

use color_eyre::eyre::eyre;
pub use models::{PackageId, SYSTEM_PACKAGE_ID};
use patch_db::HasModel;
use serde::{Deserialize, Serialize};
use url::Url;

use crate::backup::BackupActions;
use crate::config::action::ConfigActions;
use crate::dependencies::Dependencies;
use crate::migration::Migrations;
use crate::net::interface::Interfaces;
use crate::procedure::PackageProcedure;
use crate::status::health_check::HealthChecks;
use crate::util::Version;
use crate::version::{Current, VersionT};
use crate::volume::Volumes;
use crate::Error;
use crate::{action::Actions, procedure::docker::DockerContainer};

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
    #[model]
    pub container: Option<DockerContainer>,
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
