use std::borrow::Cow;
use std::path::Path;

use emver::{Version, VersionRange};
use linear_map::LinearMap;
use rand::SeedableRng;

use crate::config::{Config, ConfigRuleEntryWithSuggestions, ConfigSpec};
use crate::manifest::ManifestLatest;
use crate::Error;
use crate::ResultExt as _;

#[derive(Clone, Debug, Fail, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum DependencyError {
    NotInstalled, // "not-installed"
    NotRunning,   // "not-running"
    IncorrectVersion {
        expected: VersionRange,
        received: Version,
    }, // { "incorrect-version": { "expected": "0.1.0", "received": "^0.2.0" } }
    ConfigUnsatisfied(Vec<String>), // { "config-unsatisfied": ["Bitcoin Core must have pruning set to manual."] }
    PointerUpdateError(String), // { "pointer-update-error": "Bitcoin Core RPC Port must not be 18332" }
    Other(String),              // { "other": "Well fuck." }
}
impl std::fmt::Display for DependencyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use DependencyError::*;
        match self {
            NotInstalled => write!(f, "Not Installed"),
            NotRunning => write!(f, "Not Running"),
            IncorrectVersion { expected, received } => write!(
                f,
                "Incorrect Version: Expected {}, Received {}",
                expected, received
            ),
            ConfigUnsatisfied(rules) => {
                write!(f, "Configuration Rule(s) Violated: {}", rules.join(", "))
            }
            PointerUpdateError(e) => write!(f, "Pointer Update Caused {}", e),
            Other(e) => write!(f, "System Error: {}", e),
        }
    }
}

#[derive(Clone, Debug, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct TaggedDependencyError {
    pub dependency: String,
    pub error: DependencyError,
}
impl std::fmt::Display for TaggedDependencyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.dependency, self.error)
    }
}

#[derive(Clone, Debug, Default, serde::Deserialize, serde::Serialize)]
pub struct Dependencies(pub LinearMap<String, DepInfo>);

#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct DepInfo {
    pub version: VersionRange,
    pub optional: Option<String>,
    pub description: Option<String>,
    #[serde(default)]
    pub mount_public: bool,
    #[serde(default)]
    pub mount_shared: bool,
    #[serde(default)]
    pub config: Vec<ConfigRuleEntryWithSuggestions>,
}
impl DepInfo {
    pub async fn satisfied(
        &self,
        dependency_id: &str,
        dependency_config: Option<Config>, // fetch if none
        dependent_id: &str,
        dependent_config: &Config,
    ) -> Result<Result<(), DependencyError>, Error> {
        let info = if let Some(info) = crate::apps::list_info().await?.remove(dependency_id) {
            info
        } else {
            return Ok(Err(DependencyError::NotInstalled));
        };
        if !&info.version.satisfies(&self.version) {
            return Ok(Err(DependencyError::IncorrectVersion {
                expected: self.version.clone(),
                received: info.version.clone(),
            }));
        }
        let dependency_config = if let Some(cfg) = dependency_config {
            cfg
        } else {
            let app_config = crate::apps::config(dependency_id).await?;
            if let Some(cfg) = app_config.config {
                cfg
            } else {
                app_config
                    .spec
                    .gen(&mut rand::rngs::StdRng::from_entropy(), &None)
                    .unwrap_or_default()
            }
        };
        let mut errors = Vec::new();
        let mut cfgs = LinearMap::with_capacity(2);
        cfgs.insert(dependency_id, Cow::Borrowed(&dependency_config));
        cfgs.insert(dependent_id, Cow::Borrowed(dependent_config));
        for rule in self.config.iter() {
            if !(rule.entry.rule.compiled)(&dependency_config, &cfgs) {
                errors.push(rule.entry.description.clone());
            }
        }
        if !errors.is_empty() {
            return Ok(Err(DependencyError::ConfigUnsatisfied(errors)));
        }
        if crate::apps::status(dependency_id, false).await?.status
            != crate::apps::DockerStatus::Running
        {
            return Ok(Err(DependencyError::NotRunning));
        }
        Ok(Ok(()))
    }
}

#[derive(Debug, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct AppDepInfo {
    #[serde(flatten)]
    pub info: DepInfo,
    pub required: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<DependencyError>,
}

#[derive(Debug, Default, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct AppDependencies(pub LinearMap<String, AppDepInfo>);

pub async fn check_dependencies(
    manifest: ManifestLatest,
    dependent_config: &Config,
    dependent_config_spec: &ConfigSpec,
) -> Result<AppDependencies, Error> {
    let mut deps = AppDependencies::default();
    for (dependency_id, dependency_info) in manifest.dependencies.0.into_iter() {
        let required = dependency_info.optional.is_none()
            || dependent_config_spec.requires(&dependency_id, dependent_config);
        let error = dependency_info
            .satisfied(&dependency_id, None, &manifest.id, dependent_config)
            .await?
            .err();
        let app_dep_info = AppDepInfo {
            error,
            required,
            info: dependency_info,
        };
        deps.0.insert(dependency_id, app_dep_info);
    }
    Ok(deps)
}

pub async fn auto_configure(
    dependent: &str,
    dependency: &str,
    dry_run: bool,
) -> Result<crate::config::ConfigurationRes, Error> {
    let (dependent_config, mut dependency_config, manifest) = futures::try_join!(
        crate::apps::config_or_default(dependent),
        crate::apps::config_or_default(dependency),
        crate::apps::manifest(dependent)
    )?;
    let mut cfgs = LinearMap::new();
    cfgs.insert(dependent, Cow::Borrowed(&dependent_config));
    cfgs.insert(dependency, Cow::Owned(dependency_config.clone()));
    let dep_info = manifest
        .dependencies
        .0
        .get(dependency)
        .ok_or_else(|| failure::format_err!("{} Does Not Depend On {}", dependent, dependency))
        .no_code()?;
    for rule in &dep_info.config {
        if let Err(e) = rule.apply(dependency, &mut dependency_config, &mut cfgs) {
            log::warn!("Rule Unsatisfied After Applying Suggestions: {}", e);
        }
    }
    crate::config::configure(dependency, Some(dependency_config), None, dry_run).await
}

pub async fn update_binds(dependent_id: &str) -> Result<(), Error> {
    let dependent_manifest = crate::apps::manifest(dependent_id).await?;
    let dependency_manifests = futures::future::try_join_all(
        dependent_manifest
            .dependencies
            .0
            .into_iter()
            .filter(|(_, info)| info.mount_public || info.mount_shared)
            .map(|(id, info)| async {
                Ok::<_, Error>(if crate::apps::list_info().await?.contains_key(&id) {
                    let man = crate::apps::manifest(&id).await?;
                    Some((id, info, man))
                } else {
                    None
                })
            }),
    )
    .await?;
    // i just have a gut feeling this shouldn't be concurrent
    for (dependency_id, info, dependency_manifest) in
        dependency_manifests.into_iter().filter_map(|a| a)
    {
        match (dependency_manifest.public, info.mount_public) {
            (Some(public), true) => {
                let public_path = Path::new(crate::VOLUMES).join(&dependency_id).join(public);
                if let Ok(metadata) = tokio::fs::metadata(&public_path).await {
                    if metadata.is_dir() {
                        crate::disks::bind(
                            public_path,
                            Path::new(crate::VOLUMES)
                                .join(&dependent_id)
                                .join("start9")
                                .join("public")
                                .join(&dependency_id),
                            true,
                        )
                        .await?
                    }
                }
            }
            _ => (),
        }
        match (dependency_manifest.shared, info.mount_shared) {
            (Some(shared), true) => {
                let shared_path = Path::new(crate::VOLUMES)
                    .join(&dependency_id)
                    .join(shared)
                    .join(dependent_id); // namespaced by dependent
                tokio::fs::create_dir_all(&shared_path).await?;
                if let Ok(metadata) = tokio::fs::metadata(&shared_path).await {
                    if metadata.is_dir() {
                        crate::disks::bind(
                            shared_path,
                            Path::new(crate::VOLUMES)
                                .join(&dependent_id)
                                .join("start9")
                                .join("shared")
                                .join(&dependency_id),
                            false,
                        )
                        .await?
                    }
                }
            }
            _ => (),
        }
    }

    Ok(())
}
