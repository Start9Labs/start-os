use failure::ResultExt as _;
use futures::future::{BoxFuture, FutureExt, OptionFuture};
use linear_map::{set::LinearSet, LinearMap};
use rand::SeedableRng;

use crate::dependencies::AppDependencies;
use crate::manifest::{Manifest, ManifestLatest};
use crate::util::Apply;
use crate::util::{from_yaml_async_reader, PersistencePath, YamlUpdateHandle};
use crate::Error;
use crate::ResultExt as _;

#[derive(Clone, Copy, Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum DockerStatus {
    Running,
    Stopped, // created || exited
    Paused,
    Restarting,
    Removing,
    Dead,
}

fn not(b: &bool) -> bool {
    !b
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct AppInfo {
    pub title: String,
    pub version: emver::Version,
    pub tor_address: Option<String>,
    pub configured: bool,
    #[serde(default)]
    #[serde(skip_serializing_if = "not")]
    pub recoverable: bool,
    #[serde(default)]
    #[serde(skip_serializing_if = "not")]
    pub needs_restart: bool,
}

#[derive(Clone, Debug, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct AppStatus {
    pub status: DockerStatus,
}

#[derive(Debug, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct AppConfig {
    pub spec: crate::config::ConfigSpec,
    pub rules: Vec<crate::config::ConfigRuleEntry>,
    pub config: Option<crate::config::Config>,
}

#[derive(Debug, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct AppInfoFull {
    #[serde(flatten)]
    pub info: AppInfo,
    #[serde(flatten)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub status: Option<AppStatus>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub manifest: Option<ManifestLatest>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub config: Option<AppConfig>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub dependencies: Option<AppDependencies>,
}

pub async fn list_info() -> Result<LinearMap<String, AppInfo>, Error> {
    let apps_path = PersistencePath::from_ref("apps.yaml");
    let mut f = match apps_path.maybe_read(false).await.transpose()? {
        Some(a) => a,
        None => return Ok(LinearMap::new()),
    };
    from_yaml_async_reader(&mut *f).await
}

pub async fn list_info_mut() -> Result<YamlUpdateHandle<LinearMap<String, AppInfo>>, Error> {
    let apps_path = PersistencePath::from_ref("apps.yaml");
    YamlUpdateHandle::new_or_default(apps_path).await
}

pub async fn add(id: &str, info: AppInfo) -> Result<(), failure::Error> {
    let mut apps = list_info_mut().await?;
    apps.insert(id.to_string(), info);
    apps.commit().await?;
    Ok(())
}

pub async fn set_configured(id: &str, configured: bool) -> Result<(), Error> {
    let mut apps = list_info_mut().await?;
    let mut app = apps
        .get_mut(id)
        .ok_or_else(|| failure::format_err!("App Not Installed: {}", id))
        .with_code(crate::error::NOT_FOUND)?;
    app.configured = configured;
    apps.commit().await?;
    Ok(())
}

pub async fn set_needs_restart(id: &str, needs_restart: bool) -> Result<(), Error> {
    let mut apps = list_info_mut().await?;
    let mut app = apps
        .get_mut(id)
        .ok_or_else(|| failure::format_err!("App Not Installed: {}", id))
        .with_code(crate::error::NOT_FOUND)?;
    app.needs_restart = needs_restart;
    apps.commit().await?;
    Ok(())
}

pub async fn set_recoverable(id: &str, recoverable: bool) -> Result<(), Error> {
    let mut apps = list_info_mut().await?;
    let mut app = apps
        .get_mut(id)
        .ok_or_else(|| failure::format_err!("App Not Installed: {}", id))
        .with_code(crate::error::NOT_FOUND)?;
    app.recoverable = recoverable;
    apps.commit().await?;
    Ok(())
}

pub async fn remove(id: &str) -> Result<(), failure::Error> {
    let mut apps = list_info_mut().await?;
    apps.remove(id);
    apps.commit().await?;
    Ok(())
}

pub async fn status(id: &str) -> Result<AppStatus, Error> {
    let output = std::process::Command::new("docker")
        .args(&["inspect", id, "--format", "{{.State.Status}}"])
        .stdout(std::process::Stdio::piped())
        .stderr(match log::max_level() {
            log::LevelFilter::Error => std::process::Stdio::null(),
            _ => std::process::Stdio::inherit(),
        })
        .spawn()?
        .wait_with_output()?;
    crate::ensure_code!(
        output.status.success(),
        crate::error::DOCKER_ERROR,
        "{}: Docker Error: {}",
        id,
        std::str::from_utf8(&output.stderr).no_code()?
    );
    let status = std::str::from_utf8(&output.stdout).no_code()?;
    Ok(AppStatus {
        status: match status.trim() {
            "running" => DockerStatus::Running,
            "restarting" => DockerStatus::Restarting,
            "removing" => DockerStatus::Removing,
            "dead" => DockerStatus::Dead,
            "created" | "exited" => DockerStatus::Stopped,
            "paused" => DockerStatus::Paused,
            _ => Err(format_err!("unknown status: {}", status))?,
        },
    })
}

pub async fn manifest(id: &str) -> Result<ManifestLatest, Error> {
    let manifest: Manifest = from_yaml_async_reader(
        &mut *PersistencePath::from_ref("apps")
            .join(id)
            .join("manifest.yaml")
            .read(false)
            .await?,
    )
    .await?;
    Ok(manifest.into_latest())
}

pub async fn config(id: &str) -> Result<AppConfig, Error> {
    let spec = PersistencePath::from_ref("apps")
        .join(id)
        .join("config_spec.yaml");
    let spec: crate::config::ConfigSpec =
        crate::util::from_yaml_async_reader(&mut *spec.read(false).await?)
            .await
            .no_code()?;
    let rules = PersistencePath::from_ref("apps")
        .join(id)
        .join("config_rules.yaml");
    let rules: Vec<crate::config::ConfigRuleEntry> =
        crate::util::from_yaml_async_reader(&mut *rules.read(false).await?)
            .await
            .no_code()?;
    let config = PersistencePath::from_ref("apps")
        .join(id)
        .join("config.yaml");
    let config: Option<crate::config::Config> = match config
        .maybe_read(false)
        .await
        .transpose()?
        .map(|mut f| async move { from_yaml_async_reader(&mut *f).await })
        .apply(OptionFuture::from)
        .await
    {
        Some(Ok(cfg)) => Some(cfg),
        #[cfg(not(feature = "production"))]
        Some(Err(e)) => return Err(e),
        _ => {
            let volume_config = std::path::Path::new(crate::VOLUMES)
                .join(id)
                .join("start9")
                .join("config.yaml");
            if volume_config.exists() {
                let cfg_path = config.path();
                tokio::fs::copy(&volume_config, &cfg_path)
                    .await
                    .with_context(|e| {
                        format!(
                            "{}: {} -> {}",
                            e,
                            volume_config.display(),
                            cfg_path.display()
                        )
                    })
                    .with_code(crate::error::FILESYSTEM_ERROR)?;
                let mut f = tokio::fs::File::open(&volume_config)
                    .await
                    .with_context(|e| format!("{}: {}", e, volume_config.display()))
                    .with_code(crate::error::FILESYSTEM_ERROR)?;
                match from_yaml_async_reader(&mut f).await {
                    Ok(a) => Some(a),
                    #[cfg(not(feature = "production"))]
                    Err(e) => return Err(e),
                    #[cfg(feature = "production")]
                    _ => None,
                }
            } else {
                None
            }
        }
    };
    Ok(AppConfig {
        spec,
        rules,
        config,
    })
}

pub async fn config_or_default(id: &str) -> Result<crate::config::Config, Error> {
    let config = config(id).await?;
    Ok(if let Some(config) = config.config {
        config
    } else {
        config
            .spec
            .gen(&mut rand::rngs::StdRng::from_entropy(), &None)
            .with_code(crate::error::CFG_SPEC_VIOLATION)?
    })
}

pub async fn info(id: &str) -> Result<AppInfo, Error> {
    list_info()
        .await
        .map_err(Error::from)?
        .get(id)
        .ok_or_else(|| Error::new(failure::format_err!("{} is not installed", id), Some(6)))
        .map(Clone::clone)
}

pub async fn info_full(
    id: &str,
    with_status: bool,
    with_manifest: bool,
    with_config: bool,
    with_dependencies: bool,
) -> Result<AppInfoFull, Error> {
    Ok(AppInfoFull {
        info: info(id).await?,
        status: if with_status {
            Some(status(id).await?)
        } else {
            None
        },
        manifest: if with_manifest {
            Some(manifest(id).await?)
        } else {
            None
        },
        config: if with_config {
            Some(config(id).await?)
        } else {
            None
        },
        dependencies: if with_dependencies {
            Some(dependencies(id, true).await?)
        } else {
            None
        },
    })
}

pub async fn dependencies(id_version: &str, local_only: bool) -> Result<AppDependencies, Error> {
    let mut id_version_iter = id_version.split("@");
    let id = id_version_iter.next().unwrap();
    let version_range = id_version_iter
        .next()
        .map(|a| a.parse::<emver::VersionRange>())
        .transpose()
        .with_context(|e| format!("Failed to Parse Version Requirement: {}", e))
        .no_code()?
        .unwrap_or_else(emver::VersionRange::any);
    let (manifest, config_info) = match list_info().await?.get(id) {
        Some(info) if info.version.satisfies(&version_range) => {
            futures::try_join!(manifest(id), config(id))?
        }
        _ if !local_only => futures::try_join!(
            crate::registry::manifest(id, &version_range),
            crate::registry::config(id, &version_range)
        )?,
        _ => {
            return Err(failure::format_err!("App Not Installed: {}", id))
                .with_code(crate::error::NOT_FOUND)
        }
    };
    let config = if let Some(cfg) = config_info.config {
        cfg
    } else {
        config_info
            .spec
            .gen(&mut rand::rngs::StdRng::from_entropy(), &None)
            .unwrap_or_default()
    };
    crate::dependencies::check_dependencies(manifest, &config, &config_info.spec).await
}

pub async fn dependents(id: &str, transitive: bool) -> Result<LinearSet<String>, Error> {
    pub fn dependents_rec<'a>(
        id: &'a str,
        transitive: bool,
        res: &'a mut LinearSet<String>,
    ) -> BoxFuture<'a, Result<(), Error>> {
        async move {
            for (app_id, _) in list_info().await? {
                let manifest = manifest(&app_id).await?;
                match manifest.dependencies.0.get(id) {
                    Some(info) if !res.contains(&app_id) => {
                        let config_info = config(&app_id).await?;
                        let config = if let Some(cfg) = config_info.config {
                            cfg
                        } else {
                            config_info
                                .spec
                                .gen(&mut rand::rngs::StdRng::from_entropy(), &None)
                                .unwrap_or_default()
                        };
                        if info.optional.is_none() || config_info.spec.requires(&id, &config) {
                            res.insert(app_id.clone());
                            if transitive {
                                dependents_rec(&app_id, true, res).await?;
                            }
                        }
                    }
                    _ => (),
                }
            }
            Ok(())
        }
        .boxed()
    }
    let mut res = LinearSet::new();
    dependents_rec(id, transitive, &mut res).await?;
    Ok(res)
}

pub async fn list(
    with_status: bool,
    with_manifest: bool,
    with_config: bool,
    with_dependencies: bool,
) -> Result<LinearMap<String, AppInfoFull>, Error> {
    let info = list_info().await?;
    futures::future::join_all(info.into_iter().map(move |(id, info)| async move {
        let (status, manifest, config, dependencies) = futures::try_join!(
            OptionFuture::from(if with_status { Some(status(&id)) } else { None })
                .map(Option::transpose),
            OptionFuture::from(if with_manifest {
                Some(manifest(&id))
            } else {
                None
            })
            .map(Option::transpose),
            OptionFuture::from(if with_config { Some(config(&id)) } else { None })
                .map(Option::transpose),
            OptionFuture::from(if with_dependencies {
                Some(dependencies(&id, true))
            } else {
                None
            })
            .map(Option::transpose)
        )?;
        Ok((
            id,
            AppInfoFull {
                info,
                status,
                manifest,
                config,
                dependencies,
            },
        ))
    }))
    .await
    .into_iter()
    .collect()
}

pub async fn print_instructions(id: &str) -> Result<(), Error> {
    if let Some(file) = PersistencePath::from_ref("apps")
        .join(id)
        .join("instructions.md")
        .maybe_read(false)
        .await
    {
        use tokio::io::AsyncWriteExt;

        let mut stdout = tokio::io::stdout();
        tokio::io::copy(&mut *file?, &mut stdout)
            .await
            .with_code(crate::error::FILESYSTEM_ERROR)?;
        stdout
            .flush()
            .await
            .with_code(crate::error::FILESYSTEM_ERROR)?;
        stdout
            .shutdown()
            .await
            .with_code(crate::error::FILESYSTEM_ERROR)?;
        Ok(())
    } else {
        Err(failure::format_err!("No Instructions: {}", id)).with_code(crate::error::NOT_FOUND)
    }
}
