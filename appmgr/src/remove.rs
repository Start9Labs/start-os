use std::path::Path;

use linear_map::LinearMap;

use crate::dependencies::{DependencyError, TaggedDependencyError};
use crate::Error;

pub async fn remove(
    name: &str,
    purge: bool,
    dry_run: bool,
) -> Result<LinearMap<String, TaggedDependencyError>, Error> {
    let manifest = crate::apps::manifest(name).await?;
    let mut res = LinearMap::new();
    crate::stop_dependents(name, dry_run, DependencyError::NotInstalled, &mut res).await?;
    if dry_run {
        return Ok(res);
    }
    let image_name = format!("start9/{}", name);
    log::info!("Removing app from manifest.");
    crate::apps::remove(name).await?;
    log::info!("Stopping docker container.");
    let res = crate::control::stop_app(name, false, false)
        .await
        .unwrap_or_else(|e| {
            log::error!("Error stopping app: {}", e);
            LinearMap::new()
        });
    log::info!("Removing docker container.");
    if !std::process::Command::new("docker")
        .args(&["rm", name])
        .stdout(std::process::Stdio::null())
        .stderr(match log::max_level() {
            log::LevelFilter::Error => std::process::Stdio::null(),
            _ => std::process::Stdio::inherit(),
        })
        .status()?
        .success()
    {
        log::error!("Failed to Remove Docker Container");
    };
    if !std::process::Command::new("docker")
        .args(&["rmi", &image_name])
        .stdout(std::process::Stdio::null())
        .stderr(match log::max_level() {
            log::LevelFilter::Error => std::process::Stdio::null(),
            _ => std::process::Stdio::inherit(),
        })
        .status()?
        .success()
    {
        log::error!("Failed to Remove Docker Image");
    };
    if purge {
        log::info!("Removing tor hidden service.");
        crate::tor::rm_svc(name).await?;
        log::info!("Removing app metadata.");
        tokio::fs::remove_dir_all(Path::new(crate::PERSISTENCE_DIR).join("apps").join(name))
            .await?;
        log::info!("Destroying mounted volume.");
        log::info!("Unbinding shared filesystem.");
        for (dep, info) in manifest.dependencies.0.iter() {
            if info.mount_public {
                crate::disks::unmount(
                    Path::new(crate::VOLUMES)
                        .join(name)
                        .join("start9")
                        .join("public")
                        .join(&dep),
                )
                .await?;
            }
            if info.mount_shared {
                if let Some(shared) = match crate::apps::manifest(dep).await {
                    Ok(man) => man.shared,
                    Err(e) => {
                        log::error!("Failed to Fetch Dependency Manifest: {}", e);
                        None
                    }
                } {
                    let path = Path::new(crate::VOLUMES)
                        .join(name)
                        .join("start9")
                        .join("shared")
                        .join(&dep);
                    if path.exists() {
                        crate::disks::unmount(&path).await?;
                    }
                    let path = Path::new(crate::VOLUMES).join(dep).join(&shared).join(name);
                    if path.exists() {
                        tokio::fs::remove_dir_all(
                            Path::new(crate::VOLUMES).join(dep).join(&shared).join(name),
                        )
                        .await?;
                    }
                }
            }
        }
        tokio::fs::remove_dir_all(Path::new(crate::VOLUMES).join(name)).await?;
        log::info!("Pruning unused docker images.");
        crate::ensure_code!(
            std::process::Command::new("docker")
                .args(&["image", "prune", "-a", "-f"])
                .stdout(std::process::Stdio::null())
                .stderr(match log::max_level() {
                    log::LevelFilter::Error => std::process::Stdio::null(),
                    _ => std::process::Stdio::inherit(),
                })
                .status()?
                .success(),
            crate::error::DOCKER_ERROR,
            "Failed to Prune Docker Images"
        );
    };

    Ok(res)
}
