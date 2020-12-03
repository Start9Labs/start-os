use std::path::Path;

use super::*;

const V0_1_1: emver::Version = emver::Version::new(0, 1, 1, 0);

pub struct Version;
#[async_trait]
impl VersionT for Version {
    type Previous = v0_1_0::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> &'static emver::Version {
        &V0_1_1
    }
    async fn up(&self) -> Result<(), Error> {
        log::info!("Update torrc");
        let mut outfile = crate::util::PersistencePath::from_ref("tor/torrc")
            .write(None)
            .await?;
        tokio::io::copy(
            &mut AsyncCompat(
                reqwest::get(&format!("{}/torrc?spec==0.1.1", &*crate::SYS_REGISTRY_URL))
                    .compat()
                    .await
                    .with_context(|e| format!("GET {}/torrc: {}", &*crate::SYS_REGISTRY_URL, e))
                    .with_code(crate::error::NETWORK_ERROR)?
                    .error_for_status()
                    .with_context(|e| format!("GET {}/torrc: {}", &*crate::SYS_REGISTRY_URL, e))
                    .with_code(crate::error::REGISTRY_ERROR)?
                    .bytes_stream()
                    .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))
                    .into_async_read(),
            ),
            outfile.as_mut(),
        )
        .await
        .with_code(crate::error::FILESYSTEM_ERROR)?;
        outfile.commit().await?;
        if !std::process::Command::new("docker")
            .arg("network")
            .arg("create")
            .arg("-d")
            .arg("bridge")
            .arg("--subnet=172.18.0.0/16")
            .arg("start9")
            .stdout(std::process::Stdio::null())
            .status()?
            .success()
        {
            log::warn!("Failed to Create Network")
        }

        match tokio::fs::remove_file(Path::new(crate::PERSISTENCE_DIR).join(crate::SERVICES_YAML))
            .await
        {
            Ok(_) => Ok(()),
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(()),
            Err(e) => Err(e),
        }
        .with_context(|e| format!("{}/{}: {}", crate::PERSISTENCE_DIR, crate::SERVICES_YAML, e))
        .with_code(crate::error::FILESYSTEM_ERROR)?;
        crate::tor::reload().await?;

        for app in crate::apps::list_info().await? {
            legacy::update::update(&app.0).await?;
        }

        Ok(())
    }
    async fn down(&self) -> Result<(), Error> {
        let mut outfile = crate::util::PersistencePath::from_ref("tor/torrc")
            .write(None)
            .await?;

        tokio::io::copy(
            &mut AsyncCompat(
                reqwest::get(&format!("{}/torrc?spec==0.1.0", &*crate::SYS_REGISTRY_URL))
                    .compat()
                    .await
                    .with_context(|e| format!("GET {}/torrc: {}", &*crate::SYS_REGISTRY_URL, e))
                    .no_code()?
                    .error_for_status()
                    .with_context(|e| format!("GET {}/torrc: {}", &*crate::SYS_REGISTRY_URL, e))
                    .no_code()?
                    .bytes_stream()
                    .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))
                    .into_async_read(),
            ),
            outfile.as_mut(),
        )
        .await
        .with_code(crate::error::FILESYSTEM_ERROR)?;
        outfile.commit().await?;

        for app in crate::apps::list_info().await? {
            legacy::remove::remove(&app.0, false).await?;
        }
        let tor_svcs = crate::util::PersistencePath::from_ref(crate::SERVICES_YAML).path();
        if tor_svcs.exists() {
            tokio::fs::remove_file(&tor_svcs)
                .await
                .with_context(|e| format!("{}: {}", tor_svcs.display(), e))
                .with_code(crate::error::FILESYSTEM_ERROR)?;
        }
        if !std::process::Command::new("docker")
            .arg("network")
            .arg("rm")
            .arg("start9")
            .stdout(std::process::Stdio::null())
            .status()?
            .success()
        {
            log::warn!("Failed to Remove Network");
        }

        Ok(())
    }
}

mod legacy {
    pub mod remove {
        use std::path::Path;

        use crate::Error;

        pub async fn remove(name: &str, purge: bool) -> Result<(), Error> {
            log::info!("Removing app from manifest.");
            crate::apps::remove(name).await?;
            log::info!("Stopping docker container.");
            if !tokio::process::Command::new("docker")
                .args(&["stop", name])
                .stdout(std::process::Stdio::null())
                .stderr(match log::max_level() {
                    log::LevelFilter::Error => std::process::Stdio::null(),
                    _ => std::process::Stdio::inherit(),
                })
                .status()
                .await?
                .success()
            {
                log::error!("Failed to Stop Docker Container");
            };
            log::info!("Removing docker container.");
            if !tokio::process::Command::new("docker")
                .args(&["rm", name])
                .stdout(std::process::Stdio::null())
                .stderr(match log::max_level() {
                    log::LevelFilter::Error => std::process::Stdio::null(),
                    _ => std::process::Stdio::inherit(),
                })
                .status()
                .await?
                .success()
            {
                log::error!("Failed to Remove Docker Container");
            };
            if purge {
                log::info!("Removing tor hidden service.");
                crate::tor::rm_svc(name).await?;
                log::info!("Removing app metadata.");
                std::fs::remove_dir_all(Path::new(crate::PERSISTENCE_DIR).join("apps").join(name))?;
                log::info!("Destroying mounted volume.");
                std::fs::remove_dir_all(Path::new(crate::VOLUMES).join(name))?;
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
                    3,
                    "Failed to Prune Docker Images"
                );
            };

            Ok(())
        }
    }
    pub mod update {
        use crate::Error;
        pub async fn update(name_version: &str) -> Result<(), Error> {
            let name = name_version
                .split("@")
                .next()
                .ok_or_else(|| failure::format_err!("invalid app id"))?;
            crate::install::download_name(name_version).await?;
            super::remove::remove(name, false).await?;
            crate::install::install_name(name_version, true).await?;
            let config = crate::apps::config(name).await?;
            if let Some(cfg) = config.config {
                if config.spec.matches(&cfg).is_ok() {
                    crate::apps::set_configured(name, true).await?;
                }
            }
            Ok(())
        }
    }
}
