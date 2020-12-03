use std::path::Path;

use super::*;

const V0_1_0: emver::Version = emver::Version::new(0, 1, 0, 0);

pub struct Version;
#[async_trait]
impl VersionT for Version {
    type Previous = ();
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> &'static emver::Version {
        &V0_1_0
    }
    async fn up(&self) -> Result<(), Error> {
        tokio::fs::create_dir_all(Path::new(crate::PERSISTENCE_DIR).join("tor")).await?;
        tokio::fs::create_dir_all(Path::new(crate::PERSISTENCE_DIR).join("apps")).await?;
        tokio::fs::create_dir_all(Path::new(crate::TMP_DIR).join("tor")).await?;
        tokio::fs::create_dir_all(Path::new(crate::TMP_DIR).join("apps")).await?;
        let mut outfile = legacy::util::PersistencePath::from_ref("tor/torrc")
            .write()
            .await?;
        tokio::io::copy(
            &mut AsyncCompat(
                reqwest::get(&format!("{}/torrc?spec==0.0.0", &*crate::SYS_REGISTRY_URL))
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
        legacy::tor::set_svc(
            "start9-agent",
            legacy::tor::Service {
                ports: vec![5959],
                hidden_service_version: Default::default(),
            },
        )
        .await
        .no_code()?;
        Ok(())
    }
    async fn down(&self) -> Result<(), Error> {
        Ok(())
    }
}

mod legacy {
    pub mod tor {
        use failure::{Error, ResultExt};
        use linear_map::LinearMap;
        use tokio::io::AsyncWriteExt;

        use crate::tor::HiddenServiceVersion;

        use super::util::PersistencePath;

        pub const ETC_TOR_RC: &'static str = "/etc/tor/torrc";
        pub const HIDDEN_SERVICE_DIR_ROOT: &'static str = "/var/lib/tor";

        #[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
        pub struct Service {
            pub ports: Vec<u16>,
            pub hidden_service_version: HiddenServiceVersion,
        }

        async fn services_map(path: &PersistencePath) -> Result<LinearMap<String, Service>, Error> {
            use crate::util::Apply;
            Ok(path
                .maybe_read()
                .await
                .transpose()?
                .map(crate::util::from_yaml_async_reader)
                .apply(futures::future::OptionFuture::from)
                .await
                .transpose()?
                .unwrap_or_else(LinearMap::new))
        }

        pub async fn write_services(
            hidden_services: &LinearMap<String, Service>,
        ) -> Result<(), Error> {
            tokio::fs::copy(crate::TOR_RC, ETC_TOR_RC)
                .await
                .with_context(|e| format!("{} -> {}: {}", crate::TOR_RC, ETC_TOR_RC, e))?;
            let mut f = tokio::fs::OpenOptions::new()
                .append(true)
                .open(ETC_TOR_RC)
                .await?;
            f.write("\n".as_bytes()).await?;
            for (name, service) in hidden_services {
                f.write("\n".as_bytes()).await?;
                f.write(format!("# HIDDEN SERVICE FOR {}\n", name).as_bytes())
                    .await?;
                f.write(
                    format!(
                        "HiddenServiceDir {}/app-{}/\n",
                        HIDDEN_SERVICE_DIR_ROOT, name
                    )
                    .as_bytes(),
                )
                .await?;
                f.write(format!("{}\n", service.hidden_service_version).as_bytes())
                    .await?;
                for port in &service.ports {
                    f.write(format!("HiddenServicePort {} 127.0.0.1:{}\n", port, port).as_bytes())
                        .await?;
                }
                f.write("\n".as_bytes()).await?;
            }
            Ok(())
        }

        pub async fn set_svc(name: &str, service: Service) -> Result<(), Error> {
            log::info!(
                "Adding Tor hidden service {} to {}.",
                name,
                crate::SERVICES_YAML
            );
            let path = PersistencePath::from_ref(crate::SERVICES_YAML);
            let mut hidden_services = services_map(&path).await?;
            hidden_services.insert(name.to_owned(), service);
            let mut services_yaml = path.write().await?;
            crate::util::to_yaml_async_writer(services_yaml.as_mut(), &hidden_services).await?;
            services_yaml.write_all("\n".as_bytes()).await?;
            services_yaml.commit().await?;
            log::info!("Adding Tor hidden service {} to {}.", name, ETC_TOR_RC);
            write_services(&hidden_services).await?;
            log::info!("Restarting Tor.");
            let svc_exit = std::process::Command::new("service")
                .args(&["tor", "restart"])
                .status()?;
            ensure!(
                svc_exit.success(),
                "Failed to Restart Tor: {}",
                svc_exit.code().unwrap_or(0)
            );
            Ok(())
        }
    }

    pub mod util {
        use std::path::{Path, PathBuf};
        use tokio::fs::File;

        use crate::Error;
        use crate::ResultExt as _;
        use failure::ResultExt as _;

        #[derive(Clone, Debug)]
        pub struct PersistencePath(PathBuf);
        impl PersistencePath {
            pub fn from_ref<P: AsRef<Path>>(p: P) -> Self {
                let path = p.as_ref();
                PersistencePath(if path.has_root() {
                    path.strip_prefix("/").unwrap().to_owned()
                } else {
                    path.to_owned()
                })
            }

            pub fn tmp(&self) -> PathBuf {
                Path::new(crate::TMP_DIR).join(&self.0)
            }

            pub fn path(&self) -> PathBuf {
                Path::new(crate::PERSISTENCE_DIR).join(&self.0)
            }

            pub async fn maybe_read(&self) -> Option<Result<File, Error>> {
                let path = self.path();
                if path.exists() {
                    Some(
                        File::open(&path)
                            .await
                            .with_context(|e| format!("{}: {}", path.display(), e))
                            .with_code(crate::error::FILESYSTEM_ERROR),
                    )
                } else {
                    None
                }
            }

            pub async fn write(&self) -> Result<PersistenceFile, Error> {
                let path = self.path();
                if let Some(parent) = path.parent() {
                    if !parent.exists() {
                        tokio::fs::create_dir_all(parent).await?;
                    }
                }
                Ok(if path.exists() {
                    let path = self.tmp();
                    if let Some(parent) = path.parent() {
                        if !parent.exists() {
                            tokio::fs::create_dir_all(parent).await?;
                        }
                    }
                    PersistenceFile::new(File::create(path).await?, Some(self.clone()))
                } else {
                    PersistenceFile::new(File::create(path).await?, None)
                })
            }
        }

        #[derive(Debug)]
        pub struct PersistenceFile {
            file: File,
            needs_commit: Option<PersistencePath>,
        }
        impl PersistenceFile {
            pub fn new(file: File, needs_commit: Option<PersistencePath>) -> Self {
                PersistenceFile { file, needs_commit }
            }
            /// Commits the file to the persistence directory.
            /// If this fails, the file was not saved.
            pub async fn commit(mut self) -> Result<(), Error> {
                if let Some(path) = self.needs_commit.take() {
                    tokio::fs::rename(path.tmp(), path.path())
                        .await
                        .with_context(|e| {
                            format!(
                                "{} -> {}: {}",
                                path.tmp().display(),
                                path.path().display(),
                                e
                            )
                        })
                        .with_code(crate::error::FILESYSTEM_ERROR)
                } else {
                    Ok(())
                }
            }
        }
        impl std::ops::Deref for PersistenceFile {
            type Target = File;

            fn deref(&self) -> &Self::Target {
                &self.file
            }
        }
        impl std::ops::DerefMut for PersistenceFile {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.file
            }
        }
        impl AsRef<File> for PersistenceFile {
            fn as_ref(&self) -> &File {
                &*self
            }
        }
        impl AsMut<File> for PersistenceFile {
            fn as_mut(&mut self) -> &mut File {
                &mut *self
            }
        }
        impl Drop for PersistenceFile {
            fn drop(&mut self) {
                if let Some(path) = &self.needs_commit {
                    log::warn!(
                        "{} was dropped without being committed.",
                        path.path().display()
                    );
                }
            }
        }
    }
}
