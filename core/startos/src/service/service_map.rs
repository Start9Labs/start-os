use std::sync::Arc;
use std::{collections::BTreeMap, path::PathBuf};

use color_eyre::eyre::eyre;
use futures::future::BoxFuture;
use futures::{Future, FutureExt, TryFutureExt};
use tokio::fs::File;
use tokio::sync::{OnceCell, RwLock};
use tracing::instrument;

use crate::context::RpcContext;
use crate::db::model::{
    PackageDataEntry, PackageDataEntryInstalled, PackageDataEntryInstalling,
    PackageDataEntryUpdating, StaticFiles,
};
use crate::install::progress::{InstallProgress, InstallProgressTracker};
use crate::install::PKG_ARCHIVE_DIR;
use crate::notifications::NotificationLevel;
use crate::prelude::*;
use crate::s9pk::manifest::PackageId;
use crate::s9pk::merkle_archive::source::FileSource;
use crate::s9pk::S9pk;
use crate::service::start_stop::StartStop;
use crate::service::Service;
use crate::util::GeneralGuard;

pub type DownloadInstallFuture = BoxFuture<'static, Result<InstallFuture, Error>>;
pub type InstallFuture = BoxFuture<'static, Result<(), Error>>;

/// This is the structure to contain all the services
#[derive(Default)]
pub struct ServiceMap(RwLock<BTreeMap<PackageId, Arc<Service>>>);
impl ServiceMap {
    #[instrument(skip_all)]
    pub async fn init(&self, ctx: &RpcContext) -> Result<(), Error> {
        let mut res = BTreeMap::new();
        let s9pk_dir = ctx.datadir.join(PKG_ARCHIVE_DIR).join("installed");
        for (id, entry) in ctx.db.peek().await.as_package_data().as_entries()? {
            if let Err(e) = async {
                let s9pk = S9pk::open(s9pk_dir.join(&id).with_extension("s9pk")).await?;
                let id = s9pk.as_manifest().id.clone();
                if let Some(service) = Service::load(ctx.clone(), s9pk, entry).await? {
                    res.insert(id, Arc::new(service));
                }
                Ok::<_, Error>(())
            }
            .await
            {
                tracing::error!("Error loading installed package as service: {e}");
                tracing::debug!("{e:?}");
            }
        }
        *self.0.write().await = res;
        Ok(())
    }

    #[instrument(skip_all)]
    pub async fn install<S: FileSource>(
        &self,
        ctx: RpcContext,
        s9pk: S9pk<S>,
    ) -> Result<DownloadInstallFuture, Error> {
        let manifest = Arc::new(s9pk.as_manifest().clone());
        let id = manifest.id.clone();

        let install_progress = Arc::new(InstallProgress::new(s9pk.size()));

        let error = Arc::new(OnceCell::new());
        let cancel_hook = GeneralGuard::new({
            let manifest = manifest.clone();
            let cancel_error = error.clone();
            let cancel_ctx = ctx.clone();
            let cancel_id = id.clone();
            move || {
                tokio::spawn({
                    let manifest = manifest.clone();
                    async move {
                        let id = cancel_id;
                        match (
                            cancel_ctx
                                .db
                                .mutate(|db| {
                                    let pde = match db
                                        .as_package_data()
                                        .as_idx(&id)
                                        .map(|x| x.de())
                                        .transpose()?
                                    {
                                        Some(PackageDataEntry::Updating(
                                            PackageDataEntryUpdating {
                                                install_progress,
                                                installed,
                                                manifest,
                                                static_files,
                                            },
                                        )) => Some(PackageDataEntry::Installed(
                                            PackageDataEntryInstalled {
                                                manifest: installed.manifest.clone(),
                                                installed,
                                                static_files,
                                            },
                                        )),
                                        Some(PackageDataEntry::Installing(_)) => None,
                                        _ => return Ok::<bool, Error>(false),
                                    };
                                    if let Some(pde) = pde {
                                        db.as_package_data_mut().insert(&id, &pde)?;
                                    } else {
                                        db.as_package_data_mut().remove(&id)?;
                                    }
                                    Ok::<bool, Error>(true)
                                })
                                .await,
                            cancel_error.get(),
                        ) {
                            (Ok(false), _) => (),
                            (Ok(true), Some(e)) => {
                                let err_str = format!(
                                    "Install of {}@{} Failed: {}",
                                    manifest.id, manifest.version, e
                                );
                                tracing::error!("{}", err_str);
                                tracing::debug!("{:?}", e);
                                if let Err(e) = cancel_ctx
                                    .notification_manager
                                    .notify(
                                        cancel_ctx.db.clone(),
                                        Some(manifest.id.clone()),
                                        NotificationLevel::Error,
                                        String::from("Install Failed"),
                                        err_str,
                                        (),
                                        None,
                                    )
                                    .await
                                {
                                    tracing::error!("Failed to issue Notification: {}", e);
                                    tracing::debug!("{:?}", e);
                                }
                            }
                            _ => todo!(),
                        }
                    }
                })
            }
        });

        ctx.db
            .mutate({
                let manifest = manifest.clone();
                let id = id.clone();
                let install_progress = install_progress.clone();
                move |db| {
                    let pde = match db
                        .as_package_data()
                        .as_idx(&id)
                        .map(|x| x.de())
                        .transpose()?
                    {
                        Some(PackageDataEntry::Installed(PackageDataEntryInstalled {
                            installed,
                            static_files,
                            ..
                        })) => PackageDataEntry::Updating(PackageDataEntryUpdating {
                            install_progress,
                            installed,
                            manifest: (*manifest).clone(),
                            static_files,
                        }),
                        None => PackageDataEntry::Installing(PackageDataEntryInstalling {
                            install_progress,
                            static_files: StaticFiles::local(
                                &manifest.id,
                                &manifest.version,
                                todo!(),
                            ),
                            manifest: (*manifest).clone(),
                        }),
                        _ => {
                            return Err(Error::new(
                                eyre!("Cannot install over a package in a transient state"),
                                crate::ErrorKind::InvalidRequest,
                            ))
                        }
                    };
                    db.as_package_data_mut().insert(&manifest.id, &pde)
                }
            })
            .await?;

        Ok(async move {
            match async {
            let download_path = ctx
                .datadir
                .join(PKG_ARCHIVE_DIR)
                .join("downloading")
                .join(&id)
                .with_extension("s9pk");

            install_progress
                .track_download_during(ctx.db.clone(), &id, || async {
                    let mut progress_writer = InstallProgressTracker::new(
                        crate::util::io::create_file(&download_path).await?,
                        install_progress.clone(),
                    );
                    s9pk.serialize(&mut progress_writer, true).await?;
                    progress_writer.into_inner().sync_all().await?;
                    install_progress.download_complete();
                    Ok(())
                })
                .await?;

            let installed_path = ctx
                .datadir
                .join(PKG_ARCHIVE_DIR)
                .join("installed")
                .join(&id)
                .with_extension("s9pk");

            crate::util::io::rename(&download_path, &installed_path).await?;

            Ok::<PathBuf, Error>(installed_path)
        }.await {
            Ok(installed_path) => Ok(async move {
                if let Err(e) = async {
                    let s9pk = S9pk::open(&installed_path).await?;

                    let mut lock = ctx.services.0.write().await;
                    if let Some(service) = lock.get(&id).cloned() {
                        drop(lock);
                        service.update(s9pk).await?;
                        Ok::<_, Error>(())
                    } else {
                        let service = Arc::new(
                            Service::load(
                                ctx.clone(),
                                s9pk,
                                ctx.db
                                    .peek()
                                    .await
                                    .as_package_data()
                                    .as_idx(&id)
                                    .or_not_found(&id)?,
                            )
                            .await?.ok_or_else(|| Error::new(eyre!("PackageDataEntry must not be in `removing` or `restoring` state in db for Service to be `load`ed"), ErrorKind::Incoherent))?,
                        );
                        lock.insert(id, service.clone());
                        Ok(())
                    }
                }.await {
                    let _ = error.set(e.clone_output());
                    cancel_hook.drop().await.with_kind(ErrorKind::Unknown)?;
                    Err(e)
                } else {
                    Ok(())
                }
            }.boxed()),
            Err(e) => {
                let _ = error.set(e.clone_output());
                cancel_hook.drop().await.with_kind(ErrorKind::Unknown)?;
                Err(e)
            }
        }
        }.boxed())
    }

    /// This is ran during the cleanup, so when we are uninstalling the service
    #[instrument(skip_all)]
    pub async fn uninstall(&self, id: &PackageId) -> Result<(), Error> {
        if let Some(service) = self.0.write().await.remove(id) {
            service.uninstall().await?;
        }
        Ok(())
    }

    /// Used during a shutdown
    #[instrument(skip_all)]
    pub async fn empty(&self) -> Result<(), Error> {
        let res =
            futures::future::join_all(std::mem::take(&mut *self.0.write().await).into_iter().map(
                |(id, service)| async move {
                    tracing::debug!("Service for {id} shutting down");
                    service.shutdown().await?;
                    tracing::debug!("Service for {id} is shutdown");
                    if let Err(e) = Arc::try_unwrap(service) {
                        tracing::trace!(
                            "Service for {} still has {} other open references",
                            id,
                            Arc::strong_count(&e) - 1
                        );
                    }
                    Ok::<_, Error>(())
                },
            ))
            .await;
        res.into_iter().fold(Ok(()), |res, x| match (res, x) {
            (Ok(()), x) => x,
            (Err(e), Ok(())) => Err(e),
            (Err(e1), Err(e2)) => Err(Error::new(eyre!("{}, {}", e1.source, e2.source), e1.kind)),
        })
    }

    #[instrument(skip_all)]
    pub async fn get(&self, id: &PackageId) -> Option<Arc<Service>> {
        self.0.read().await.get(id).cloned()
    }
}
