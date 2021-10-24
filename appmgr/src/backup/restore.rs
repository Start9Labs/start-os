use std::path::{Path, PathBuf};
use std::sync::Arc;

use clap::ArgMatches;
use color_eyre::eyre::eyre;
use patch_db::{DbHandle, PatchDbHandle, Revision};
use rpc_toolkit::command;
use tokio::fs::File;
use tracing::instrument;

use crate::auth::check_password_against_db;
use crate::context::RpcContext;
use crate::db::model::{PackageDataEntry, StaticFiles};
use crate::db::util::WithRevision;
use crate::disk::util::{BackupMountGuard, PackageBackupMountGuard, TmpMountGuard};
use crate::install::progress::{InstallProgress, InstallProgressTracker};
use crate::install::{install_s9pk_or_cleanup, PKG_PUBLIC_DIR};
use crate::s9pk::manifest::PackageId;
use crate::s9pk::reader::S9pkReader;
use crate::util::{display_none, Version};
use crate::volume::BACKUP_DIR;
use crate::Error;

fn parse_comma_separated(arg: &str, _: &ArgMatches<'_>) -> Result<Vec<PackageId>, Error> {
    arg.split(",")
        .map(|s| s.trim().parse().map_err(Error::from))
        .collect()
}

#[command(rename = "restore", display(display_none))]
#[instrument(skip(ctx, old_password, password))]
pub async fn restore_packages(
    #[context] ctx: RpcContext,
    #[arg(parse(parse_comma_separated))] ids: Vec<PackageId>,
    #[arg] logicalname: PathBuf,
    #[arg(rename = "old-password", long = "old-password")] old_password: Option<String>,
    #[arg] password: String,
) -> Result<WithRevision<()>, Error> {
    let mut db = ctx.db.handle();
    check_password_against_db(&mut ctx.secret_store.acquire().await?, &password).await?;
    let mut backup_guard = BackupMountGuard::mount(
        TmpMountGuard::mount(&logicalname, None).await?,
        old_password.as_ref().unwrap_or(&password),
    )
    .await?;
    if old_password.is_some() {
        backup_guard.change_password(&password)?;
    }
    let (revision, guards) = assure_restoring(&ctx, &mut db, ids, &backup_guard).await?;

    let mut tasks = Vec::with_capacity(guards.len());
    for (id, version, progress, guard) in guards {
        let ctx = ctx.clone();
        tasks.push(tokio::spawn(async move {
            if let Err(e) = restore_package(&ctx, &id, &version, progress, guard).await {
                tracing::error!("Error restoring package {}: {}", id, e);
                tracing::debug!("{:?}", e);
            }
        }));
    }

    tokio::spawn(async {
        futures::future::join_all(tasks).await;
        if let Err(e) = backup_guard.unmount().await {
            tracing::error!("Error unmounting backup drive: {}", e);
            tracing::debug!("{:?}", e);
        }
    });

    Ok(WithRevision {
        response: (),
        revision,
    })
}

#[instrument(skip(ctx, db, backup_guard))]
async fn assure_restoring(
    ctx: &RpcContext,
    db: &mut PatchDbHandle,
    ids: Vec<PackageId>,
    backup_guard: &BackupMountGuard<TmpMountGuard>,
) -> Result<
    (
        Option<Arc<Revision>>,
        Vec<(
            PackageId,
            Version,
            Arc<InstallProgress>,
            PackageBackupMountGuard,
        )>,
    ),
    Error,
> {
    let mut tx = db.begin().await?;

    let mut guards = Vec::with_capacity(ids.len());

    for id in ids {
        let mut model = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(&id)
            .get_mut(&mut tx)
            .await?;

        if !model.is_none() {
            return Err(Error::new(
                eyre!("Can't restore over existing package: {}", id),
                crate::ErrorKind::InvalidRequest,
            ));
        }

        let guard = backup_guard.mount_package_backup(&id).await?;
        let s9pk_path = Path::new(BACKUP_DIR).join(&id).join(format!("{}.s9pk", id));
        let mut rdr = S9pkReader::open(&s9pk_path, false).await?;

        let manifest = rdr.manifest().await?;
        let version = manifest.version.clone();
        let progress = InstallProgress::new(Some(tokio::fs::metadata(&s9pk_path).await?.len()));
        progress
            .download_complete
            .store(true, std::sync::atomic::Ordering::SeqCst);

        let public_dir_path = ctx
            .datadir
            .join(PKG_PUBLIC_DIR)
            .join(&id)
            .join(version.as_str());
        tokio::fs::create_dir_all(&public_dir_path).await?;

        let license_path = public_dir_path.join("LICENSE.md");
        let mut dst = File::create(&license_path).await?;
        tokio::io::copy(&mut rdr.license().await?, &mut dst).await?;
        dst.sync_all().await?;

        let instructions_path = public_dir_path.join("INSTRUCTIONS.md");
        let mut dst = File::create(&instructions_path).await?;
        tokio::io::copy(&mut rdr.instructions().await?, &mut dst).await?;
        dst.sync_all().await?;

        let icon_path = Path::new("icon").with_extension(&manifest.assets.icon_type());
        let icon_path = public_dir_path.join(&icon_path);
        let mut dst = File::create(&icon_path).await?;
        tokio::io::copy(&mut rdr.icon().await?, &mut dst).await?;
        dst.sync_all().await?;

        *model = Some(PackageDataEntry::Restoring {
            install_progress: progress.clone(),
            static_files: StaticFiles::local(&id, &version, manifest.assets.icon_type()),
            manifest,
        });
        model.save(&mut tx).await?;

        guards.push((id, version, progress, guard));
    }

    Ok((tx.commit(None).await?, guards))
}

#[instrument(skip(ctx, guard))]
async fn restore_package(
    ctx: &RpcContext,
    id: &PackageId,
    version: &Version,
    progress: Arc<InstallProgress>,
    guard: PackageBackupMountGuard,
) -> Result<(), Error> {
    let s9pk_path = Path::new(BACKUP_DIR).join(id).join(format!("{}.s9pk", id));
    let progress_reader =
        InstallProgressTracker::new(File::open(&s9pk_path).await?, progress.clone());
    let mut s9pk_reader = progress
        .track_read_during(
            crate::db::DatabaseModel::new()
                .package_data()
                .idx_model(id)
                .and_then(|pde| pde.install_progress()),
            &ctx.db,
            || S9pkReader::from_reader(progress_reader, true),
        )
        .await?;

    install_s9pk_or_cleanup(ctx, id, version, &mut s9pk_reader, progress).await?;

    guard.unmount().await?;

    Ok(())
}
