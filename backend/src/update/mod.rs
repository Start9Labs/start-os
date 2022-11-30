use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use clap::ArgMatches;
use color_eyre::eyre::{eyre, Result};
use emver::Version;
use helpers::{Rsync, RsyncOptions};
use lazy_static::lazy_static;
use patch_db::{DbHandle, LockType, Revision};
use reqwest::Url;
use rpc_toolkit::command;
use tokio::process::Command;
use tokio_stream::StreamExt;
use tracing::instrument;

use crate::context::RpcContext;
use crate::db::model::UpdateProgress;
use crate::disk::mount::filesystem::bind::Bind;
use crate::disk::mount::filesystem::ReadWrite;
use crate::disk::mount::guard::MountGuard;
use crate::notifications::NotificationLevel;
use crate::sound::{
    CIRCLE_OF_5THS_SHORT, UPDATE_FAILED_1, UPDATE_FAILED_2, UPDATE_FAILED_3, UPDATE_FAILED_4,
};
use crate::update::latest_information::LatestInformation;
use crate::util::Invoke;
use crate::version::{Current, VersionT};
use crate::{Error, ErrorKind, ResultExt, IS_RASPBERRY_PI};

mod latest_information;

lazy_static! {
    static ref UPDATED: AtomicBool = AtomicBool::new(false);
}

/// An user/ daemon would call this to update the system to the latest version and do the updates available,
/// and this will return something if there is an update, and in that case there will need to be a restart.
#[command(
    rename = "update",
    display(display_update_result),
    metadata(sync_db = true)
)]
#[instrument(skip(ctx))]
pub async fn update_system(
    #[context] ctx: RpcContext,
    #[arg(rename = "marketplace-url")] marketplace_url: Url,
) -> Result<UpdateResult, Error> {
    if UPDATED.load(Ordering::SeqCst) {
        return Ok(UpdateResult::NoUpdates);
    }
    Ok(if maybe_do_update(ctx, marketplace_url).await?.is_some() {
        UpdateResult::Updating
    } else {
        UpdateResult::NoUpdates
    })
}

/// What is the status of the updates?
#[derive(serde::Serialize, serde::Deserialize, Clone, Debug)]
#[serde(rename_all = "kebab-case")]
pub enum UpdateResult {
    NoUpdates,
    Updating,
}

fn display_update_result(status: UpdateResult, _: &ArgMatches) {
    match status {
        UpdateResult::Updating => {
            println!("Updating...");
        }
        UpdateResult::NoUpdates => {
            println!("No updates available");
        }
    }
}

#[instrument(skip(ctx))]
async fn maybe_do_update(
    ctx: RpcContext,
    marketplace_url: Url,
) -> Result<Option<Arc<Revision>>, Error> {
    let mut db = ctx.db.handle();
    let arch = if *IS_RASPBERRY_PI {
        "raspberrypi"
    } else {
        *crate::ARCH
    };
    let latest_version: Version = reqwest::get(format!(
        "{}/eos/v0/latest?eos-version={}&arch={}",
        marketplace_url,
        Current::new().semver(),
        arch,
    ))
    .await
    .with_kind(ErrorKind::Network)?
    .json::<LatestInformation>()
    .await
    .with_kind(ErrorKind::Network)?
    .version;
    crate::db::DatabaseModel::new()
        .server_info()
        .lock(&mut db, LockType::Write)
        .await?;
    let current_version = crate::db::DatabaseModel::new()
        .server_info()
        .version()
        .get_mut(&mut db)
        .await?;
    if &latest_version < &current_version {
        return Ok(None);
    }
    let mut tx = db.begin().await?;
    let mut status = crate::db::DatabaseModel::new()
        .server_info()
        .status_info()
        .get_mut(&mut tx)
        .await?;
    if status.update_progress.is_some() {
        return Err(Error::new(
            eyre!("Server is already updating!"),
            crate::ErrorKind::InvalidRequest,
        ));
    }
    if status.updated {
        return Ok(None);
    }

    let eos_url = EosUrl {
        base: marketplace_url,
        version: latest_version,
    };

    status.update_progress = Some(UpdateProgress {
        size: None,
        downloaded: 0,
    });
    status.save(&mut tx).await?;
    let rev = tx.commit().await?;

    tokio::spawn(async move {
        let res = do_update(ctx.clone(), eos_url).await;
        let mut db = ctx.db.handle();
        let mut status = crate::db::DatabaseModel::new()
            .server_info()
            .status_info()
            .get_mut(&mut db)
            .await
            .expect("could not access status");
        status.update_progress = None;
        match res {
            Ok(()) => {
                status.updated = true;
                status.save(&mut db).await.expect("could not save status");
                CIRCLE_OF_5THS_SHORT
                    .play()
                    .await
                    .expect("could not play sound");
            }
            Err(e) => {
                status.save(&mut db).await.expect("could not save status");
                ctx.notification_manager
                    .notify(
                        &mut db,
                        None,
                        NotificationLevel::Error,
                        "embassyOS Update Failed".to_owned(),
                        format!("Update was not successful because of {}", e),
                        (),
                        None,
                    )
                    .await
                    .expect("");
                // TODO: refactor sound lib to make compound tempos easier to deal with
                UPDATE_FAILED_1
                    .play()
                    .await
                    .expect("could not play song: update failed 1");
                UPDATE_FAILED_2
                    .play()
                    .await
                    .expect("could not play song: update failed 2");
                UPDATE_FAILED_3
                    .play()
                    .await
                    .expect("could not play song: update failed 3");
                UPDATE_FAILED_4
                    .play()
                    .await
                    .expect("could not play song: update failed 4");
            }
        }
    });
    Ok(rev)
}

#[instrument(skip(ctx, eos_url))]
async fn do_update(ctx: RpcContext, eos_url: EosUrl) -> Result<(), Error> {
    let mut rsync = Rsync::new(
        eos_url.rsync_path()?,
        "/media/embassy/next",
        Default::default(),
    )
    .await?;
    while let Some(progress) = rsync.progress.next().await {
        crate::db::DatabaseModel::new()
            .server_info()
            .status_info()
            .update_progress()
            .put(
                &mut ctx.db.handle(),
                &UpdateProgress {
                    size: Some(100),
                    downloaded: (100.0 * progress) as u64,
                },
            )
            .await?;
    }
    rsync.wait().await?;

    copy_fstab().await?;
    copy_machine_id().await?;
    copy_ssh_host_keys().await?;
    sync_boot().await?;
    swap_boot_label().await?;

    Ok(())
}

#[derive(Debug)]
struct EosUrl {
    base: Url,
    version: Version,
}

impl EosUrl {
    #[instrument()]
    pub fn rsync_path(&self) -> Result<PathBuf, Error> {
        let host = self
            .base
            .host_str()
            .ok_or_else(|| Error::new(eyre!("Could not get host of base"), ErrorKind::ParseUrl))?;
        let version: &Version = &self.version;
        let arch = if *IS_RASPBERRY_PI {
            "raspberrypi"
        } else {
            *crate::ARCH
        };
        Ok(format!("{host}::{version}/{arch}/")
            .parse()
            .map_err(|_| Error::new(eyre!("Could not parse path"), ErrorKind::ParseUrl))?)
    }
}

async fn copy_fstab() -> Result<(), Error> {
    tokio::fs::copy("/etc/fstab", "/media/embassy/next/etc/fstab").await?;
    Ok(())
}

async fn copy_machine_id() -> Result<(), Error> {
    tokio::fs::copy("/etc/machine-id", "/media/embassy/next/etc/machine-id").await?;
    Ok(())
}

async fn copy_ssh_host_keys() -> Result<(), Error> {
    tokio::fs::copy(
        "/etc/ssh/ssh_host_rsa_key",
        "/media/embassy/next/etc/ssh/ssh_host_rsa_key",
    )
    .await?;
    tokio::fs::copy(
        "/etc/ssh/ssh_host_rsa_key.pub",
        "/media/embassy/next/etc/ssh/ssh_host_rsa_key.pub",
    )
    .await?;
    tokio::fs::copy(
        "/etc/ssh/ssh_host_ecdsa_key",
        "/media/embassy/next/etc/ssh/ssh_host_ecdsa_key",
    )
    .await?;
    tokio::fs::copy(
        "/etc/ssh/ssh_host_ecdsa_key.pub",
        "/media/embassy/next/etc/ssh/ssh_host_ecdsa_key.pub",
    )
    .await?;
    tokio::fs::copy(
        "/etc/ssh/ssh_host_ed25519_key",
        "/media/embassy/next/etc/ssh/ssh_host_ed25519_key",
    )
    .await?;
    tokio::fs::copy(
        "/etc/ssh/ssh_host_ed25519_key.pub",
        "/media/embassy/next/etc/ssh/ssh_host_ed25519_key.pub",
    )
    .await?;
    Ok(())
}

async fn sync_boot() -> Result<(), Error> {
    Rsync::new(
        "/media/embassy/next/boot/",
        "/boot/",
        RsyncOptions {
            delete: false,
            force: false,
            ignore_existing: true,
            exclude: Vec::new(),
        },
    )
    .await?
    .wait()
    .await?;
    if !*IS_RASPBERRY_PI {
        let dev_mnt =
            MountGuard::mount(&Bind::new("/dev"), "/media/embassy/next/dev", ReadWrite).await?;
        let sys_mnt =
            MountGuard::mount(&Bind::new("/sys"), "/media/embassy/next/sys", ReadWrite).await?;
        let proc_mnt =
            MountGuard::mount(&Bind::new("/proc"), "/media/embassy/next/proc", ReadWrite).await?;
        let boot_mnt =
            MountGuard::mount(&Bind::new("/boot"), "/media/embassy/next/boot", ReadWrite).await?;
        Command::new("chroot")
            .arg("/media/embassy/next")
            .arg("update-grub")
            .invoke(ErrorKind::MigrationFailed)
            .await?;
        boot_mnt.unmount().await?;
        proc_mnt.unmount().await?;
        sys_mnt.unmount().await?;
        dev_mnt.unmount().await?;
    }
    Ok(())
}

#[instrument]
async fn swap_boot_label() -> Result<(), Error> {
    tokio::fs::write("/media/embassy/config/upgrade", b"").await?;
    Ok(())
}
