use std::future::Future;
use std::path::Path;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Duration;

use clap::ArgMatches;
use color_eyre::eyre::{eyre, Result};
use digest::Digest;
use emver::Version;
use futures::Stream;
use lazy_static::lazy_static;
use patch_db::{DbHandle, LockType, Revision};
use regex::Regex;
use reqwest::Url;
use rpc_toolkit::command;
use sha2::Sha256;
use tokio::io::AsyncWriteExt;
use tokio::pin;
use tokio::process::Command;
use tokio::time::Instant;
use tokio_stream::StreamExt;
use tracing::instrument;

use crate::context::RpcContext;
use crate::db::model::UpdateProgress;
use crate::db::util::WithRevision;
use crate::disk::mount::filesystem::block_dev::BlockDev;
use crate::disk::mount::filesystem::{FileSystem, ReadWrite};
use crate::disk::mount::guard::TmpMountGuard;
use crate::disk::BOOT_RW_PATH;
use crate::notifications::NotificationLevel;
use crate::sound::{
    CIRCLE_OF_5THS_SHORT, UPDATE_FAILED_1, UPDATE_FAILED_2, UPDATE_FAILED_3, UPDATE_FAILED_4,
};
use crate::update::latest_information::LatestInformation;
use crate::util::Invoke;
use crate::version::{Current, VersionT};
use crate::{Error, ErrorKind, ResultExt};

mod latest_information;

lazy_static! {
    static ref UPDATED: AtomicBool = AtomicBool::new(false);
}

/// An user/ daemon would call this to update the system to the latest version and do the updates available,
/// and this will return something if there is an update, and in that case there will need to be a restart.
#[command(rename = "update", display(display_update_result))]
#[instrument(skip(ctx))]
pub async fn update_system(
    #[context] ctx: RpcContext,
    #[arg(rename = "marketplace-url")] marketplace_url: Url,
) -> Result<WithRevision<UpdateResult>, Error> {
    let noop = WithRevision {
        response: UpdateResult::NoUpdates,
        revision: None,
    };
    if UPDATED.load(Ordering::SeqCst) {
        return Ok(noop);
    }
    match maybe_do_update(ctx, marketplace_url).await? {
        None => Ok(noop),
        Some(r) => Ok(WithRevision {
            response: UpdateResult::Updating,
            revision: Some(r),
        }),
    }
}

/// What is the status of the updates?
#[derive(serde::Serialize, serde::Deserialize, Clone, Debug)]
#[serde(rename_all = "kebab-case")]
pub enum UpdateResult {
    NoUpdates,
    Updating,
}

fn display_update_result(status: WithRevision<UpdateResult>, _: &ArgMatches) {
    match status.response {
        UpdateResult::Updating => {
            println!("Updating...");
        }
        UpdateResult::NoUpdates => {
            println!("No updates available");
        }
    }
}

const HEADER_KEY: &str = "x-eos-hash";

#[derive(Debug, Clone, Copy)]
pub enum WritableDrives {
    Green,
    Blue,
}
impl WritableDrives {
    pub fn label(&self) -> &'static str {
        match self {
            Self::Green => "green",
            Self::Blue => "blue",
        }
    }
    pub fn block_dev(&self) -> &'static Path {
        Path::new(match self {
            Self::Green => "/dev/mmcblk0p3",
            Self::Blue => "/dev/mmcblk0p4",
        })
    }
    pub fn part_uuid(&self) -> &'static str {
        match self {
            Self::Green => "cb15ae4d-03",
            Self::Blue => "cb15ae4d-04",
        }
    }
    pub fn as_fs(&self) -> impl FileSystem {
        BlockDev::new(self.block_dev())
    }
}

/// This will be where we are going to be putting the new update
#[derive(Debug, Clone, Copy)]
pub struct NewLabel(pub WritableDrives);

/// This is our current label where the os is running
pub struct CurrentLabel(pub WritableDrives);

lazy_static! {
    static ref PARSE_COLOR: Regex = Regex::new("LABEL=(\\w+)[ \t]+/").unwrap();
}

#[instrument(skip(ctx))]
async fn maybe_do_update(
    ctx: RpcContext,
    marketplace_url: Url,
) -> Result<Option<Arc<Revision>>, Error> {
    let mut db = ctx.db.handle();
    let latest_version = reqwest::get(format!(
        "{}/eos/v0/latest?eos-version={}&arch={}",
        marketplace_url,
        Current::new().semver(),
        &*crate::ARCH,
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
    if &latest_version <= &current_version {
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

    let (new_label, _current_label) = query_mounted_label().await?;
    let (size, download) = download_file(
        ctx.db.handle(),
        &EosUrl {
            base: marketplace_url,
            version: latest_version.clone(),
        },
        new_label,
    )
    .await?;
    status.update_progress = Some(UpdateProgress {
        size,
        downloaded: 0,
    });
    status.save(&mut tx).await?;
    let rev = tx.commit(None).await?;

    tokio::spawn(async move {
        let mut db = ctx.db.handle();
        let res = do_update(download, new_label).await;
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
                        "EmbassyOS Update Failed".to_owned(),
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

#[instrument(skip(download))]
async fn do_update(
    download: impl Future<Output = Result<(), Error>>,
    new_label: NewLabel,
) -> Result<(), Error> {
    download.await?;
    copy_machine_id(new_label).await?;
    copy_ssh_host_keys(new_label).await?;
    swap_boot_label(new_label).await?;

    Ok(())
}

#[instrument]
pub async fn query_mounted_label() -> Result<(NewLabel, CurrentLabel), Error> {
    let output = tokio::fs::read_to_string("/etc/fstab")
        .await
        .with_ctx(|_| (crate::ErrorKind::Filesystem, "/etc/fstab"))?;

    match &PARSE_COLOR.captures(&output).ok_or_else(|| {
        Error::new(
            eyre!("Can't find pattern in {}", output),
            crate::ErrorKind::Filesystem,
        )
    })?[1]
    {
        x if x == WritableDrives::Green.label() => Ok((
            NewLabel(WritableDrives::Blue),
            CurrentLabel(WritableDrives::Green),
        )),
        x if x == WritableDrives::Blue.label() => Ok((
            NewLabel(WritableDrives::Green),
            CurrentLabel(WritableDrives::Blue),
        )),
        e => {
            return Err(Error::new(
                eyre!("Could not find a mounted resource for {}", e),
                crate::ErrorKind::Filesystem,
            ))
        }
    }
}

#[derive(Debug)]
struct EosUrl {
    base: Url,
    version: Version,
}
impl std::fmt::Display for EosUrl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}/eos/v0/eos.img?spec=={}&eos-version={}&arch={}",
            self.base,
            self.version,
            Current::new().semver(),
            &*crate::ARCH,
        )
    }
}

#[instrument(skip(db))]
async fn download_file<'a, Db: DbHandle + 'a>(
    mut db: Db,
    eos_url: &EosUrl,
    new_label: NewLabel,
) -> Result<(Option<u64>, impl Future<Output = Result<(), Error>> + 'a), Error> {
    let download_request = reqwest::get(eos_url.to_string())
        .await
        .with_kind(ErrorKind::Network)?;
    let size = download_request
        .headers()
        .get("content-length")
        .and_then(|a| a.to_str().ok())
        .map(|l| l.parse())
        .transpose()?;
    Ok((size, async move {
        let hash_from_header: String = download_request
            .headers()
            .get(HEADER_KEY)
            .ok_or_else(|| Error::new(eyre!("No {} in headers", HEADER_KEY), ErrorKind::Network))?
            .to_str()
            .with_kind(ErrorKind::InvalidRequest)?
            .to_owned();
        let stream_download = download_request.bytes_stream();
        let file_sum = write_stream_to_label(&mut db, size, stream_download, new_label).await?;
        check_download(&hash_from_header, file_sum).await?;
        Ok(())
    }))
}

#[instrument(skip(db, stream_download))]
async fn write_stream_to_label<Db: DbHandle>(
    db: &mut Db,
    size: Option<u64>,
    stream_download: impl Stream<Item = Result<rpc_toolkit::hyper::body::Bytes, reqwest::Error>>,
    file: NewLabel,
) -> Result<Vec<u8>, Error> {
    let block_dev = file.0.block_dev();
    let mut file = tokio::fs::OpenOptions::new()
        .write(true)
        .open(&block_dev)
        .await
        .with_kind(ErrorKind::Filesystem)?;
    let mut hasher = Sha256::new();
    pin!(stream_download);
    let mut downloaded = 0;
    let mut last_progress_update = Instant::now();
    while let Some(item) = stream_download
        .next()
        .await
        .transpose()
        .with_kind(ErrorKind::Network)?
    {
        file.write_all(&item)
            .await
            .with_kind(ErrorKind::Filesystem)?;
        downloaded += item.len() as u64;
        if last_progress_update.elapsed() > Duration::from_secs(1) {
            last_progress_update = Instant::now();
            crate::db::DatabaseModel::new()
                .server_info()
                .status_info()
                .update_progress()
                .put(db, &UpdateProgress { size, downloaded })
                .await?;
        }
        hasher.update(item);
    }
    file.flush().await.with_kind(ErrorKind::Filesystem)?;
    file.shutdown().await.with_kind(ErrorKind::Filesystem)?;
    drop(file);
    Ok(hasher.finalize().to_vec())
}

#[instrument]
async fn check_download(hash_from_header: &str, file_digest: Vec<u8>) -> Result<(), Error> {
    if hex::decode(hash_from_header).with_kind(ErrorKind::Network)? != file_digest {
        return Err(Error::new(
            eyre!("Hash sum does not match source"),
            ErrorKind::Network,
        ));
    }
    Ok(())
}

async fn copy_machine_id(new_label: NewLabel) -> Result<(), Error> {
    let new_guard = TmpMountGuard::mount(&new_label.0.as_fs(), ReadWrite).await?;
    tokio::fs::copy("/etc/machine-id", new_guard.as_ref().join("etc/machine-id")).await?;
    new_guard.unmount().await?;
    Ok(())
}

async fn copy_ssh_host_keys(new_label: NewLabel) -> Result<(), Error> {
    let new_guard = TmpMountGuard::mount(&new_label.0.as_fs(), ReadWrite).await?;
    tokio::fs::copy(
        "/etc/ssh/ssh_host_rsa_key",
        new_guard.as_ref().join("etc/ssh/ssh_host_rsa_key"),
    )
    .await?;
    tokio::fs::copy(
        "/etc/ssh/ssh_host_rsa_key.pub",
        new_guard.as_ref().join("etc/ssh/ssh_host_rsa_key.pub"),
    )
    .await?;
    tokio::fs::copy(
        "/etc/ssh/ssh_host_ecdsa_key",
        new_guard.as_ref().join("etc/ssh/ssh_host_ecdsa_key"),
    )
    .await?;
    tokio::fs::copy(
        "/etc/ssh/ssh_host_ecdsa_key.pub",
        new_guard.as_ref().join("etc/ssh/ssh_host_ecdsa_key.pub"),
    )
    .await?;
    tokio::fs::copy(
        "/etc/ssh/ssh_host_ed25519_key",
        new_guard.as_ref().join("etc/ssh/ssh_host_ed25519_key"),
    )
    .await?;
    tokio::fs::copy(
        "/etc/ssh/ssh_host_ed25519_key.pub",
        new_guard.as_ref().join("etc/ssh/ssh_host_ed25519_key.pub"),
    )
    .await?;
    new_guard.unmount().await?;
    Ok(())
}

#[instrument]
async fn swap_boot_label(new_label: NewLabel) -> Result<(), Error> {
    let block_dev = new_label.0.block_dev();
    Command::new("e2label")
        .arg(block_dev)
        .arg(new_label.0.label())
        .invoke(crate::ErrorKind::BlockDevice)
        .await?;
    let mounted = TmpMountGuard::mount(&new_label.0.as_fs(), ReadWrite).await?;
    Command::new("sed")
        .arg("-i")
        .arg(&format!(
            "s/LABEL=\\(blue\\|green\\)/LABEL={}/g",
            new_label.0.label()
        ))
        .arg(mounted.as_ref().join("etc/fstab"))
        .invoke(crate::ErrorKind::Filesystem)
        .await?;
    mounted.unmount().await?;
    Command::new("sed")
        .arg("-i")
        .arg(&format!(
            "s/PARTUUID=cb15ae4d-\\(03\\|04\\)/PARTUUID={}/g",
            new_label.0.part_uuid()
        ))
        .arg(Path::new(BOOT_RW_PATH).join("cmdline.txt.orig"))
        .invoke(crate::ErrorKind::Filesystem)
        .await?;
    Command::new("sed")
        .arg("-i")
        .arg(&format!(
            "s/PARTUUID=cb15ae4d-\\(03\\|04\\)/PARTUUID={}/g",
            new_label.0.part_uuid()
        ))
        .arg(Path::new(BOOT_RW_PATH).join("cmdline.txt"))
        .invoke(crate::ErrorKind::Filesystem)
        .await?;

    UPDATED.store(true, Ordering::SeqCst);
    Ok(())
}

/// Captured from doing an fstab with an embassy box and the cat from the /etc/fstab
#[test]
fn test_capture() {
    let output = r#"
LABEL=blue       /       ext4    discard,errors=remount-ro       0       1
LABEL=system-boot       /media/boot-rw  vfat    defaults        0       1
/media/boot-rw  /boot   none    defaults,bind,ro        0       0
LABEL=EMBASSY   /embassy-os     vfat    defaults        0       1
# a swapfile is not a swap partition, no line here
# use dphys-swapfile swap[on|off] for that
"#;
    assert_eq!(&PARSE_COLOR.captures(&output).unwrap()[1], "blue");
}
