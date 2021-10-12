use std::future::Future;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Duration;

use clap::ArgMatches;
use color_eyre::eyre::{eyre, Result};
use digest::Digest;
use emver::Version;
use futures::Stream;
use lazy_static::lazy_static;
use patch_db::{DbHandle, Revision};
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
use crate::db::model::{ServerStatus, UpdateProgress};
use crate::notifications::{NotificationLevel, NotificationSubtype};
use crate::update::latest_information::LatestInformation;
use crate::util::Invoke;
use crate::{Error, ErrorKind, ResultExt};

lazy_static! {
    static ref UPDATED: AtomicBool = AtomicBool::new(false);
}

/// An user/ daemon would call this to update the system to the latest version and do the updates available,
/// and this will return something if there is an update, and in that case there will need to be a restart.
#[command(rename = "update", display(display_properties))]
#[instrument(skip(ctx))]
pub async fn update_system(#[context] ctx: RpcContext) -> Result<UpdateSystem, Error> {
    if UPDATED.load(Ordering::SeqCst) {
        return Ok(UpdateSystem::NoUpdates);
    }
    if let None = maybe_do_update(ctx).await? {
        return Ok(UpdateSystem::Updated);
    }
    Ok(UpdateSystem::NoUpdates)
}

/// What is the status of the updates?
#[derive(serde::Serialize, serde::Deserialize, Clone, Debug)]
pub enum UpdateSystem {
    NoUpdates,
    Updated,
}

fn display_properties(status: UpdateSystem, _: &ArgMatches<'_>) {
    match status {
        UpdateSystem::NoUpdates => {
            println!("Updates are ready, please reboot");
        }
        UpdateSystem::Updated => {
            println!("No updates needed");
        }
    }
}

const HEADER_KEY: &str = "x-eos-hash";
mod latest_information;

#[derive(Debug, Clone, Copy)]
enum WritableDrives {
    Green,
    Blue,
}

#[derive(Debug, Clone, Copy)]
struct Boot;

/// We are going to be creating some folders and mounting so
/// we need to know the labels for those types. These labels
/// are the labels that are shipping with the embassy, blue/ green
/// are where the os sits and will do a swap during update.
trait FileType: std::fmt::Debug + Copy + Send + Sync + 'static {
    fn mount_folder(&self) -> PathBuf {
        Path::new("/media").join(self.label())
    }
    fn label(&self) -> &'static str;
    fn block_dev(&self) -> &'static Path;
}
impl FileType for WritableDrives {
    fn label(&self) -> &'static str {
        match self {
            WritableDrives::Green => "green",
            WritableDrives::Blue => "blue",
        }
    }
    fn block_dev(&self) -> &'static Path {
        Path::new(match self {
            WritableDrives::Green => "/dev/mmcblk0p3",
            WritableDrives::Blue => "/dev/mmcblk0p4",
        })
    }
}
impl FileType for Boot {
    fn label(&self) -> &'static str {
        "system-boot"
    }
    fn block_dev(&self) -> &'static Path {
        Path::new("/dev/mmcblk0p1")
    }
}

/// Proven data that this is mounted, should be consumed in an unmount
#[derive(Debug)]
struct MountedResource<X: FileType> {
    value: X,
    mounted: bool,
}
impl<X: FileType> MountedResource<X> {
    fn new(value: X) -> Self {
        MountedResource {
            value,
            mounted: true,
        }
    }
    #[instrument]
    async fn unmount(value: X) -> Result<(), Error> {
        let folder = value.mount_folder();
        Command::new("umount")
            .arg(&folder)
            .invoke(crate::ErrorKind::Filesystem)
            .await?;
        tokio::fs::remove_dir_all(&folder)
            .await
            .with_ctx(|_| (crate::ErrorKind::Filesystem, folder.display().to_string()))?;
        Ok(())
    }
    #[instrument]
    async fn unmount_label(&mut self) -> Result<(), Error> {
        Self::unmount(self.value).await?;
        self.mounted = false;
        Ok(())
    }
}
impl<X: FileType> Drop for MountedResource<X> {
    fn drop(&mut self) {
        if self.mounted {
            let value = self.value;
            tokio::spawn(async move { Self::unmount(value).await.expect("failed to unmount") });
        }
    }
}

/// This will be where we are going to be putting the new update
#[derive(Debug, Clone, Copy)]
struct NewLabel(WritableDrives);

/// This is our current label where the os is running
struct CurrentLabel(WritableDrives);

lazy_static! {
    static ref PARSE_COLOR: Regex = Regex::new("#LABEL=(\\w+) /media/root-ro/").unwrap();
}

#[instrument(skip(ctx))]
async fn maybe_do_update(ctx: RpcContext) -> Result<Option<Arc<Revision>>, Error> {
    let mut db = ctx.db.handle();
    let latest_version = reqwest::get(format!("{}/eos/latest", ctx.eos_registry_url().await?))
        .await
        .with_kind(ErrorKind::Network)?
        .json::<LatestInformation>()
        .await
        .with_kind(ErrorKind::Network)?
        .version;
    let current_version = crate::db::DatabaseModel::new()
        .server_info()
        .version()
        .get_mut(&mut db)
        .await?;
    if &latest_version <= &current_version {
        return Ok(None);
    }
    let mut tx = db.begin().await?;
    let mut info = crate::db::DatabaseModel::new()
        .server_info()
        .get_mut(&mut tx)
        .await?;
    match &info.status {
        ServerStatus::Updating => {
            return Err(Error::new(
                eyre!("Server is already updating!"),
                crate::ErrorKind::InvalidRequest,
            ))
        }
        ServerStatus::Updated => {
            return Ok(None);
        }
        ServerStatus::BackingUp => {
            return Err(Error::new(
                eyre!("Server is backing up!"),
                crate::ErrorKind::InvalidRequest,
            ))
        }
        _ => (),
    }

    let mounted_boot = mount_label(Boot).await?;
    let (new_label, _current_label) = query_mounted_label().await?;
    let (size, download) = download_file(
        ctx.db.handle(),
        &EosUrl {
            base: info.eos_marketplace.clone(),
            version: latest_version.clone(),
        },
        new_label,
    )
    .await?;
    info.status = ServerStatus::Updating;
    info.update_progress = Some(UpdateProgress {
        size,
        downloaded: 0,
    });
    info.save(&mut tx).await?;
    let rev = tx.commit(None).await?;

    tokio::spawn(async move {
        let mut db = ctx.db.handle();
        let res = do_update(download, new_label, mounted_boot).await;
        let mut info = crate::db::DatabaseModel::new()
            .server_info()
            .get_mut(&mut db)
            .await
            .expect("could not access status");
        info.update_progress = None;
        match res {
            Ok(()) => {
                info.status = ServerStatus::Updated;
                info.save(&mut db).await.expect("could not save status");
            }
            Err(e) => {
                info.status = ServerStatus::Running;
                info.save(&mut db).await.expect("could not save status");
                ctx.notification_manager
                    .notify(
                        &mut db,
                        None,
                        NotificationLevel::Error,
                        "EmbassyOS Update Failed".to_owned(),
                        format!("Update was not successful because of {}", e),
                        NotificationSubtype::General,
                    )
                    .await
                    .expect("")
            }
        }
    });
    Ok(rev)
}

#[instrument(skip(download))]
async fn do_update(
    download: impl Future<Output = Result<(), Error>>,
    new_label: NewLabel,
    mut mounted_boot: MountedResource<Boot>,
) -> Result<(), Error> {
    download.await?;
    swap_boot_label(new_label, &mounted_boot).await?;

    mounted_boot.unmount_label().await?;

    Ok(())
}

#[instrument]
async fn query_mounted_label() -> Result<(NewLabel, CurrentLabel), Error> {
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
        write!(f, "{}/eos/eos.img?version=={}", self.base, self.version)
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
        let hash_from_header: String = "".to_owned(); // download_request
                                                      // .headers()
                                                      // .get(HEADER_KEY)
                                                      // .ok_or_else(|| Error::new(eyre!("No {} in headers", HEADER_KEY), ErrorKind::Network))?
                                                      // .to_str()
                                                      // .with_kind(ErrorKind::InvalidRequest)?
                                                      // .to_owned();
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
    while let Some(Ok(item)) = stream_download.next().await {
        file.write_all(&item)
            .await
            .with_kind(ErrorKind::Filesystem)?;
        downloaded += item.len() as u64;
        if last_progress_update.elapsed() > Duration::from_secs(1) {
            last_progress_update = Instant::now();
            crate::db::DatabaseModel::new()
                .server_info()
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
    // if hex::decode(hash_from_header).with_kind(ErrorKind::Network)? != file_digest {
    //     return Err(Error::new(
    //         eyre!("Hash sum does not match source"),
    //         ErrorKind::Network,
    //     ));
    // }
    Ok(())
}

#[instrument]
async fn swap_boot_label(
    new_label: NewLabel,
    mounted_boot: &MountedResource<Boot>,
) -> Result<(), Error> {
    let block_dev = new_label.0.block_dev();
    Command::new("e2label")
        .arg(block_dev)
        .arg(new_label.0.label())
        .invoke(crate::ErrorKind::BlockDevice)
        .await?;
    let mut mounted = mount_label(new_label.0).await?;
    let sedcmd = format!("s/LABEL=\\(blue\\|green\\)/LABEL={}/g", new_label.0.label());
    Command::new("sed")
        .arg("-i")
        .arg(&sedcmd)
        .arg(mounted.value.mount_folder().join("etc/fstab"))
        .output()
        .await?;
    mounted.unmount_label().await?;
    Command::new("sed")
        .arg("-i")
        .arg(&sedcmd)
        .arg(mounted_boot.value.mount_folder().join("cmdline.txt"))
        .output()
        .await?;

    UPDATED.store(true, Ordering::SeqCst);
    Ok(())
}

#[instrument]
async fn mount_label<F>(file_type: F) -> Result<MountedResource<F>, Error>
where
    F: FileType,
{
    let label = file_type.label();
    let folder = file_type.mount_folder();
    tokio::fs::create_dir_all(&folder)
        .await
        .with_ctx(|_| (crate::ErrorKind::Filesystem, folder.display().to_string()))?;
    Command::new("mount")
        .arg("-L")
        .arg(label)
        .arg(folder)
        .invoke(crate::ErrorKind::Filesystem)
        .await?;
    Ok(MountedResource::new(file_type))
}
/// Captured from doing an fstab with an embassy box and the cat from the /etc/fstab
#[test]
fn test_capture() {
    let output = r#"
#
#  This fstab is for overlayroot. The real one can be found at
#  /media/root-ro/etc/fstab
#  The original entry for '/' and other mounts have been updated to be placed
#  under /media/root-ro.
#  To permanently modify this (or any other file), you should change-root into
#  a writable view of the underlying filesystem using:
#      sudo overlayroot-chroot
#
#LABEL=blue /media/root-ro/ ext4 ro,discard,errors=remount-ro,noauto 0 1
/media/root-ro/ / overlay lowerdir=/media/root-ro/,upperdir=/media/root-rw/overlay/,workdir=/media/root-rw/overlay-workdir/_ 0 1
LABEL=system-boot /boot/firmware vfat defaults 0 1 # overlayroot:fs-unsupported
"#;
    assert_eq!(&PARSE_COLOR.captures(&output).unwrap()[1], "blue");
}
