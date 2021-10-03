use anyhow::{anyhow, bail, Result};
use clap::ArgMatches;
use digest::Digest;
use futures::Stream;
use lazy_static::lazy_static;
use regex::Regex;
use rpc_toolkit::command;
use sha2::Sha256;
use tokio::io::AsyncWriteExt;
use tokio::pin;
use tokio_stream::StreamExt;

use crate::context::RpcContext;
use crate::update::latest_information::LatestInformation;
use crate::{Error, ErrorKind, ResultExt};

/// An user/ daemon would call this to update the system to the latest version and do the updates available,
/// and this will return something if there is an update, and in that case there will need to be a restart.
#[command(display(display_properties))]
pub async fn update_system(#[context] ctx: RpcContext) -> Result<UpdateSystem, Error> {
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

const URL: &str = "https://beta-registry-0-3.start9labs.com/eos/latest";
const HEADER_KEY: &str = "CHECKSUM";
mod latest_information;

enum WritableDrives {
    Green,
    Blue,
}

struct Boot;

/// We are going to be creating some folders and mounting so
/// we need to know the labels for those types. These labels
/// are the labels that are shipping with the embassy, blue/ green
/// are where the os sits and will do a swap during update.
trait FileType {
    fn mount_folder(&self) -> String {
        format!("/media/{}", self.label())
    }
    fn label(&self) -> String;
}
impl FileType for WritableDrives {
    fn label(&self) -> String {
        match self {
            WritableDrives::Green => "green",
            WritableDrives::Blue => "blue",
        }
        .to_string()
    }
}
impl FileType for Boot {
    fn label(&self) -> String {
        "system-boot".to_string()
    }
}

/// Proven data that this is mounted, should be consumed in an unmount
struct MountedResource<X: FileType>(X);
impl<X: FileType> MountedResource<X> {
    async fn unmount_label(&self) -> Result<()> {
        let folder = self.0.mount_folder();
        tokio::process::Command::new("umount")
            .arg(&folder)
            .output()
            .await?;
        tokio::process::Command::new("rmdir")
            .arg(folder)
            .output()
            .await?;
        Ok(())
    }
}

/// This will be where we are going to be putting the new update
struct NewLabel<'a>(&'a MountedResource<WritableDrives>);

/// This is our current label where the os is running
struct CurrentLabel<'a>(&'a MountedResource<WritableDrives>);

lazy_static! {
    static ref PARSE_COLOR: Regex = Regex::new("#LABEL=(\\w+) /media/root-ro/").unwrap();
}

async fn maybe_do_update(ctx: RpcContext) -> Result<Option<()>, Error> {
    let mut db = ctx.db.handle();
    let latest_version = reqwest::get(URL)
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
    let mounted_blue = mount_label(WritableDrives::Blue)
        .await
        .with_kind(ErrorKind::Filesystem)?;
    let mounted_green = mount_label(WritableDrives::Green)
        .await
        .with_kind(ErrorKind::Filesystem)?;
    let mounted_boot = mount_label(Boot).await.with_kind(ErrorKind::Filesystem)?;
    let potential_error_actions = async {
        let (new_label, _current_label) = query_mounted_label(&mounted_blue, &mounted_green)
            .await
            .with_kind(ErrorKind::Filesystem)?;
        download_file(&new_label).await?;

        swap_boot_label(&new_label, &mounted_boot).await?;
        Ok::<_, Error>(())
    }
    .await;

    mounted_blue
        .unmount_label()
        .await
        .with_kind(ErrorKind::Filesystem)?;
    mounted_green
        .unmount_label()
        .await
        .with_kind(ErrorKind::Filesystem)?;
    mounted_boot
        .unmount_label()
        .await
        .with_kind(ErrorKind::Filesystem)?;
    potential_error_actions?;
    Ok(Some(()))
}

async fn query_mounted_label<'a>(
    mounted_resource_left: &'a MountedResource<WritableDrives>,
    mounted_resource_right: &'a MountedResource<WritableDrives>,
) -> Result<(NewLabel<'a>, CurrentLabel<'a>)> {
    let output = String::from_utf8(
        tokio::process::Command::new("cat")
            .arg("/etc/fstab")
            .output()
            .await?
            .stdout,
    )?;
    match &PARSE_COLOR
        .captures(&output)
        .ok_or_else(|| anyhow!("Can't find pattern in {}", output))?[1]
    {
        x if x == &mounted_resource_left.0.label() => Ok((
            NewLabel(mounted_resource_left),
            CurrentLabel(mounted_resource_right),
        )),
        x if x == &mounted_resource_right.0.label() => Ok((
            NewLabel(mounted_resource_right),
            CurrentLabel(mounted_resource_left),
        )),
        e => bail!("Could not find a mounted resource for {}", e),
    }
}

async fn download_file(new_label: &NewLabel<'_>) -> Result<(), Error> {
    let download_request = reqwest::get(URL).await.with_kind(ErrorKind::Network)?;
    let hash_from_header: String = download_request
        .headers()
        .get(HEADER_KEY)
        .ok_or_else(|| Error::new(anyhow!("No {} in headers", HEADER_KEY), ErrorKind::Network))?
        .to_str()
        .with_kind(ErrorKind::InvalidRequest)?
        .to_owned();
    let stream_download = download_request.bytes_stream();
    let file_sum = write_stream_to_label(stream_download, new_label).await?;
    check_download(&hash_from_header, file_sum).await?;
    Ok(())
}

async fn write_stream_to_label(
    stream_download: impl Stream<Item = Result<rpc_toolkit::hyper::body::Bytes, reqwest::Error>>,
    file: &NewLabel<'_>,
) -> Result<Vec<u8>, Error> {
    let folder = file.0 .0.mount_folder();
    let file_path = format!("{}/download.img", folder);
    tokio::process::Command::new("rm")
        .arg("-rf")
        .arg(format!("{}/*", folder))
        .output()
        .await?;
    let mut file = tokio::fs::File::create(&file_path)
        .await
        .with_kind(ErrorKind::Filesystem)?;
    let mut hasher = Sha256::new();
    pin!(stream_download);
    while let Some(Ok(item)) = stream_download.next().await {
        file.write(&item).await.with_kind(ErrorKind::Filesystem)?;
        hasher.update(item);
    }
    file.flush().await.with_kind(ErrorKind::Filesystem)?;
    drop(file);
    tokio::process::Command::new("dd")
        .arg(format!("if={}", file_path))
        .arg(format!("of={}", folder))
        .output()
        .await?;
    Ok(hasher.finalize().to_vec())
}

async fn check_download(hash_from_header: &str, file_digest: Vec<u8>) -> Result<(), Error> {
    if hex::decode(hash_from_header).with_kind(ErrorKind::Network)? != file_digest {
        return Err(Error::new(
            anyhow!("Hash sum does not match source"),
            ErrorKind::Network,
        ));
    }
    Ok(())
}
async fn swap_boot_label(
    new_label: &NewLabel<'_>,
    mounted_boot: &MountedResource<Boot>,
) -> Result<(), Error> {
    // disk/util add setLabel
    tokio::process::Command::new("sed")
        .arg(format!(r#""r/(blue|green)/{}/g""#, new_label.0 .0.label()))
        .arg(format!("{}/etc/fstab", mounted_boot.0.mount_folder()))
        .output()
        .await?;
    Ok(())
}

async fn mount_label<F>(file_type: F) -> Result<MountedResource<F>>
where
    F: FileType,
{
    let label = file_type.label();
    let folder = file_type.mount_folder();
    tokio::process::Command::new("mdkir")
        .arg(&folder)
        .output()
        .await?;
    tokio::process::Command::new("mount")
        .arg("-L")
        .arg(label)
        .arg(folder)
        .output()
        .await?;
    Ok(MountedResource(file_type))
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
