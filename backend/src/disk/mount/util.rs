use std::path::Path;

use tracing::instrument;

use crate::util::Invoke;
use crate::{Error, ResultExt};

#[instrument(skip(src, dst))]
pub async fn bind<P0: AsRef<Path>, P1: AsRef<Path>>(
    src: P0,
    dst: P1,
    read_only: bool,
) -> Result<(), Error> {
    tracing::info!(
        "Binding {} to {}",
        src.as_ref().display(),
        dst.as_ref().display()
    );
    let is_mountpoint = tokio::process::Command::new("mountpoint")
        .arg(dst.as_ref())
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .await?;
    if is_mountpoint.success() {
        unmount(dst.as_ref()).await?;
    }
    tokio::fs::create_dir_all(&src).await?;
    tokio::fs::create_dir_all(&dst).await?;
    let mut mount_cmd = tokio::process::Command::new("mount");
    mount_cmd.arg("--bind");
    if read_only {
        mount_cmd.arg("-o").arg("ro");
    }
    mount_cmd
        .arg(src.as_ref())
        .arg(dst.as_ref())
        .invoke(crate::ErrorKind::Filesystem)
        .await?;
    Ok(())
}

#[instrument(skip(mountpoint))]
pub async fn unmount<P: AsRef<Path>>(mountpoint: P) -> Result<(), Error> {
    tracing::debug!("Unmounting {}.", mountpoint.as_ref().display());
    tokio::process::Command::new("umount")
        .arg("-l")
        .arg(mountpoint.as_ref())
        .invoke(crate::ErrorKind::Filesystem)
        .await?;
    match tokio::fs::remove_dir(mountpoint.as_ref()).await {
        Err(e) if e.raw_os_error() == Some(39) => Ok(()), // directory not empty
        a => a,
    }
    .with_ctx(|_| {
        (
            crate::ErrorKind::Filesystem,
            format!("rm {}", mountpoint.as_ref().display()),
        )
    })?;
    Ok(())
}
