use std::path::Path;

use tracing::instrument;

use crate::util::Invoke;
use crate::Error;

pub async fn is_mountpoint(path: impl AsRef<Path>) -> Result<bool, Error> {
    let is_mountpoint = tokio::process::Command::new("mountpoint")
        .arg(path.as_ref())
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .await?;
    Ok(is_mountpoint.success())
}

#[instrument(skip_all)]
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
    if is_mountpoint(&dst).await? {
        unmount(dst.as_ref(), true).await?;
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

#[instrument(skip_all)]
pub async fn unmount<P: AsRef<Path>>(mountpoint: P, lazy: bool) -> Result<(), Error> {
    tracing::debug!("Unmounting {}.", mountpoint.as_ref().display());
    let mut cmd = tokio::process::Command::new("umount");
    cmd.arg("-R");
    if lazy {
        cmd.arg("-l");
    }
    cmd.arg(mountpoint.as_ref())
        .invoke(crate::ErrorKind::Filesystem)
        .await?;
    Ok(())
}
