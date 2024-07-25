use std::ffi::OsString;
use std::os::unix::process::CommandExt;
use std::path::{Path, PathBuf};

use models::ImageId;
use rpc_toolkit::Context;
use tokio::process::Command;

use crate::disk::mount::filesystem::overlayfs::OverlayGuard;
use crate::rpc_continuations::Guid;
use crate::service::effects::prelude::*;
use crate::util::Invoke;

#[derive(Debug, Clone, Serialize, Deserialize, Parser)]
pub struct ChrootParams {
    #[arg(short = 'e', long = "env")]
    env: Option<PathBuf>,
    #[arg(short = 'w', long = "workdir")]
    workdir: Option<PathBuf>,
    #[arg(short = 'u', long = "user")]
    user: Option<String>,
    path: PathBuf,
    command: OsString,
    args: Vec<OsString>,
}
pub fn chroot<C: Context>(
    _: C,
    ChrootParams {
        env,
        workdir,
        user,
        path,
        command,
        args,
    }: ChrootParams,
) -> Result<(), Error> {
    let mut cmd = std::process::Command::new(command);
    if let Some(env) = env {
        for (k, v) in std::fs::read_to_string(env)?
            .lines()
            .map(|l| l.trim())
            .filter_map(|l| l.split_once("="))
        {
            cmd.env(k, v);
        }
    }
    nix::unistd::setsid().ok(); // https://stackoverflow.com/questions/25701333/os-setsid-operation-not-permitted
    std::os::unix::fs::chroot(path)?;
    if let Some(uid) = user.as_deref().and_then(|u| u.parse::<u32>().ok()) {
        cmd.uid(uid);
    } else if let Some(user) = user {
        let (uid, gid) = std::fs::read_to_string("/etc/passwd")?
            .lines()
            .find_map(|l| {
                let mut split = l.trim().split(":");
                if user != split.next()? {
                    return None;
                }
                split.next(); // throw away x
                Some((split.next()?.parse().ok()?, split.next()?.parse().ok()?))
                // uid gid
            })
            .or_not_found(lazy_format!("{user} in /etc/passwd"))?;
        cmd.uid(uid);
        cmd.gid(gid);
    };
    if let Some(workdir) = workdir {
        cmd.current_dir(workdir);
    }
    cmd.args(args);
    Err(cmd.exec().into())
}

#[derive(Debug, Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct DestroyOverlayedImageParams {
    guid: Guid,
}
#[instrument(skip_all)]
pub async fn destroy_overlayed_image(
    context: EffectContext,
    DestroyOverlayedImageParams { guid }: DestroyOverlayedImageParams,
) -> Result<(), Error> {
    let context = context.deref()?;
    if context
        .seed
        .persistent_container
        .overlays
        .lock()
        .await
        .remove(&guid)
        .is_none()
    {
        tracing::warn!("Could not find a guard to remove on the destroy overlayed image; assumming that it already is removed and will be skipping");
    }
    Ok(())
}

#[derive(Debug, Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct CreateOverlayedImageParams {
    image_id: ImageId,
}
#[instrument(skip_all)]
pub async fn create_overlayed_image(
    context: EffectContext,
    CreateOverlayedImageParams { image_id }: CreateOverlayedImageParams,
) -> Result<(PathBuf, Guid), Error> {
    let context = context.deref()?;
    if let Some(image) = context
        .seed
        .persistent_container
        .images
        .get(&image_id)
        .cloned()
    {
        let guid = Guid::new();
        let rootfs_dir = context
            .seed
            .persistent_container
            .lxc_container
            .get()
            .ok_or_else(|| {
                Error::new(
                    eyre!("PersistentContainer has been destroyed"),
                    ErrorKind::Incoherent,
                )
            })?
            .rootfs_dir();
        let mountpoint = rootfs_dir
            .join("media/startos/overlays")
            .join(guid.as_ref());
        tokio::fs::create_dir_all(&mountpoint).await?;
        let container_mountpoint = Path::new("/").join(
            mountpoint
                .strip_prefix(rootfs_dir)
                .with_kind(ErrorKind::Incoherent)?,
        );
        tracing::info!("Mounting overlay {guid} for {image_id}");
        let guard = OverlayGuard::mount(image, &mountpoint).await?;
        Command::new("chown")
            .arg("100000:100000")
            .arg(&mountpoint)
            .invoke(ErrorKind::Filesystem)
            .await?;
        tracing::info!("Mounted overlay {guid} for {image_id}");
        context
            .seed
            .persistent_container
            .overlays
            .lock()
            .await
            .insert(guid.clone(), guard);
        Ok((container_mountpoint, guid))
    } else {
        Err(Error::new(
            eyre!("image {image_id} not found in s9pk"),
            ErrorKind::NotFound,
        ))
    }
}
