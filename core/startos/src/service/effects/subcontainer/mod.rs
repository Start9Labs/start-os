use std::path::{Path, PathBuf};

use models::ImageId;
use tokio::process::Command;

use crate::disk::mount::filesystem::overlayfs::OverlayGuard;
use crate::rpc_continuations::Guid;
use crate::service::effects::prelude::*;
use crate::util::Invoke;

#[cfg(feature = "container-runtime")]
mod sync;

#[cfg(not(feature = "container-runtime"))]
mod sync_dummy;

pub use sync::*;
#[cfg(not(feature = "container-runtime"))]
use sync_dummy as sync;

#[derive(Debug, Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct DestroySubcontainerFsParams {
    guid: Guid,
}
#[instrument(skip_all)]
pub async fn destroy_subcontainer_fs(
    context: EffectContext,
    DestroySubcontainerFsParams { guid }: DestroySubcontainerFsParams,
) -> Result<(), Error> {
    let context = context.deref()?;
    if let Some(overlay) = context
        .seed
        .persistent_container
        .subcontainers
        .lock()
        .await
        .remove(&guid)
    {
        overlay.unmount(true).await?;
    } else {
        tracing::warn!("Could not find a subcontainer fs to destroy; assumming that it already is destroyed and will be skipping");
    }
    Ok(())
}

#[derive(Debug, Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct CreateSubcontainerFsParams {
    image_id: ImageId,
}
#[instrument(skip_all)]
pub async fn create_subcontainer_fs(
    context: EffectContext,
    CreateSubcontainerFsParams { image_id }: CreateSubcontainerFsParams,
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
            .join("media/startos/subcontainers")
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
            .subcontainers
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
