use std::path::{Path, PathBuf};

use models::ImageId;
use tokio::process::Command;

use crate::disk::mount::filesystem::overlayfs::OverlayGuard;
use crate::rpc_continuations::Guid;
use crate::service::effects::prelude::*;
use crate::util::Invoke;

mod sync;

pub use sync::*;

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
    if let Some(overlay) = context
        .seed
        .persistent_container
        .overlays
        .lock()
        .await
        .remove(&guid)
    {
        overlay.unmount(true).await?;
    } else {
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
