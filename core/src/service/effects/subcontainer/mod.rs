use std::path::{Path, PathBuf};

use imbl_value::InternedString;
use tokio::process::Command;

use crate::ImageId;
use crate::disk::mount::filesystem::bind::Bind;
use crate::disk::mount::filesystem::overlayfs::OverlayGuard;
use crate::disk::mount::filesystem::ReadOnly;
use crate::disk::mount::guard::{GenericMountGuard, MountGuard, TMP_MOUNTPOINT};
use crate::rpc_continuations::Guid;
use crate::service::effects::prelude::*;
use crate::service::persistent_container::Subcontainer;
use crate::util::Invoke;

#[cfg(target_os = "linux")]
mod sync;

#[cfg(not(target_os = "linux"))]
mod sync_dummy;

pub use sync::*;
#[cfg(not(target_os = "linux"))]
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
        #[cfg(target_os = "linux")]
        if tokio::fs::metadata(overlay.overlay.path().join("proc/1"))
            .await
            .is_ok()
        {
            let procfs = context
                .seed
                .persistent_container
                .lxc_container
                .get()
                .or_not_found("lxc container")?
                .rootfs_dir()
                .join("proc");
            let overlay_path = overlay.overlay.path().to_owned();
            tokio::task::spawn_blocking(move || sync::kill_init(&procfs, &overlay_path))
                .await
                .with_kind(ErrorKind::Unknown)??;
        }
        overlay.overlay.unmount(true).await?;
    } else {
        tracing::warn!(
            "Could not find a subcontainer fs to destroy; assumming that it already is destroyed and will be skipping"
        );
    }
    Ok(())
}

#[derive(Debug, Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct CreateSubcontainerFsParams {
    image_id: ImageId,
    #[ts(type = "string | null")]
    name: Option<InternedString>,
}
#[instrument(skip_all)]
pub async fn create_subcontainer_fs(
    context: EffectContext,
    CreateSubcontainerFsParams { image_id, name }: CreateSubcontainerFsParams,
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
        let final_mountpoint = rootfs_dir
            .join("media/startos/subcontainers")
            .join(guid.as_ref());
        tokio::fs::create_dir_all(&final_mountpoint).await?;
        let container_mountpoint = Path::new("/").join(
            final_mountpoint
                .strip_prefix(&rootfs_dir)
                .with_kind(ErrorKind::Incoherent)?,
        );

        let nvidia_container = context
            .seed
            .persistent_container
            .s9pk
            .as_manifest()
            .images
            .get(&image_id)
            .map_or(false, |i| i.nvidia_container);

        // If nvidia_container is enabled, we need to stage the overlay outside the LXC rootfs
        // to safely mount /proc for nvidia-container-cli without exposing it to the container
        let overlay = if nvidia_container {
            // Create staging directory outside LXC rootfs
            let staging_dir = Path::new(TMP_MOUNTPOINT)
                .join("nvidia-staging")
                .join(guid.as_ref());
            tokio::fs::create_dir_all(&staging_dir).await?;

            tracing::info!("Mounting overlay {guid} for {image_id} at staging location");
            let mut overlay = OverlayGuard::mount(image, &staging_dir).await?;

            // Mount /proc temporarily for nvidia-container-cli (outside LXC rootfs)
            let staging_proc = staging_dir.join("proc");
            tokio::fs::create_dir_all(&staging_proc).await?;
            let proc_mount = MountGuard::mount(&Bind::new("/proc"), &staging_proc, ReadOnly).await?;

            // Read environment variables from the image's env file
            let env_file = rootfs_dir
                .join("media/startos/images")
                .join(image_id.as_ref())
                .with_extension("env");
            let env_content = tokio::fs::read_to_string(&env_file)
                .await
                .unwrap_or_default();

            // Build nvidia-container-cli command with environment variables
            let mut cmd = Command::new("nvidia-container-cli");
            cmd.arg("configure")
                .arg("--no-cgroups")
                .arg("--utility")
                .arg("--compute")
                .arg("--graphics")
                .arg("--video");

            // Pass NVIDIA_* environment variables to nvidia-container-cli
            for line in env_content.lines() {
                if let Some((key, value)) = line.split_once('=') {
                    if key.starts_with("NVIDIA_") {
                        cmd.env(key, value);
                    }
                }
            }

            cmd.arg(&staging_dir);

            tracing::info!("Running nvidia-container-cli for {image_id}");
            cmd.invoke(ErrorKind::Unknown).await?;

            // Unmount /proc
            proc_mount.unmount(false).await?;
            tracing::info!("nvidia-container-cli completed for {image_id}");

            // Remount overlay at final location inside LXC rootfs
            tracing::info!("Remounting overlay {guid} to final location");
            overlay.remount(&final_mountpoint).await?;

            // Clean up staging directory
            tokio::fs::remove_dir_all(&staging_dir).await.ok();

            overlay
        } else {
            tracing::info!("Mounting overlay {guid} for {image_id}");
            OverlayGuard::mount(image, &final_mountpoint).await?
        };

        let subcontainer_wrapper = Subcontainer {
            overlay,
            name: name
                .unwrap_or_else(|| InternedString::intern(format!("subcontainer-{}", image_id))),
            image_id: image_id.clone(),
        };

        Command::new("chown")
            .arg("100000:100000")
            .arg(&final_mountpoint)
            .invoke(ErrorKind::Filesystem)
            .await?;
        tracing::info!("Mounted overlay {guid} for {image_id}");
        context
            .seed
            .persistent_container
            .subcontainers
            .lock()
            .await
            .insert(guid.clone(), subcontainer_wrapper);
        Ok((container_mountpoint, guid))
    } else {
        Err(Error::new(
            eyre!("image {image_id} not found in s9pk"),
            ErrorKind::NotFound,
        ))
    }
}
