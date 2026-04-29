use std::path::{Path, PathBuf};

use gpt::GptConfig;
use gpt::disk::LogicalBlockSize;

use crate::disk::OsPartitionInfo;
use crate::os_install::partition_for;
use crate::os_install::quiesce::{quiesce_disk, update_partition_table};
use crate::prelude::*;

/// Target size for the OS root (rootfs) partition. We try to allocate this much.
const ROOT_TARGET_BYTES: u64 = 14 * 1024 * 1024 * 1024;
/// Minimum size we will accept for the OS root partition before erroring out. If a
/// protected data partition's `first_lba` sits inside the planned root region we will
/// shrink root down to (but not below) this floor.
const ROOT_MIN_BYTES: u64 = 12 * 1024 * 1024 * 1024;

pub async fn partition(
    disk_path: &Path,
    capacity: u64,
    protect: Option<&Path>,
    use_efi: bool,
) -> Result<OsPartitionInfo, Error> {
    // Guard: cannot protect the whole disk
    if let Some(p) = protect {
        if p == disk_path {
            return Err(Error::new(
                eyre!(
                    "Cannot protect the entire disk {}; must specify a partition",
                    disk_path.display()
                ),
                crate::ErrorKind::InvalidRequest,
            ));
        }
    }

    // Drop every kernel-side reference to partitions on this disk before rewriting the
    // table. Quiesce is best-effort; partx --update afterwards is what actually makes
    // the kernel see the new layout, and it tolerates a still-busy protected partition.
    quiesce_disk(disk_path).await?;

    let disk_path = disk_path.to_owned();
    let disk_path_clone = disk_path.clone();
    let protect = protect.map(|p| p.to_owned());
    let (efi, data_part) = tokio::task::spawn_blocking(move || {
        let disk_path = disk_path_clone;

        let protected_partition_info: Option<(u64, u64, PathBuf)> =
            if let Some(ref protect_path) = protect {
                let existing_gpt = GptConfig::new()
                    .writable(false)
                    .logical_block_size(LogicalBlockSize::Lb512)
                    .open_from_device(Box::new(
                        std::fs::File::options().read(true).open(&disk_path)?,
                    ))?;
                let info = existing_gpt
                    .partitions()
                    .iter()
                    .find(|(num, _)| partition_for(&disk_path, **num) == *protect_path)
                    .map(|(_, p)| (p.first_lba, p.last_lba, protect_path.clone()));
                if info.is_none() {
                    return Err(Error::new(
                        eyre!(
                            "Protected partition {} not found in GPT on {}",
                            protect_path.display(),
                            disk_path.display()
                        ),
                        crate::ErrorKind::NotFound,
                    ));
                }
                info
            } else {
                None
            };

        let mut device = Box::new(
            std::fs::File::options()
                .read(true)
                .write(true)
                .open(&disk_path)?,
        );

        let mbr = gpt::mbr::ProtectiveMBR::with_lb_size(
            u32::try_from((capacity / 512) - 1).unwrap_or(0xFF_FF_FF_FF),
        );
        mbr.overwrite_lba0(&mut device)?;
        let mut gpt = GptConfig::new()
            .writable(true)
            .logical_block_size(LogicalBlockSize::Lb512)
            .create_from_device(device, None)?;

        gpt.update_partitions(Default::default())?;

        let efi = if use_efi {
            gpt.add_partition("efi", 100 * 1024 * 1024, gpt::partition_types::EFI, 0, None)?;
            true
        } else {
            gpt.add_partition(
                "bios-grub",
                8 * 1024 * 1024,
                gpt::partition_types::BIOS,
                0,
                None,
            )?;
            false
        };
        gpt.add_partition(
            "boot",
            2 * 1024 * 1024 * 1024,
            gpt::partition_types::LINUX_FS,
            0,
            None,
        )?;

        // Compute the root size, shrinking it within [ROOT_MIN_BYTES, ROOT_TARGET_BYTES]
        // when a protected partition's `first_lba` would otherwise be overwritten. This
        // tolerates a few MiB of geometry drift on legacy hardware where the protected
        // partition isn't quite aligned the way the gpt crate would lay it out today.
        let lb_size: u64 = (*gpt.logical_block_size()).into();
        let target_root_lba = ROOT_TARGET_BYTES.div_ceil(lb_size);
        let min_root_lba = ROOT_MIN_BYTES.div_ceil(lb_size);
        let root_size_bytes = if let Some((protect_first_lba, _, ref path)) =
            protected_partition_info
        {
            // The next add_partition() will pick the lowest free block that fits, so the
            // earliest free LBA is where root would land. Find it, and clamp root's
            // length so it ends strictly before the protected partition.
            let earliest_free_start = gpt
                .find_free_sectors()
                .into_iter()
                .filter(|(start, _)| *start < protect_first_lba)
                .map(|(start, _)| start)
                .min()
                .ok_or_else(|| {
                    Error::new(
                        eyre!("No free space left on device for OS root partition"),
                        crate::ErrorKind::BlockDevice,
                    )
                })?;
            let available_lba = protect_first_lba.saturating_sub(earliest_free_start);
            if available_lba < min_root_lba {
                let available_bytes = available_lba.saturating_mul(lb_size);
                return Err(Error::new(
                    eyre!(
                        concat!(
                            "Protected partition {} starts at sector {}, leaving only ",
                            "{} bytes for the OS root partition (minimum is {} bytes)"
                        ),
                        path.display(),
                        protect_first_lba,
                        available_bytes,
                        ROOT_MIN_BYTES
                    ),
                    crate::ErrorKind::DiskManagement,
                ));
            }
            available_lba.min(target_root_lba).saturating_mul(lb_size)
        } else {
            ROOT_TARGET_BYTES
        };
        gpt.add_partition(
            "root",
            root_size_bytes,
            match crate::ARCH {
                "x86_64" => gpt::partition_types::LINUX_ROOT_X64,
                "aarch64" => gpt::partition_types::LINUX_ROOT_ARM_64,
                _ => gpt::partition_types::LINUX_FS,
            },
            0,
            None,
        )?;

        let data_part = if let Some((first_lba, last_lba, path)) = protected_partition_info {
            // Re-create the data partition entry at the same location
            let length_lba = last_lba - first_lba + 1;
            let next_id = gpt.partitions().keys().max().map(|k| k + 1).unwrap_or(1);
            gpt.add_partition_at(
                "data",
                next_id,
                first_lba,
                length_lba,
                gpt::partition_types::LINUX_LVM,
                0,
            )?;
            Some(path)
        } else {
            gpt.add_partition(
                "data",
                gpt.find_free_sectors()
                    .iter()
                    .map(|(_, size)| *size * u64::from(*gpt.logical_block_size()))
                    .max()
                    .ok_or_else(|| {
                        Error::new(
                            eyre!("No free space left on device"),
                            crate::ErrorKind::BlockDevice,
                        )
                    })?,
                gpt::partition_types::LINUX_LVM,
                0,
                None,
            )?;
            gpt.partitions()
                .last_key_value()
                .map(|(num, _)| partition_for(&disk_path, *num))
        };

        gpt.write()?;

        Ok::<_, Error>((efi, data_part))
    })
    .await
    .unwrap()?;

    // Make the kernel see the new layout via per-entry BLKPG ioctls. This works even
    // when a protected partition on this disk is still claimed by something we
    // couldn't tear down in quiesce_disk, because partx updates entries individually
    // rather than asking the kernel to re-read the whole table.
    update_partition_table(&disk_path).await?;

    let mut extra_boot = std::collections::BTreeMap::new();
    let bios;
    if efi {
        extra_boot.insert("efi".to_string(), partition_for(&disk_path, 1));
        bios = None;
    } else {
        bios = Some(partition_for(&disk_path, 1));
    }
    Ok(OsPartitionInfo {
        bios,
        boot: partition_for(&disk_path, 2),
        root: partition_for(&disk_path, 3),
        extra_boot,
        data: data_part,
    })
}
