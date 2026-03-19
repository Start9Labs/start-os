use std::path::{Path, PathBuf};

use gpt::GptConfig;
use gpt::disk::LogicalBlockSize;
use tokio::process::Command;

use crate::disk::OsPartitionInfo;
use crate::os_install::partition_for;
use crate::prelude::*;
use crate::util::Invoke;

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
        gpt.add_partition(
            "root",
            14 * 1024 * 1024 * 1024,
            match crate::ARCH {
                "x86_64" => gpt::partition_types::LINUX_ROOT_X64,
                "aarch64" => gpt::partition_types::LINUX_ROOT_ARM_64,
                _ => gpt::partition_types::LINUX_FS,
            },
            0,
            None,
        )?;

        // Check if protected partition would be overwritten by OS partitions
        if let Some((first_lba, _, ref path)) = protected_partition_info {
            // Get the actual end sector of the last OS partition (root = partition 3)
            let os_partitions_end_sector =
                gpt.partitions().get(&3).map(|p| p.last_lba).unwrap_or(0);
            if first_lba <= os_partitions_end_sector {
                return Err(Error::new(
                    eyre!(
                        concat!(
                            "Protected partition {} starts at sector {}",
                            " which would be overwritten by OS partitions ending at sector {}"
                        ),
                        path.display(),
                        first_lba,
                        os_partitions_end_sector
                    ),
                    crate::ErrorKind::DiskManagement,
                ));
            }
        }

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

    // Re-read partition table and wait for udev to create device nodes
    Command::new("vgchange")
        .arg("-an")
        .invoke(crate::ErrorKind::DiskManagement)
        .await
        .ok();
    Command::new("dmsetup")
        .arg("remove_all")
        .arg("--force")
        .invoke(crate::ErrorKind::DiskManagement)
        .await
        .ok();
    // BLKRRPART can fail with "Device or resource busy" if the kernel still
    // holds references to old partitions.  Retry a few times with a delay.
    let mut last_err = None;
    for attempt in 0..5u32 {
        if attempt > 0 {
            tokio::time::sleep(std::time::Duration::from_secs(1)).await;
        }
        match Command::new("blockdev")
            .arg("--rereadpt")
            .arg(&disk_path)
            .invoke(crate::ErrorKind::DiskManagement)
            .await
        {
            Ok(_) => {
                last_err = None;
                break;
            }
            Err(e) => {
                tracing::warn!("blockdev --rereadpt attempt {} failed: {e}", attempt + 1);
                last_err = Some(e);
            }
        }
    }
    if let Some(e) = last_err {
        return Err(e);
    }
    Command::new("udevadm")
        .arg("settle")
        .invoke(crate::ErrorKind::DiskManagement)
        .await?;

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
