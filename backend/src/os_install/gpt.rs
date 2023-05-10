use std::path::Path;

use color_eyre::eyre::eyre;
use gpt::disk::LogicalBlockSize;
use gpt::GptConfig;

use crate::disk::util::DiskInfo;
use crate::disk::OsPartitionInfo;
use crate::os_install::partition_for;
use crate::Error;

pub async fn partition(disk: &DiskInfo, overwrite: bool) -> Result<OsPartitionInfo, Error> {
    let efi = {
        let disk = disk.clone();
        tokio::task::spawn_blocking(move || {
            let use_efi = Path::new("/sys/firmware/efi").exists();
            let mut device = Box::new(
                std::fs::File::options()
                    .read(true)
                    .write(true)
                    .open(&disk.logicalname)?,
            );
            let (mut gpt, guid_part) = if overwrite {
                let mbr = gpt::mbr::ProtectiveMBR::with_lb_size(
                    u32::try_from((disk.capacity / 512) - 1).unwrap_or(0xFF_FF_FF_FF),
                );
                mbr.overwrite_lba0(&mut device)?;
                (
                    GptConfig::new()
                        .writable(true)
                        .initialized(false)
                        .logical_block_size(LogicalBlockSize::Lb512)
                        .create_from_device(device, None)?,
                    None,
                )
            } else {
                let gpt = GptConfig::new()
                    .writable(true)
                    .initialized(true)
                    .logical_block_size(LogicalBlockSize::Lb512)
                    .open_from_device(device)?;
                let mut guid_part = None;
                for (idx, part_info) in disk
                    .partitions
                    .iter()
                    .enumerate()
                    .map(|(idx, x)| (idx + 1, x))
                {
                    if let Some(entry) = gpt.partitions().get(&(idx as u32)) {
                        if entry.first_lba >= if use_efi { 33759266 } else { 33570850 } {
                            if idx < 4 {
                                guid_part = Some(entry.clone())
                            }
                            break;
                        }
                        if part_info.guid.is_some() {
                            return Err(Error::new(
                                eyre!("Not enough space before embassy data"),
                                crate::ErrorKind::InvalidRequest,
                            ));
                        }
                    }
                }
                (gpt, guid_part)
            };

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
                1024 * 1024 * 1024,
                gpt::partition_types::LINUX_FS,
                0,
                None,
            )?;
            gpt.add_partition(
                "root",
                15 * 1024 * 1024 * 1024,
                match *crate::ARCH {
                    "x86_64" => gpt::partition_types::LINUX_ROOT_X64,
                    "aarch64" => gpt::partition_types::LINUX_ROOT_ARM_64,
                    _ => gpt::partition_types::LINUX_FS,
                },
                0,
                None,
            )?;

            if overwrite {
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
            } else if let Some(guid_part) = guid_part {
                let mut parts = gpt.partitions().clone();
                parts.insert(gpt.find_next_partition_id(), guid_part);
                gpt.update_partitions(parts)?;
            }

            gpt.write()?;

            Ok(efi)
        })
        .await
        .unwrap()?
    };

    Ok(OsPartitionInfo {
        efi: efi.then(|| partition_for(&disk.logicalname, 1)),
        bios: (!efi).then(|| partition_for(&disk.logicalname, 1)),
        boot: partition_for(&disk.logicalname, 2),
        root: partition_for(&disk.logicalname, 3),
    })
}
