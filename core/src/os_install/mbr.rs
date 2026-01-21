use std::path::{Path, PathBuf};

use color_eyre::eyre::eyre;
use mbrman::{CHS, MBR, MBRPartitionEntry};

use crate::disk::OsPartitionInfo;
use crate::os_install::partition_for;
use crate::prelude::*;

pub async fn partition(
    disk_path: &Path,
    capacity: u64,
    protect: Option<&Path>,
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
    let sectors = (capacity / 512) as u32;
    let data_part = tokio::task::spawn_blocking(move || {
        let disk_path = disk_path_clone;

        // If protecting a partition, read its location from the existing MBR
        let protected_partition_info: Option<(u32, u32, PathBuf)> =
            if let Some(ref protect_path) = protect {
                let mut file = std::fs::File::options().read(true).open(&disk_path)?;
                let existing_mbr = MBR::read_from(&mut file, 512)?;
                // Find the partition matching the protected path (check partitions 1-4)
                let info = (1..=4u32)
                    .find(|&idx| partition_for(&disk_path, idx) == *protect_path)
                    .and_then(|idx| {
                        let entry = &existing_mbr[idx as usize];
                        if entry.sectors > 0 {
                            Some((entry.starting_lba, entry.sectors, protect_path.clone()))
                        } else {
                            None
                        }
                    });
                if info.is_none() {
                    return Err(Error::new(
                        eyre!(
                            "Protected partition {} not found in MBR on {}",
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

        // MBR partition layout:
        // Partition 1 (boot): starts at 2048, ends at 4196352 (sectors: 4194304 = 2GB)
        // Partition 2 (root): starts at 4196352, ends at 33556480 (sectors: 29360128 = 14GB)
        // OS partitions end at sector 33556480
        let os_partitions_end_sector: u32 = 33556480;

        // Check if protected partition would be overwritten
        if let Some((starting_lba, _, ref path)) = protected_partition_info {
            if starting_lba < os_partitions_end_sector {
                return Err(Error::new(
                    eyre!(
                        concat!(
                            "Protected partition {} starts at sector {}",
                            " which would be overwritten by OS partitions ending at sector {}"
                        ),
                        path.display(),
                        starting_lba,
                        os_partitions_end_sector
                    ),
                    crate::ErrorKind::DiskManagement,
                ));
            }
        }

        let mut file = std::fs::File::options()
            .read(true)
            .write(true)
            .open(&disk_path)?;
        let mut mbr = MBR::new_from(&mut file, 512, rand::random())?;

        mbr[1] = MBRPartitionEntry {
            boot: 0x80,
            first_chs: CHS::empty(),
            sys: 0x0b,
            last_chs: CHS::empty(),
            starting_lba: 2048,
            sectors: 4196352 - 2048,
        };
        mbr[2] = MBRPartitionEntry {
            boot: 0,
            first_chs: CHS::empty(),
            sys: 0x83,
            last_chs: CHS::empty(),
            starting_lba: 4196352,
            sectors: 33556480 - 4196352,
        };

        let data_part = if let Some((starting_lba, part_sectors, path)) = protected_partition_info {
            // Re-create the data partition entry at the same location
            mbr[3] = MBRPartitionEntry {
                boot: 0,
                first_chs: CHS::empty(),
                sys: 0x8e,
                last_chs: CHS::empty(),
                starting_lba,
                sectors: part_sectors,
            };
            Some(path)
        } else {
            mbr[3] = MBRPartitionEntry {
                boot: 0,
                first_chs: CHS::empty(),
                sys: 0x8e,
                last_chs: CHS::empty(),
                starting_lba: 33556480,
                sectors: sectors - 33556480,
            };
            Some(partition_for(&disk_path, 3))
        };
        mbr.write_into(&mut file)?;

        Ok::<_, Error>(data_part)
    })
    .await
    .unwrap()?;

    Ok(OsPartitionInfo {
        efi: None,
        bios: None,
        boot: partition_for(&disk_path, 1),
        root: partition_for(&disk_path, 2),
        data: data_part,
    })
}
