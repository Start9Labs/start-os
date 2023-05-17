use color_eyre::eyre::eyre;
use mbrman::{MBRPartitionEntry, CHS, MBR};

use crate::disk::util::DiskInfo;
use crate::disk::OsPartitionInfo;
use crate::os_install::partition_for;
use crate::prelude::*;

pub async fn partition(disk: &DiskInfo, overwrite: bool) -> Result<OsPartitionInfo, Error> {
    {
        let sectors = (disk.capacity / 512) as u32;
        let disk = disk.clone();
        tokio::task::spawn_blocking(move || {
            let mut file = std::fs::File::options()
                .read(true)
                .write(true)
                .open(&disk.logicalname)?;
            let (mut mbr, guid_part) = if overwrite {
                (MBR::new_from(&mut file, 512, rand::random())?, None)
            } else {
                let mut mbr = MBR::read_from(&mut file, 512)?;
                let mut guid_part = None;
                for (idx, part_info) in disk
                    .partitions
                    .iter()
                    .enumerate()
                    .map(|(idx, x)| (idx + 1, x))
                {
                    if let Some(entry) = mbr.get_mut(idx) {
                        if part_info.guid.is_some() {
                            if entry.starting_lba < 33556480 {
                                return Err(Error::new(
                                    eyre!("Not enough space before embassy data"),
                                    ErrorKind::InvalidRequest,
                                ));
                            }
                            guid_part = Some(std::mem::replace(entry, MBRPartitionEntry::empty()));
                        }
                        *entry = MBRPartitionEntry::empty();
                    }
                }
                (mbr, guid_part)
            };

            mbr[1] = MBRPartitionEntry {
                boot: 0x80,
                first_chs: CHS::empty(),
                sys: 0x0b,
                last_chs: CHS::empty(),
                starting_lba: 2048,
                sectors: 2099200 - 2048,
            };
            mbr[2] = MBRPartitionEntry {
                boot: 0,
                first_chs: CHS::empty(),
                sys: 0x83,
                last_chs: CHS::empty(),
                starting_lba: 2099200,
                sectors: 33556480 - 2099200,
            };

            if overwrite {
                mbr[3] = MBRPartitionEntry {
                    boot: 0,
                    first_chs: CHS::empty(),
                    sys: 0x8e,
                    last_chs: CHS::empty(),
                    starting_lba: 33556480,
                    sectors: sectors - 33556480,
                }
            } else if let Some(guid_part) = guid_part {
                mbr[3] = guid_part;
            }
            mbr.write_into(&mut file)?;

            Ok(())
        })
        .await
        .unwrap()?;
    }

    Ok(OsPartitionInfo {
        efi: None,
        bios: None,
        boot: partition_for(&disk.logicalname, 1),
        root: partition_for(&disk.logicalname, 2),
    })
}
