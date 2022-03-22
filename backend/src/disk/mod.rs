use clap::ArgMatches;
use rpc_toolkit::command;

use self::util::DiskListResponse;
use crate::util::display_none;
use crate::util::serde::{display_serializable, IoFormat};
use crate::Error;

pub mod fsck;
pub mod main;
pub mod mount;
pub mod quirks;
pub mod util;

pub const BOOT_RW_PATH: &str = "/media/boot-rw";
pub const REPAIR_DISK_PATH: &str = "/embassy-os/repair-disk";

#[command(subcommands(list, repair))]
pub fn disk() -> Result<(), Error> {
    Ok(())
}

fn display_disk_info(info: DiskListResponse, matches: &ArgMatches<'_>) {
    use prettytable::*;

    if matches.is_present("format") {
        return display_serializable(info, matches);
    }

    let mut table = Table::new();
    table.add_row(row![bc =>
        "LOGICALNAME",
        "LABEL",
        "CAPACITY",
        "USED",
        "EMBASSY OS VERSION"
    ]);
    for disk in info.disks {
        let row = row![
            disk.logicalname.display(),
            "N/A",
            &format!("{:.2} GiB", disk.capacity as f64 / 1024.0 / 1024.0 / 1024.0),
            "N/A",
            "N/A",
        ];
        table.add_row(row);
        for part in disk.partitions {
            let row = row![
                part.logicalname.display(),
                if let Some(label) = part.label.as_ref() {
                    label
                } else {
                    "N/A"
                },
                part.capacity,
                if let Some(used) = part
                    .used
                    .map(|u| format!("{:.2} GiB", u as f64 / 1024.0 / 1024.0 / 1024.0))
                    .as_ref()
                {
                    used
                } else {
                    "N/A"
                },
                if let Some(eos) = part.embassy_os.as_ref() {
                    eos.version.as_str()
                } else {
                    "N/A"
                },
            ];
            table.add_row(row);
        }
    }
    table.print_tty(false);
}

#[command(display(display_disk_info))]
pub async fn list(
    #[allow(unused_variables)]
    #[arg]
    format: Option<IoFormat>,
) -> Result<DiskListResponse, Error> {
    crate::disk::util::list().await
}

#[command(display(display_none))]
pub async fn repair() -> Result<(), Error> {
    tokio::fs::write(REPAIR_DISK_PATH, b"").await?;
    Ok(())
}
