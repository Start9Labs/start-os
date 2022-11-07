use std::path::{Path, PathBuf};

use clap::ArgMatches;
use rpc_toolkit::command;
use serde::{Deserialize, Serialize};

use crate::context::RpcContext;
use crate::disk::util::DiskInfo;
use crate::util::display_none;
use crate::util::serde::{display_serializable, IoFormat};
use crate::Error;

pub mod fsck;
pub mod main;
pub mod mount;
pub mod util;

pub const BOOT_RW_PATH: &str = "/media/boot-rw";
pub const REPAIR_DISK_PATH: &str = "/media/embassy/config/repair-disk";

#[derive(Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct OsPartitionInfo {
    pub boot: PathBuf,
    pub root: PathBuf,
}
impl OsPartitionInfo {
    pub fn contains(&self, logicalname: impl AsRef<Path>) -> bool {
        &*self.boot == logicalname.as_ref() || &*self.root == logicalname.as_ref()
    }
}

#[command(subcommands(list, repair))]
pub fn disk() -> Result<(), Error> {
    Ok(())
}

fn display_disk_info(info: Vec<DiskInfo>, matches: &ArgMatches) {
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
    for disk in info {
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
    table.print_tty(false).unwrap();
}

#[command(display(display_disk_info))]
pub async fn list(
    #[context] ctx: RpcContext,
    #[allow(unused_variables)]
    #[arg]
    format: Option<IoFormat>,
) -> Result<Vec<DiskInfo>, Error> {
    crate::disk::util::list(&ctx.os_partitions).await
}

#[command(display(display_none))]
pub async fn repair() -> Result<(), Error> {
    tokio::fs::write(REPAIR_DISK_PATH, b"").await?;
    Ok(())
}
