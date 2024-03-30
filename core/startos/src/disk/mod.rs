use std::path::{Path, PathBuf};

use rpc_toolkit::{from_fn_async, AnyContext, Empty, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};

use crate::context::{CliContext, RpcContext};
use crate::disk::util::DiskInfo;
use crate::util::serde::{display_serializable, HandlerExtSerde, WithIoFormat};
use crate::Error;

pub mod fsck;
pub mod main;
pub mod mount;
pub mod util;

pub const BOOT_RW_PATH: &str = "/media/boot-rw";
pub const REPAIR_DISK_PATH: &str = "/media/embassy/config/repair-disk";

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct OsPartitionInfo {
    pub efi: Option<PathBuf>,
    pub bios: Option<PathBuf>,
    pub boot: PathBuf,
    pub root: PathBuf,
}
impl OsPartitionInfo {
    pub fn contains(&self, logicalname: impl AsRef<Path>) -> bool {
        self.efi
            .as_ref()
            .map(|p| p == logicalname.as_ref())
            .unwrap_or(false)
            || self
                .bios
                .as_ref()
                .map(|p| p == logicalname.as_ref())
                .unwrap_or(false)
            || &*self.boot == logicalname.as_ref()
            || &*self.root == logicalname.as_ref()
    }
}

pub fn disk() -> ParentHandler {
    ParentHandler::new()
        .subcommand(
            "list",
            from_fn_async(list)
                .with_display_serializable()
                .with_custom_display_fn::<AnyContext, _>(|handle, result| {
                    Ok(display_disk_info(handle.params, result))
                })
                .with_remote_cli::<CliContext>(),
        )
        .subcommand(
            "repair",
            from_fn_async(repair)
                .no_display()
                .with_remote_cli::<CliContext>(),
        )
}

fn display_disk_info(params: WithIoFormat<Empty>, args: Vec<DiskInfo>) {
    use prettytable::*;

    if let Some(format) = params.format {
        return display_serializable(format, args);
    }

    let mut table = Table::new();
    table.add_row(row![bc =>
        "LOGICALNAME",
        "LABEL",
        "CAPACITY",
        "USED",
        "STARTOS VERSION"
    ]);
    for disk in args {
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
                if let Some(eos) = part.start_os.as_ref() {
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

// #[command(display(display_disk_info))]
pub async fn list(ctx: RpcContext, _: Empty) -> Result<Vec<DiskInfo>, Error> {
    crate::disk::util::list(&ctx.os_partitions).await
}

pub async fn repair() -> Result<(), Error> {
    tokio::fs::write(REPAIR_DISK_PATH, b"").await?;
    Ok(())
}
