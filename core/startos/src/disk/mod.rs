use std::path::{Path, PathBuf};

use itertools::Itertools;
use lazy_format::lazy_format;
use rpc_toolkit::{from_fn_async, CallRemoteHandler, Context, Empty, HandlerExt, ParentHandler};
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
pub const REPAIR_DISK_PATH: &str = "/media/startos/config/repair-disk";

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

pub fn disk<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "list",
            from_fn_async(list)
                .with_display_serializable()
                .with_custom_display_fn(|handle, result| {
                    Ok(display_disk_info(handle.params, result))
                })
                .with_about("List disk info")
                .with_call_remote::<CliContext>(),
        )
        .subcommand("repair", from_fn_async(|_: C| repair()).no_cli())
        .subcommand(
            "repair",
            CallRemoteHandler::<CliContext, _, _>::new(
                from_fn_async(|_: RpcContext| repair())
                    .no_display()
                    .with_about("Repair disk in the event of corruption"),
            ),
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
                &if part.start_os.is_empty() {
                    "N/A".to_owned()
                } else if part.start_os.len() == 1 {
                    part.start_os
                        .first_key_value()
                        .map(|(_, info)| info.version.to_string())
                        .unwrap()
                } else {
                    part.start_os
                        .iter()
                        .map(|(id, info)| lazy_format!("{} ({})", info.version, id))
                        .join(", ")
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
