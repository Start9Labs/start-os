use std::path::Path;
use std::sync::Arc;

use rpc_toolkit::command;
use rpc_toolkit::yajrc::RpcError;

use crate::context::DiagnosticContext;
use crate::logs::{display_logs, fetch_logs, LogResponse, LogSource};
use crate::shutdown::Shutdown;
use crate::util::display_none;
use crate::Error;

pub const SYSTEMD_UNIT: &'static str = "embassy-init";

#[command(subcommands(error, logs, exit, restart, forget_disk))]
pub fn diagnostic() -> Result<(), Error> {
    Ok(())
}

#[command]
pub fn error(#[context] ctx: DiagnosticContext) -> Result<Arc<RpcError>, Error> {
    Ok(ctx.error.clone())
}

#[command(display(display_logs))]
pub async fn logs(
    #[arg] limit: Option<usize>,
    #[arg] cursor: Option<String>,
    #[arg] before_flag: Option<bool>,
) -> Result<LogResponse, Error> {
    Ok(fetch_logs(
        LogSource::Service(SYSTEMD_UNIT),
        limit,
        cursor,
        before_flag.unwrap_or(false),
    )
    .await?)
}

#[command(display(display_none))]
pub fn exit(#[context] ctx: DiagnosticContext) -> Result<(), Error> {
    ctx.shutdown.send(None).expect("receiver dropped");
    Ok(())
}

#[command(display(display_none))]
pub fn restart(#[context] ctx: DiagnosticContext) -> Result<(), Error> {
    ctx.shutdown
        .send(Some(Shutdown {
            zfs_pool: ctx.zfs_pool_name.clone(),
            db_handle: None,
            restart: true,
        }))
        .expect("receiver dropped");
    Ok(())
}

#[command(rename = "forget-disk", display(display_none))]
pub async fn forget_disk() -> Result<(), Error> {
    let disk_guid = Path::new("/embassy-os/disk.guid");
    if tokio::fs::metadata(disk_guid).await.is_ok() {
        tokio::fs::remove_file(disk_guid).await?;
    }
    Ok(())
}
