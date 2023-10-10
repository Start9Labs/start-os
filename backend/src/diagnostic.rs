use std::path::Path;
use std::sync::Arc;

use rpc_toolkit::command;
use rpc_toolkit::yajrc::RpcError;

use crate::context::DiagnosticContext;
use crate::disk::repair;
use crate::init::SYSTEM_REBUILD_PATH;
use crate::logs::{fetch_logs, LogResponse, LogSource};
use crate::shutdown::Shutdown;
use crate::util::display_none;
use crate::Error;

#[command(subcommands(error, logs, exit, restart, forget_disk, disk, rebuild))]
pub fn diagnostic() -> Result<(), Error> {
    Ok(())
}

#[command]
pub fn error(#[context] ctx: DiagnosticContext) -> Result<Arc<RpcError>, Error> {
    Ok(ctx.error.clone())
}

#[command(rpc_only)]
pub async fn logs(
    #[arg] limit: Option<usize>,
    #[arg] cursor: Option<String>,
    #[arg] before: bool,
) -> Result<LogResponse, Error> {
    Ok(fetch_logs(LogSource::System, limit, cursor, before).await?)
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
            datadir: ctx.datadir.clone(),
            disk_guid: ctx.disk_guid.clone(),
            restart: true,
        }))
        .expect("receiver dropped");
    Ok(())
}

#[command(display(display_none))]
pub async fn rebuild(#[context] ctx: DiagnosticContext) -> Result<(), Error> {
    tokio::fs::write(SYSTEM_REBUILD_PATH, b"").await?;
    restart(ctx)
}

#[command(subcommands(forget_disk, repair))]
pub fn disk() -> Result<(), Error> {
    Ok(())
}

#[command(rename = "forget", display(display_none))]
pub async fn forget_disk() -> Result<(), Error> {
    let disk_guid = Path::new("/media/embassy/config/disk.guid");
    if tokio::fs::metadata(disk_guid).await.is_ok() {
        tokio::fs::remove_file(disk_guid).await?;
    }
    Ok(())
}
