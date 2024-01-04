use std::path::Path;
use std::sync::Arc;

use clap::Parser;
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{command, from_fn_async, AnyContext, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};

use crate::context::DiagnosticContext;
use crate::init::SYSTEM_REBUILD_PATH;
use crate::logs::{fetch_logs, LogResponse, LogSource};
use crate::shutdown::Shutdown;
use crate::Error;

pub fn diagnostic() -> ParentHandler {
    ParentHandler::new()
        .subcommand("error", from_fn_async(error))
        .subcommand("logs", from_fn_async(logs).no_cli())
        .subcommand("exit", from_fn_async(exit).no_display())
        .subcommand("restart", from_fn_async(restart).no_display())
        .subcommand("forget", from_fn_async(forget_disk).no_display())
        .subcommand("disk", from_fn_async(disk))
        .subcommand("rebuild", from_fn_async(rebuild).no_display())
}

// #[command]
pub fn error(ctx: DiagnosticContext) -> Result<Arc<RpcError>, Error> {
    Ok(ctx.error.clone())
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct LogsParams {
    limit: Option<usize>,
    cursor: Option<String>,
    before: bool,
}
pub async fn logs(
    _: AnyContext,
    LogsParams {
        limit,
        cursor,
        before,
    }: LogsParams,
) -> Result<LogResponse, Error> {
    Ok(fetch_logs(LogSource::System, limit, cursor, before).await?)
}

pub fn exit(ctx: DiagnosticContext) -> Result<(), Error> {
    ctx.shutdown.send(None).expect("receiver dropped");
    Ok(())
}

pub fn restart(ctx: DiagnosticContext) -> Result<(), Error> {
    ctx.shutdown
        .send(Some(Shutdown {
            export_args: ctx
                .disk_guid
                .clone()
                .map(|guid| (guid, ctx.datadir.clone())),
            restart: true,
        }))
        .expect("receiver dropped");
    Ok(())
}
pub async fn rebuild(ctx: DiagnosticContext) -> Result<(), Error> {
    tokio::fs::write(SYSTEM_REBUILD_PATH, b"").await?;
    restart(ctx)
}

pub fn disk() -> Result<(), Error> {
    Ok(())
}

pub async fn forget_disk() -> Result<(), Error> {
    let disk_guid = Path::new("/media/embassy/config/disk.guid");
    if tokio::fs::metadata(disk_guid).await.is_ok() {
        tokio::fs::remove_file(disk_guid).await?;
    }
    Ok(())
}
