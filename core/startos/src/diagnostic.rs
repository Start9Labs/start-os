use std::path::Path;
use std::sync::Arc;

use clap::Parser;
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{
    from_fn, from_fn_async, CallRemoteHandler, Context, Empty, HandlerExt, ParentHandler,
};
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::context::{CliContext, DiagnosticContext, RpcContext};
use crate::init::SYSTEM_REBUILD_PATH;
use crate::logs::{fetch_logs, LogResponse, LogSource};
use crate::shutdown::Shutdown;
use crate::Error;

pub fn diagnostic<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand::<C, _>("error", from_fn(error).with_call_remote::<CliContext>())
        .subcommand("logs", crate::system::logs::<DiagnosticContext>())
        .subcommand(
            "logs",
            from_fn_async(crate::logs::cli_logs::<DiagnosticContext, Empty>).no_display(),
        )
        .subcommand(
            "kernel-logs",
            crate::system::kernel_logs::<DiagnosticContext>(),
        )
        .subcommand(
            "kernel-logs",
            from_fn_async(crate::logs::cli_logs::<DiagnosticContext, Empty>).no_display(),
        )
        .subcommand::<C, _>(
            "exit",
            from_fn(exit).no_display().with_call_remote::<CliContext>(),
        )
        .subcommand::<C, _>(
            "restart",
            from_fn(restart)
                .no_display()
                .with_call_remote::<CliContext>(),
        )
        .subcommand("disk", disk::<C>())
        .subcommand::<C, _>(
            "rebuild",
            from_fn_async(rebuild)
                .no_display()
                .with_call_remote::<CliContext>(),
        )
}

// #[command]
pub fn error(ctx: DiagnosticContext) -> Result<Arc<RpcError>, Error> {
    Ok(ctx.error.clone())
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

pub fn disk<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand::<C, _>("forget", from_fn_async(forget_disk::<C>).no_cli())
        .subcommand(
            "forget",
            CallRemoteHandler::<CliContext, _, _>::new(
                from_fn_async(forget_disk::<RpcContext>).no_display(),
            )
            .no_display(),
        )
}

pub async fn forget_disk<C: Context>(_: C) -> Result<(), Error> {
    let disk_guid = Path::new("/media/startos/config/disk.guid");
    if tokio::fs::metadata(disk_guid).await.is_ok() {
        tokio::fs::remove_file(disk_guid).await?;
    }
    Ok(())
}
