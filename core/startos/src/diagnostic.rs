use std::path::Path;
use std::sync::Arc;

use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{
    from_fn, from_fn_async, CallRemoteHandler, Context, Empty, HandlerExt, ParentHandler,
};

use crate::context::{CliContext, DiagnosticContext, RpcContext};
use crate::init::SYSTEM_REBUILD_PATH;
use crate::shutdown::Shutdown;
use crate::Error;

pub fn diagnostic<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "error",
            from_fn(error)
                .with_about("Display diagnostic error")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "logs",
            crate::system::logs::<DiagnosticContext>().with_about("Display OS logs"),
        )
        .subcommand(
            "logs",
            from_fn_async(crate::logs::cli_logs::<DiagnosticContext, Empty>)
                .no_display()
                .with_about("Display OS logs"),
        )
        .subcommand(
            "kernel-logs",
            crate::system::kernel_logs::<DiagnosticContext>().with_about("Display kernel logs"),
        )
        .subcommand(
            "kernel-logs",
            from_fn_async(crate::logs::cli_logs::<DiagnosticContext, Empty>)
                .no_display()
                .with_about("Display kernal logs"),
        )
        .subcommand(
            "restart",
            from_fn(restart)
                .no_display()
                .with_about("Restart the server")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "disk",
            disk::<C>().with_about("Command to remove disk from filesystem"),
        )
        .subcommand(
            "rebuild",
            from_fn_async(rebuild)
                .no_display()
                .with_about("Teardown and rebuild service containers")
                .with_call_remote::<CliContext>(),
        )
}

// #[command]
pub fn error(ctx: DiagnosticContext) -> Result<Arc<RpcError>, Error> {
    Ok(ctx.error.clone())
}

pub fn restart(ctx: DiagnosticContext) -> Result<(), Error> {
    ctx.shutdown
        .send(Shutdown {
            export_args: ctx
                .disk_guid
                .clone()
                .map(|guid| (guid, ctx.datadir.clone())),
            restart: true,
        })
        .expect("receiver dropped");
    Ok(())
}
pub async fn rebuild(ctx: DiagnosticContext) -> Result<(), Error> {
    tokio::fs::write(SYSTEM_REBUILD_PATH, b"").await?;
    restart(ctx)
}

pub fn disk<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("forget", from_fn_async(forget_disk::<C>).no_cli())
        .subcommand(
            "forget",
            CallRemoteHandler::<CliContext, _, _>::new(
                from_fn_async(forget_disk::<RpcContext>).no_display(),
            )
            .no_display()
            .with_about("Remove disk from filesystem"),
        )
}

pub async fn forget_disk<C: Context>(_: C) -> Result<(), Error> {
    let disk_guid = Path::new("/media/startos/config/disk.guid");
    if tokio::fs::metadata(disk_guid).await.is_ok() {
        tokio::fs::remove_file(disk_guid).await?;
    }
    Ok(())
}
