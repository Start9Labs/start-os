use std::sync::Arc;

use rpc_toolkit::command;
use rpc_toolkit::yajrc::RpcError;

use crate::context::RecoveryContext;
use crate::logs::{display_logs, fetch_logs, LogResponse, LogSource};
use crate::util::display_none;
use crate::Error;

pub const SYSTEMD_UNIT: &'static str = "embassy-init";

#[command(subcommands(error, logs, exit))]
pub fn recovery() -> Result<(), Error> {
    Ok(())
}

#[command]
pub fn error(#[context] ctx: RecoveryContext) -> Result<Arc<RpcError>, Error> {
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
pub fn exit(#[context] ctx: RecoveryContext) -> Result<(), Error> {
    ctx.shutdown.send(()).expect("receiver dropped");
    Ok(())
}
