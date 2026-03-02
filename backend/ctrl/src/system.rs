use rpc_toolkit::{from_fn_async, Context, HandlerExt, ParentHandler};
use tokio::process::Command;
use tracing::instrument;

use crate::error::Error;
use crate::{CliContext, ServerContext};

#[instrument(skip_all)]
async fn factory_reset(_ctx: ServerContext) -> Result<(), Error> {
    let output = Command::new("firstboot")
        .arg("-y")
        .output()
        .await
        .map_err(|e| Error::other(format!("failed to run firstboot: {e}")))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(Error::other(format!("firstboot failed: {stderr}")));
    }

    // Spawn reboot after a short delay so the HTTP response can reach the client
    tokio::spawn(async {
        tokio::time::sleep(std::time::Duration::from_secs(1)).await;
        if let Err(e) = Command::new("reboot").status().await {
            tracing::error!("failed to reboot: {e}");
        }
    });

    Ok(())
}

pub fn system<C: Context>() -> ParentHandler<C> {
    ParentHandler::new().subcommand(
        "factory-reset",
        from_fn_async(factory_reset)
            .no_display()
            .with_about("Wipe overlay and reboot (factory reset)")
            .with_call_remote::<CliContext>(),
    )
}
