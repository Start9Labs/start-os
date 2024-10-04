use std::ops::Deref;

use clap::Parser;
use rpc_toolkit::{
    from_fn_async, CallRemoteHandler, Context, Empty, HandlerArgs, HandlerExt, HandlerFor,
    ParentHandler,
};
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::context::{CliContext, RpcContext};
use crate::lxc::{ContainerId, LxcConfig};
use crate::prelude::*;
use crate::rpc_continuations::Guid;

pub fn lxc<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "create",
            from_fn_async(create)
                .with_call_remote::<CliContext>()
                .with_about("Create lxc container"),
        )
        .subcommand(
            "list",
            from_fn_async(list)
                .with_custom_display_fn(|_, res| {
                    use prettytable::*;
                    let mut table = table!([bc => "GUID"]);
                    for guid in res {
                        table.add_row(row![&*guid]);
                    }
                    table.printstd();
                    Ok(())
                })
                .with_call_remote::<CliContext>()
                .with_about("List lxc containers"),
        )
        .subcommand(
            "remove",
            from_fn_async(remove)
                .no_display()
                .with_call_remote::<CliContext>()
                .with_about("Remove lxc container"),
        )
        .subcommand("connect", from_fn_async(connect_rpc).no_cli())
        .subcommand(
            "connect",
            from_fn_async(connect_rpc_cli)
                .no_display()
                .with_about("Connect to a lxc container"),
        )
}

pub async fn create(ctx: RpcContext) -> Result<ContainerId, Error> {
    let container = ctx.lxc_manager.create(None, LxcConfig::default()).await?;
    let guid = container.guid.deref().clone();
    ctx.dev.lxc.lock().await.insert(guid.clone(), container);
    Ok(guid)
}

pub async fn list(ctx: RpcContext) -> Result<Vec<ContainerId>, Error> {
    Ok(ctx.dev.lxc.lock().await.keys().cloned().collect())
}

#[derive(Deserialize, Serialize, Parser, TS)]
pub struct RemoveParams {
    #[ts(type = "string")]
    pub guid: ContainerId,
}

pub async fn remove(ctx: RpcContext, RemoveParams { guid }: RemoveParams) -> Result<(), Error> {
    if let Some(container) = ctx.dev.lxc.lock().await.remove(&guid) {
        container.exit().await?;
    }
    Ok(())
}

#[derive(Deserialize, Serialize, Parser, TS)]
pub struct ConnectParams {
    #[ts(type = "string")]
    pub guid: ContainerId,
}

pub async fn connect_rpc(
    ctx: RpcContext,
    ConnectParams { guid }: ConnectParams,
) -> Result<Guid, Error> {
    super::connect(
        &ctx,
        ctx.dev.lxc.lock().await.get(&guid).ok_or_else(|| {
            Error::new(eyre!("No container with guid: {guid}"), ErrorKind::NotFound)
        })?,
    )
    .await
}

pub async fn connect_rpc_cli(
    HandlerArgs {
        context,
        parent_method,
        method,
        params,
        inherited_params,
        raw_params,
    }: HandlerArgs<CliContext, ConnectParams>,
) -> Result<(), Error> {
    let ctx = context.clone();
    let guid = CallRemoteHandler::<CliContext, _, _>::new(from_fn_async(connect_rpc))
        .handle_async(HandlerArgs {
            context,
            parent_method,
            method,
            params: rpc_toolkit::util::Flat(params, Empty {}),
            inherited_params,
            raw_params,
        })
        .await?;

    super::connect_cli(&ctx, guid).await
}
