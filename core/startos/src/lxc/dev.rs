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
use crate::service::ServiceStats;

pub fn lxc<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "create",
            from_fn_async(create)
                .with_about("Create lxc container")
                .with_call_remote::<CliContext>(),
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
                .with_about("List lxc containers")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "stats",
            from_fn_async(stats)
                .with_custom_display_fn(|_, res| {
                    use prettytable::*;
                    let mut table = table!([
                        "Container ID",
                        "Name",
                        "Memory Usage",
                        "Memory Limit",
                        "Memory %"
                    ]);
                    for ServiceStats {
                        container_id,
                        package_id,
                        memory_usage,
                        memory_limit,
                    } in res
                    {
                        table.add_row(row![
                            &*container_id,
                            &*package_id,
                            memory_usage,
                            memory_limit,
                            format!(
                                "{:.2}",
                                memory_usage.0 as f64 / memory_limit.0 as f64 * 100.0
                            )
                        ]);
                    }
                    table.printstd();
                    Ok(())
                })
                .with_about("List information related to the lxc containers i.e. CPU, Memory, Disk")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "remove",
            from_fn_async(remove)
                .no_display()
                .with_about("Remove lxc container")
                .with_call_remote::<CliContext>(),
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

pub async fn stats(ctx: RpcContext) -> Result<Vec<ServiceStats>, Error> {
    let ids = ctx.db.peek().await.as_public().as_package_data().keys()?;
    let guids: Vec<_> = ctx.dev.lxc.lock().await.keys().cloned().collect();

    let mut stats = Vec::with_capacity(guids.len());
    for id in ids {
        let service: tokio::sync::OwnedRwLockReadGuard<Option<crate::service::ServiceRef>> =
            ctx.services.get(&id).await;

        let service_ref = service.as_ref().or_not_found(&id)?;

        stats.push(service_ref.stats().await?);
    }
    Ok(stats)
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
