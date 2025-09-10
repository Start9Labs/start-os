use std::net::Ipv4Addr;

use clap::Parser;
use ipnet::Ipv4Net;
use rpc_toolkit::{Context, Empty, HandlerExt, ParentHandler, from_fn_async};
use serde::{Deserialize, Serialize};

use crate::context::CliContext;
use crate::prelude::*;
use crate::tunnel::context::TunnelContext;
use crate::tunnel::wg::WgSubnetConfig;

pub fn tunnel_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "db",
            super::db::db_api::<C>()
                .with_about("Commands to interact with the db i.e. dump and apply"),
        )
        .subcommand(
            "subnet",
            subnet_api::<C>().with_about("Add, remove, or modify subnets"),
        )
    // .subcommand(
    //     "forward",
    //     ParentHandler::<C>::new()
    //         .subcommand(
    //             "add",
    //             from_fn_async(add_forward)
    //                 .with_metadata("sync_db", Value::Bool(true))
    //                 .no_display()
    //                 .with_about("Add a new port forward")
    //                 .with_call_remote::<CliContext>(),
    //         )
    //         .subcommand(
    //             "remove",
    //             from_fn_async(remove_forward)
    //                 .with_metadata("sync_db", Value::Bool(true))
    //                 .no_display()
    //                 .with_about("Remove a port forward")
    //                 .with_call_remote::<CliContext>(),
    //         ),
    // )
}

#[derive(Deserialize, Serialize, Parser)]
pub struct SubnetParams {
    subnet: Ipv4Net,
}

pub fn subnet_api<C: Context>() -> ParentHandler<C, SubnetParams> {
    ParentHandler::new()
        .subcommand(
            "add",
            from_fn_async(add_subnet)
                .with_metadata("sync_db", Value::Bool(true))
                .with_inherited(|a, _| a)
                .no_display()
                .with_about("Add a new subnet")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "remove",
            from_fn_async(remove_subnet)
                .with_metadata("sync_db", Value::Bool(true))
                .with_inherited(|a, _| a)
                .no_display()
                .with_about("Remove a subnet")
                .with_call_remote::<CliContext>(),
        )
    // .subcommand(
    //     "set-default-forward-target",
    //     from_fn_async(set_default_forward_target)
    //         .with_metadata("sync_db", Value::Bool(true))
    //         .no_display()
    //         .with_about("Set the default target for port forwarding")
    //         .with_call_remote::<CliContext>(),
    // )
    // .subcommand(
    //     "add-client",
    //     from_fn_async(add_client)
    //         .with_metadata("sync_db", Value::Bool(true))
    //         .no_display()
    //         .with_about("Add a client to a subnet")
    //         .with_call_remote::<CliContext>(),
    // )
    // .subcommand(
    //     "remove-client",
    //     from_fn_async(remove_client)
    //         .with_metadata("sync_db", Value::Bool(true))
    //         .no_display()
    //         .with_about("Remove a client from a subnet")
    //         .with_call_remote::<CliContext>(),
    // )
}

pub async fn add_subnet(
    ctx: TunnelContext,
    _: Empty,
    SubnetParams { subnet }: SubnetParams,
) -> Result<(), Error> {
    let server = ctx
        .db
        .mutate(|db| {
            let map = db.as_wg_mut().as_subnets_mut();
            if !map.contains_key(&subnet)? {
                map.insert(&subnet, &WgSubnetConfig::new())?;
            }
            db.as_wg().de()
        })
        .await
        .result?;
    server.sync().await
}

pub async fn remove_subnet(
    ctx: TunnelContext,
    _: Empty,
    SubnetParams { subnet }: SubnetParams,
) -> Result<(), Error> {
    let server = ctx
        .db
        .mutate(|db| {
            db.as_wg_mut().as_subnets_mut().remove(&subnet)?;
            db.as_wg().de()
        })
        .await
        .result?;
    server.sync().await
}
