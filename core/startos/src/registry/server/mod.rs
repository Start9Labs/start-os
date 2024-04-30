use axum::Router;
use futures::future::ready;
use rpc_toolkit::{from_fn_async, ParentHandler, Server};
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::middleware::cors::Cors;
use crate::net::static_server::{bad_request, not_found, server_error};
use crate::prelude::*;
use crate::registry::server::context::RegistryContext;
use crate::registry::server::os::OsIndex;
use crate::registry::signer::SignerInfo;
use crate::rpc_continuations::RequestGuid;
use crate::util::serde::HandlerExtSerde;

pub mod admin;
pub mod auth;
pub mod context;
pub mod os;

#[derive(Debug, Default, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
pub struct RegistryDatabase {
    pub admins: Vec<SignerInfo>,
    pub index: FullIndex,
}

#[derive(Debug, Default, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct FullIndex {
    // pub package: PackageIndex,
    pub os: OsIndex,
}

pub async fn get_full_index(ctx: RegistryContext) -> Result<FullIndex, Error> {
    ctx.db.peek().await.into_index().de()
}

pub fn registry_api() -> ParentHandler {
    ParentHandler::new()
        .subcommand(
            "index",
            from_fn_async(get_full_index).with_display_serializable(), // .with_call_remote::<CliContext>(),
        )
        .subcommand("os", os::os_api())
        .subcommand("admin", admin::admin_api())
}

pub fn registry_server_router(ctx: RegistryContext) -> Router {
    use axum::extract as x;
    use axum::routing::{any, get, post};
    Router::new()
        .route("/rpc/*path", {
            let ctx = ctx.clone();
            post(
                Server::new(move || ready(Ok(ctx.clone())), registry_api())
                    .middleware(Cors::new()),
            )
        })
        .route(
            "/ws/rpc/*path",
            get({
                let ctx = ctx.clone();
                move |x::Path(path): x::Path<String>,
                      ws: axum::extract::ws::WebSocketUpgrade| async move {
                    match RequestGuid::from(&path) {
                        None => {
                            tracing::debug!("No Guid Path");
                            bad_request()
                        }
                        Some(guid) => match ctx.rpc_continuations.get_ws_handler(&guid).await {
                            Some(cont) => ws.on_upgrade(cont),
                            _ => not_found(),
                        },
                    }
                }
            }),
        )
        .route(
            "/rest/rpc/*path",
            any({
                let ctx = ctx.clone();
                move |request: x::Request| async move {
                    let path = request
                        .uri()
                        .path()
                        .strip_prefix("/rest/rpc/")
                        .unwrap_or_default();
                    match RequestGuid::from(&path) {
                        None => {
                            tracing::debug!("No Guid Path");
                            bad_request()
                        }
                        Some(guid) => match ctx.rpc_continuations.get_rest_handler(&guid).await {
                            None => not_found(),
                            Some(cont) => cont(request).await.unwrap_or_else(server_error),
                        },
                    }
                }
            }),
        )
}
