use std::collections::{BTreeMap, BTreeSet};
use std::net::SocketAddr;

use axum::Router;
use futures::future::ready;
use imbl_value::InternedString;
use models::DataUrl;
use rpc_toolkit::{from_fn_async, Context, HandlerExt, ParentHandler, Server};
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::context::CliContext;
use crate::middleware::cors::Cors;
use crate::net::static_server::{bad_request, not_found, server_error};
use crate::net::web_server::WebServer;
use crate::prelude::*;
use crate::registry::auth::Auth;
use crate::registry::context::RegistryContext;
use crate::registry::device_info::DeviceInfoMiddleware;
use crate::registry::os::index::OsIndex;
use crate::registry::package::index::{Category, PackageIndex};
use crate::registry::signer::SignerInfo;
use crate::rpc_continuations::Guid;
use crate::util::serde::HandlerExtSerde;

pub mod admin;
pub mod asset;
pub mod auth;
pub mod context;
pub mod db;
pub mod device_info;
pub mod os;
pub mod package;
pub mod signer;

#[derive(Debug, Default, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
pub struct RegistryDatabase {
    pub admins: BTreeSet<Guid>,
    pub index: FullIndex,
}
impl RegistryDatabase {}

#[derive(Debug, Default, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct FullIndex {
    pub name: Option<String>,
    pub icon: Option<DataUrl<'static>>,
    pub package: PackageIndex,
    pub os: OsIndex,
    pub signers: BTreeMap<Guid, SignerInfo>,
}

pub async fn get_full_index(ctx: RegistryContext) -> Result<FullIndex, Error> {
    ctx.db.peek().await.into_index().de()
}

#[derive(Debug, Default, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct RegistryInfo {
    pub name: Option<String>,
    pub icon: Option<DataUrl<'static>>,
    #[ts(as = "BTreeMap::<String, Category>")]
    pub categories: BTreeMap<InternedString, Category>,
}

pub async fn get_info(ctx: RegistryContext) -> Result<RegistryInfo, Error> {
    let peek = ctx.db.peek().await.into_index();
    Ok(RegistryInfo {
        name: peek.as_name().de()?,
        icon: peek.as_icon().de()?,
        categories: peek.as_package().as_categories().de()?,
    })
}

pub fn registry_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "index",
            from_fn_async(get_full_index)
                .with_display_serializable()
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "info",
            from_fn_async(get_info)
                .with_display_serializable()
                .with_call_remote::<CliContext>(),
        )
        .subcommand("os", os::os_api::<C>())
        .subcommand("package", package::package_api::<C>())
        .subcommand("admin", admin::admin_api::<C>())
        .subcommand("db", db::db_api::<C>())
}

pub fn registry_router(ctx: RegistryContext) -> Router {
    use axum::extract as x;
    use axum::routing::{any, get, post};
    Router::new()
        .route("/rpc/*path", {
            let ctx = ctx.clone();
            post(
                Server::new(move || ready(Ok(ctx.clone())), registry_api())
                    .middleware(Cors::new())
                    .middleware(Auth::new())
                    .middleware(DeviceInfoMiddleware::new()),
            )
        })
        .route(
            "/ws/rpc/*path",
            get({
                let ctx = ctx.clone();
                move |x::Path(path): x::Path<String>,
                      ws: axum::extract::ws::WebSocketUpgrade| async move {
                    match Guid::from(&path) {
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
                    match Guid::from(&path) {
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

impl WebServer {
    pub fn serve_registry(&mut self, ctx: RegistryContext) {
        self.serve_router(registry_router(ctx))
    }
}
