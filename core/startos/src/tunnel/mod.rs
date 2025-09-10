use axum::Router;
use futures::future::ready;
use rpc_toolkit::{Context, HandlerExt, ParentHandler, Server, from_fn_async};

use crate::context::CliContext;
use crate::middleware::auth::Auth;
use crate::middleware::cors::Cors;
use crate::net::static_server::{bad_request, not_found, server_error};
use crate::net::web_server::{Accept, WebServer};
use crate::prelude::*;
use crate::rpc_continuations::Guid;
use crate::tunnel::context::TunnelContext;

pub mod api;
pub mod context;
pub mod db;
pub mod forward;
pub mod wg;

pub const TUNNEL_DEFAULT_PORT: u16 = 5960;

pub fn tunnel_router(ctx: TunnelContext) -> Router {
    use axum::extract as x;
    use axum::routing::{any, get};
    Router::new()
        .route("/rpc/{*path}", {
            let ctx = ctx.clone();
            any(
                Server::new(move || ready(Ok(ctx.clone())), api::tunnel_api())
                    .middleware(Cors::new())
                    .middleware(Auth::new())
            )
        })
        .route(
            "/ws/rpc/{*path}",
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
            "/rest/rpc/{*path}",
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

impl<A: Accept + Send + Sync + 'static> WebServer<A> {
    pub fn serve_tunnel(&mut self, ctx: TunnelContext) {
        self.serve_router(tunnel_router(ctx))
    }
}
