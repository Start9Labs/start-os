use std::net::SocketAddr;

use axum::Router;
use futures::future::ready;
use rpc_toolkit::{Context, ParentHandler, Server};

use crate::analytics::context::AnalyticsContext;
use crate::middleware::cors::Cors;
use crate::net::static_server::{bad_request, not_found, server_error};
use crate::net::web_server::WebServer;
use crate::rpc_continuations::Guid;

pub mod context;

pub fn analytics_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new() // TODO: FullMetal
}

pub fn analytics_server_router(ctx: AnalyticsContext) -> Router {
    use axum::extract as x;
    use axum::routing::{any, get, post};
    Router::new()
        .route("/rpc/*path", {
            let ctx = ctx.clone();
            post(
                Server::new(move || ready(Ok(ctx.clone())), analytics_api())
                    .middleware(Cors::new())
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
    pub fn analytics(bind: SocketAddr, ctx: AnalyticsContext) -> Self {
        Self::new(bind, analytics_server_router(ctx))
    }
}
