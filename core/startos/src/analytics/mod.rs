use std::net::SocketAddr;

use axum::Router;
use chrono::Utc;
use futures::future::ready;
use rpc_toolkit::{from_fn_async, Context, HandlerExt, ParentHandler, Server};
use sqlx::query;
use ts_rs::TS;

use crate::analytics::context::AnalyticsContext;
use crate::middleware::cors::Cors;
use crate::net::static_server::{bad_request, not_found, server_error};
use crate::net::web_server::WebServer;
use crate::rpc_continuations::Guid;
use crate::Error;

pub mod context;

pub fn analytics_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("recordMetrics", from_fn_async(record_metrics).no_cli())
        .subcommand(
            "recordUserActivity",
            from_fn_async(record_user_activity).no_cli(),
        )
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

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
struct MetricsParams {
    version: char,
    pkg_id: char,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
struct ActivityParams {
    server_id: String,
    os_version: String,
    arch: String,
}

async fn record_metrics(
    ctx: AnalyticsContext,
    MetricsParams {
        version,
        pkg_id,
    }: MetricsParams,
) -> Result<(), Error> {
    let pool = ctx.db;
    let created_at = Utc::now().to_rfc3339();
    query!(
        "INSERT INTO metric (created_at, version, pkg_id) VALUES ($1, $2, $3)",
        created_at,
        version,
        pkg_id
    )
    .execute(pool)
    .await?;
    Ok(())
}

async fn record_user_activity(
    analytics_ctx: AnalyticsContext,
    ActivityParams { server_id, os_version, arch }: ActivityParams,
) -> Result<(), Error> {
    let pool = analytics_ctx.db;
    let created_at = Utc::now().to_rfc3339();

    query!("INSERT INTO user_activity (created_at, server_id, os_version, arch) VALUES ($1, $2, $3, $4)",
    created_at,
    server_id,
    os_vers,
    arch
    )
    .execute(pool)
    .await?;

    Ok(())
}
