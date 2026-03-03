use std::collections::VecDeque;
use std::ffi::OsString;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use axum::body::Body;
use axum::http::Response;
use axum::routing::post;
use axum::{Extension, Json, Router};
use color_eyre::eyre::Error;
use futures::stream::StreamExt;
use rpc_toolkit::Server;
use serde::Deserialize;
use std::future::ready;
use std::net::SocketAddr;
use std::time::Duration;
use tokio::sync::mpsc;
use tracing::instrument;

use crate::embedded_web::serve_embedded;
use crate::setup::{self, FlashMode, ResolvedPmk, SetupEvent};
use crate::{init_logging, main_api, middleware::SessionAuth, ServerContext};

/// Shared state passed to HTTP handlers via Extension.
#[derive(Clone)]
struct AppState {
    /// Resolved PMK for setup mode. None in normal mode.
    setup_pmk: Option<Arc<ResolvedPmk>>,
    /// Guards against concurrent flash operations.
    flash_in_progress: Arc<AtomicBool>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct FlashParams {
    mode: FlashMode,
    password: String,
}

fn ndjson_error(message: &str) -> Response<Body> {
    let event = SetupEvent::Error {
        message: message.into(),
    };
    let json = serde_json::to_string(&event).unwrap();
    Response::builder()
        .header("content-type", "application/x-ndjson")
        .body(Body::from(format!("{json}\n")))
        .unwrap()
}

/// POST /api/setup/flash — streams NDJSON progress events.
async fn setup_flash_handler(
    Extension(state): Extension<AppState>,
    Json(params): Json<FlashParams>,
) -> Response<Body> {
    let pmk = match state.setup_pmk {
        Some(ref pmk) => pmk.clone(),
        None => return ndjson_error("no WiFi PMK available — cannot flash"),
    };

    // Prevent concurrent flash operations
    if state
        .flash_in_progress
        .compare_exchange(false, true, Ordering::SeqCst, Ordering::SeqCst)
        .is_err()
    {
        return ndjson_error("flash already in progress");
    }

    let flash_flag = state.flash_in_progress.clone();
    let (tx, rx) = mpsc::channel::<SetupEvent>(32);

    tokio::task::spawn_blocking(move || {
        setup::run_setup_flash(params.mode, &params.password, &pmk.pmk_hex, &tx);
        flash_flag.store(false, Ordering::SeqCst);
    });

    let stream = futures::stream::unfold(rx, |mut rx| async move {
        let event = rx.recv().await?;
        let mut json = serde_json::to_string(&event).unwrap();
        json.push('\n');
        Some((Ok::<_, std::convert::Infallible>(json), rx))
    });

    Response::builder()
        .header("content-type", "application/x-ndjson")
        .body(Body::from_stream(stream))
        .unwrap()
}

#[instrument(skip_all)]
async fn inner_main() -> Result<(), Error> {
    let setup_mode = tokio::task::spawn_blocking(setup::is_setup_mode).await?;

    let app_state;

    if setup_mode {
        tracing::info!("setup mode detected (booted from removable media)");

        // Resolve WiFi PMK: SD baked-in → eMMC key_backup → None
        let pmk = tokio::task::spawn_blocking(|| match setup::resolve_pmk() {
            Ok(Some(pmk)) => {
                tracing::info!("PMK resolved (baked_in={})", pmk.baked_in);
                Some(pmk)
            }
            Ok(None) => {
                tracing::error!("no WiFi PMK available — cannot start setup AP");
                None
            }
            Err(e) => {
                tracing::error!("PMK resolution failed: {e}");
                None
            }
        })
        .await?;

        if let Some(ref pmk) = pmk {
            // Configure WiFi AP with resolved PMK (single-client limit)
            let pmk_hex = pmk.pmk_hex.clone();
            tokio::task::spawn_blocking(move || {
                if let Err(e) = crate::init::configure_wifi("/etc/config", &pmk_hex, Some(1)) {
                    tracing::error!("WiFi AP setup failed: {e}");
                }
                let _ = std::process::Command::new("wifi").arg("reload").status();
            })
            .await?;

            // Enable captive portal only when WiFi AP can start
            if let Err(e) =
                tokio::task::spawn_blocking(crate::captive::enable_captive_portal).await?
            {
                tracing::error!("captive portal setup failed: {e}");
            }
        } else {
            tracing::error!("no WiFi PMK available — setup AP cannot start");
        }

        app_state = AppState {
            setup_pmk: pmk.map(Arc::new),
            flash_in_progress: Arc::new(AtomicBool::new(false)),
        };
    } else {
        // Normal mode: restore WiFi + manage captive portal state
        tokio::task::spawn_blocking(|| {
            if let Err(e) = crate::init::restore_wifi_if_needed() {
                tracing::error!("WiFi auto-restore failed: {e}");
            }
            if let Err(e) = crate::profiles::bootstrap_admin_profile("/etc/config") {
                tracing::error!("Admin profile bootstrap failed: {e}");
            }
            if let Err(e) = crate::system::apply_remote_access(ServerContext) {
                tracing::error!("Remote access rule apply failed: {e}");
            }
        })
        .await?;

        if let Err(e) = crate::captive::ensure_captive_portal_state().await {
            tracing::error!("captive portal setup failed: {e}");
        }

        app_state = AppState {
            setup_pmk: None,
            flash_in_progress: Arc::new(AtomicBool::new(false)),
        };
    }

    let ctx = ServerContext;
    let handler = Server::new(move || ready(Ok(ctx.clone())), main_api())
        .middleware(SessionAuth::new());

    let app = Router::new()
        // RPC API at /rpc/v1 (matches frontend's RELATIVE_URL)
        .route("/rpc/v1", post(handler))
        // Streaming flash endpoint for setup wizard
        .route("/api/setup/flash", post(setup_flash_handler))
        // Everything else serves the embedded web UI
        .fallback(axum::routing::any(serve_embedded))
        .layer(Extension(app_state));

    let addr = SocketAddr::from(([0, 0, 0, 0], 80));
    println!("listening on {}", addr);
    axum_server::bind(addr)
        .serve(app.into_make_service_with_connect_info::<SocketAddr>())
        .await?;
    Ok(())
}

pub fn main(_args: VecDeque<OsString>) {
    let _guard = init_logging("startwrt-ctrld");

    let res = {
        let rt = tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
            .expect("failed to initialize runtime");
        let res = rt.block_on(inner_main());
        rt.shutdown_timeout(Duration::from_secs(60));
        res
    };

    match res {
        Ok(_) => (),
        Err(e) => {
            eprintln!("{}", e);
            tracing::debug!("{:?}", e);
            std::process::exit(1)
        }
    }
}
