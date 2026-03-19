use std::collections::VecDeque;
use std::ffi::OsString;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use axum::body::Body;
use axum::http::{Response, header};
use axum::extract::DefaultBodyLimit;
use axum::routing::{any, get, post};
use axum::{Extension, Json, Router};
use color_eyre::eyre::Error;
use rpc_toolkit::Server;
use serde::Deserialize;
use std::future::ready;
use std::net::SocketAddr;
use std::time::Duration;
use tokio::sync::mpsc;
use tracing::instrument;

use tower_http::cors::{AllowHeaders, AllowMethods, AllowOrigin, CorsLayer};

use crate::embedded_web::serve_embedded;
use crate::luci_proxy::{self, ProxyClient};
use crate::setup::{self, FlashMode, ResolvedPassword, SetupEvent};
use crate::{init_logging, main_api, middleware::SessionAuth, ssl, ServerContext};

/// Shared state passed to HTTP handlers via Extension.
#[derive(Clone)]
struct AppState {
    /// Resolved WiFi password for setup mode. None in normal mode.
    setup_password: Option<Arc<ResolvedPassword>>,
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
    let pwd = match state.setup_password {
        Some(ref pwd) => pwd.clone(),
        None => return ndjson_error("no WiFi password available — cannot flash"),
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
        setup::run_setup_flash(params.mode, &params.password, &pwd.password, &tx);
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

/// GET /static/root-ca.crt — serves the Root CA certificate for download (no auth required).
async fn root_ca_handler() -> Response<Body> {
    match ssl::read_root_ca_pem() {
        Ok(pem) => Response::builder()
            .header(header::CONTENT_TYPE, "application/x-pem-file")
            .header(
                header::CONTENT_DISPOSITION,
                "attachment; filename=\"startwrt-ca.crt\"",
            )
            .body(Body::from(pem))
            .unwrap(),
        Err(_) => Response::builder()
            .status(500)
            .body(Body::from("Root CA certificate not available"))
            .unwrap(),
    }
}

/// Initialize SSL certificates. Returns true if HTTPS is ready.
fn init_ssl() -> bool {
    if let Err(e) = ssl::ensure_root_ca() {
        tracing::error!("Root CA generation failed: {e}");
        return false;
    }

    if let Err(e) = ssl::ensure_intermediate_ca() {
        tracing::error!("intermediate CA generation failed: {e}");
        return false;
    }

    let lan_ip = ssl::read_lan_ip(std::path::Path::new("/etc/config"));
    if let Err(e) = ssl::ensure_server_cert(lan_ip) {
        tracing::error!("server cert generation failed: {e}");
        return false;
    }

    // Verify the cert files are valid by attempting to build a TLS config.
    // If they're corrupt, force-regenerate once.
    if ssl::build_tls_config().is_err() {
        tracing::warn!("TLS config build failed with existing certs, regenerating");
        if let Err(e) = ssl::regenerate_server_cert(lan_ip) {
            tracing::error!("cert regeneration failed: {e}");
            return false;
        }
        if ssl::build_tls_config().is_err() {
            tracing::error!("TLS config build still failing after regeneration");
            return false;
        }
    }

    true
}

#[instrument(skip_all)]
async fn inner_main() -> Result<(), Error> {
    let setup_mode = tokio::task::spawn_blocking(setup::is_setup_mode).await?;

    let app_state;

    if setup_mode {
        tracing::info!("setup mode detected (booted from removable media)");

        // Resolve WiFi password: SD baked-in → eMMC key_backup → None
        let pwd = tokio::task::spawn_blocking(|| match setup::resolve_password() {
            Ok(Some(pwd)) => {
                tracing::info!("WiFi password resolved (baked_in={})", pwd.baked_in);
                Some(pwd)
            }
            Ok(None) => {
                tracing::error!("no WiFi password available — cannot start setup AP");
                None
            }
            Err(e) => {
                tracing::error!("WiFi password resolution failed: {e}");
                None
            }
        })
        .await?;

        if let Some(ref pwd) = pwd {
            // Configure WiFi AP with resolved password (single-client limit)
            let wifi_password = pwd.password.clone();
            tokio::task::spawn_blocking(move || {
                if let Err(e) = crate::init::configure_wifi("/etc/config", &wifi_password, Some(1)) {
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
            tracing::error!("no WiFi password available — setup AP cannot start");
        }

        app_state = AppState {
            setup_password: pwd.map(Arc::new),
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
            setup_password: None,
            flash_in_progress: Arc::new(AtomicBool::new(false)),
        };
    }

    // Initialize SSL: ensure Root CA and server cert exist
    let tls_ready = tokio::task::spawn_blocking(init_ssl).await?;

    let ctx = ServerContext;
    let handler = Server::new(move || ready(Ok(ctx.clone())), main_api())
        .middleware(SessionAuth::new());

    let proxy_client = ProxyClient(
        reqwest::Client::builder()
            .no_proxy()
            .redirect(reqwest::redirect::Policy::none())
            .no_gzip()
            .no_brotli()
            .no_deflate()
            .build()
            .expect("failed to build proxy HTTP client"),
    );

    let cors = CorsLayer::new()
        .allow_origin(AllowOrigin::mirror_request())
        .allow_methods(AllowMethods::mirror_request())
        .allow_headers(AllowHeaders::mirror_request())
        .allow_credentials(true);

    let app = Router::new()
        // RPC API at /rpc/v1 (matches frontend's RELATIVE_URL)
        .route("/rpc/v1", post(handler))
        // Streaming flash endpoint for setup wizard
        .route("/api/setup/flash", post(setup_flash_handler))
        // WebSocket endpoint for live log streaming
        .route("/api/logs", axum::routing::get(crate::logs::logs_ws_handler))
        // Support diagnostics bundle
        .route("/api/diagnostics", get(crate::diagnostics::diagnostics_handler))
        // Config backup/restore
        .route("/api/backup", get(crate::backup::backup_handler))
        .route("/api/restore", post(crate::backup::restore_handler)
            .layer(DefaultBodyLimit::max(10 * 1024 * 1024)))
        // Root CA download (no auth required)
        .route("/static/root-ca.crt", get(root_ca_handler))
        // LuCI reverse proxy — forwards to uhttpd on localhost:8080
        .route("/cgi-bin/{*rest}", any(luci_proxy::handler))
        .route("/luci-static/{*rest}", any(luci_proxy::handler))
        .route("/ubus", any(luci_proxy::handler))
        .route("/ubus/", any(luci_proxy::handler))
        .route("/ubus/{*rest}", any(luci_proxy::handler))
        // Convenience redirect: /luci → /cgi-bin/luci
        .route(
            "/luci",
            get(|| async {
                axum::response::Redirect::temporary("/cgi-bin/luci")
            }),
        )
        // Everything else serves the embedded web UI
        .fallback(axum::routing::any(serve_embedded))
        .layer(cors)
        .layer(Extension(proxy_client))
        .layer(Extension(app_state));

    // Start HTTP on port 80 (full UI — serves everything)
    let http_addr = SocketAddr::from(([0, 0, 0, 0, 0, 0, 0, 0], 80));
    let http_app = app.clone();
    let http_handle = tokio::spawn(async move {
        tracing::info!("HTTP listening on {}", http_addr);
        axum_server::bind(http_addr)
            .serve(http_app.into_make_service_with_connect_info::<SocketAddr>())
            .await
    });

    // Start HTTPS on port 443 if TLS is ready.
    // Uses from_pem_file so axum-server watches for file changes —
    // cert rotations (e.g. after LAN IP change) take effect without restart.
    if tls_ready {
        let https_addr = SocketAddr::from(([0, 0, 0, 0, 0, 0, 0, 0], 443));
        let rustls_config = axum_server::tls_rustls::RustlsConfig::from_pem_file(
            ssl::server_cert_path(),
            ssl::server_key_path(),
        )
        .await
        .map_err(|e| color_eyre::eyre::eyre!("failed to load TLS config: {e}"))?;

        let https_app = app.clone();

        let https_handle = tokio::spawn(async move {
            tracing::info!("HTTPS listening on {}", https_addr);
            axum_server::bind_rustls(https_addr, rustls_config)
                .serve(https_app.into_make_service_with_connect_info::<SocketAddr>())
                .await
        });

        // Both listeners must stay running. If either exits, log and abort.
        tokio::select! {
            res = http_handle => {
                tracing::error!("HTTP listener exited unexpectedly");
                res??;
            }
            res = https_handle => {
                tracing::error!("HTTPS listener exited unexpectedly");
                res??;
            }
        }
    } else {
        tracing::warn!("HTTPS disabled — TLS setup failed, serving HTTP only");
        http_handle.await??;
    }

    Ok(())
}

pub fn main(_args: VecDeque<OsString>) {
    init_logging("startwrt-ctrld");
    tracing::info!("startwrt-ctrld starting (luci proxy v10)");

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
