use std::fs::Metadata;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::UNIX_EPOCH;

use color_eyre::eyre::eyre;
use digest::Digest;
use futures::FutureExt;
use http::response::Builder;
use hyper::{Body, Method, Request, Response, StatusCode};

use rpc_toolkit::rpc_handler;
use tokio::fs::File;
use tokio_util::codec::{BytesCodec, FramedRead};

use crate::context::{DiagnosticContext, InstallContext, RpcContext, SetupContext};
use crate::core::rpc_continuations::RequestGuid;
use crate::db::subscribe;
use crate::install::PKG_PUBLIC_DIR;
use crate::middleware::auth::HasValidSession;
use crate::net::HttpHandler;
use crate::{diagnostic_api, install_api, main_api, setup_api, Error, ErrorKind, ResultExt};

static NOT_FOUND: &[u8] = b"Not Found";
static NOT_AUTHORIZED: &[u8] = b"Not Authorized";

pub const MAIN_UI_WWW_DIR: &str = "/var/www/html/main";
pub const SETUP_UI_WWW_DIR: &str = "/var/www/html/setup";
pub const DIAG_UI_WWW_DIR: &str = "/var/www/html/diagnostic";
pub const INSTALL_UI_WWW_DIR: &str = "/var/www/html/install";

fn status_fn(_: i32) -> StatusCode {
    StatusCode::OK
}

#[derive(Clone)]
pub enum UiMode {
    Setup,
    Diag,
    Install,
    Main,
}

pub async fn setup_ui_file_router(ctx: SetupContext) -> Result<HttpHandler, Error> {
    let handler: HttpHandler = Arc::new(move |req| {
        let ctx = ctx.clone();

        let ui_mode = UiMode::Setup;
        async move {
            let res = match req.uri().path() {
                path if path.starts_with("/rpc/") => {
                    let rpc_handler =
                        rpc_handler!({command: setup_api, context: ctx, status: status_fn});

                    rpc_handler(req)
                        .await
                        .map_err(|err| Error::new(eyre!("{}", err), crate::ErrorKind::Network))
                }
                _ => alt_ui(req, ui_mode).await,
            };

            match res {
                Ok(data) => Ok(data),
                Err(err) => Ok(server_error(err)),
            }
        }
        .boxed()
    });

    Ok(handler)
}

pub async fn diag_ui_file_router(ctx: DiagnosticContext) -> Result<HttpHandler, Error> {
    let handler: HttpHandler = Arc::new(move |req| {
        let ctx = ctx.clone();
        let ui_mode = UiMode::Diag;
        async move {
            let res = match req.uri().path() {
                path if path.starts_with("/rpc/") => {
                    let rpc_handler =
                        rpc_handler!({command: diagnostic_api, context: ctx, status: status_fn});

                    rpc_handler(req)
                        .await
                        .map_err(|err| Error::new(eyre!("{}", err), crate::ErrorKind::Network))
                }
                _ => alt_ui(req, ui_mode).await,
            };

            match res {
                Ok(data) => Ok(data),
                Err(err) => Ok(server_error(err)),
            }
        }
        .boxed()
    });

    Ok(handler)
}

pub async fn install_ui_file_router(ctx: InstallContext) -> Result<HttpHandler, Error> {
    let handler: HttpHandler = Arc::new(move |req| {
        let ctx = ctx.clone();
        let ui_mode = UiMode::Install;
        async move {
            let res = match req.uri().path() {
                path if path.starts_with("/rpc/") => {
                    let rpc_handler =
                        rpc_handler!({command: install_api, context: ctx, status: status_fn});

                    rpc_handler(req)
                        .await
                        .map_err(|err| Error::new(eyre!("{}", err), crate::ErrorKind::Network))
                }
                _ => alt_ui(req, ui_mode).await,
            };

            match res {
                Ok(data) => Ok(data),
                Err(err) => Ok(server_error(err)),
            }
        }
        .boxed()
    });

    Ok(handler)
}

pub async fn main_ui_server_router(ctx: RpcContext) -> Result<HttpHandler, Error> {
    let handler: HttpHandler = Arc::new(move |req| {
        let ctx = ctx.clone();

        async move {
            let res = match req.uri().path() {
                path if path.starts_with("/rpc/") => {
                    let rpc_handler =
                        rpc_handler!({command: main_api, context: ctx, status: status_fn});

                    rpc_handler(req)
                        .await
                        .map_err(|err| Error::new(eyre!("{}", err), crate::ErrorKind::Network))
                }
                "/ws/db" => subscribe(ctx, req).await,
                path if path.starts_with("/ws/rpc/") => {
                    match RequestGuid::from(path.strip_prefix("/ws/rpc/").unwrap()) {
                        None => {
                            tracing::debug!("No Guid Path");
                            Ok::<_, Error>(bad_request())
                        }
                        Some(guid) => match ctx.get_ws_continuation_handler(&guid).await {
                            Some(cont) => match cont(req).await {
                                Ok::<_, Error>(r) => Ok::<_, Error>(r),
                                Err(err) => Ok::<_, Error>(server_error(err)),
                            },
                            _ => Ok::<_, Error>(not_found()),
                        },
                    }
                }
                path if path.starts_with("/rest/rpc/") => {
                    match RequestGuid::from(path.strip_prefix("/rest/rpc/").unwrap()) {
                        None => {
                            tracing::debug!("No Guid Path");
                            Ok::<_, Error>(bad_request())
                        }
                        Some(guid) => match ctx.get_rest_continuation_handler(&guid).await {
                            None => Ok::<_, Error>(not_found()),
                            Some(cont) => match cont(req).await {
                                Ok::<_, Error>(r) => Ok::<_, Error>(r),
                                Err(e) => Ok::<_, Error>(server_error(e)),
                            },
                        },
                    }
                }
                _ => main_embassy_ui(req, ctx).await,
            };

            match res {
                Ok(data) => Ok(data),
                Err(err) => Ok(server_error(err)),
            }
        }
        .boxed()
    });

    Ok(handler)
}

async fn alt_ui(req: Request<Body>, ui_mode: UiMode) -> Result<Response<Body>, Error> {
    let selected_root_dir = match ui_mode {
        UiMode::Setup => SETUP_UI_WWW_DIR,
        UiMode::Diag => DIAG_UI_WWW_DIR,
        UiMode::Install => INSTALL_UI_WWW_DIR,
        UiMode::Main => MAIN_UI_WWW_DIR,
    };

    let (request_parts, _body) = req.into_parts();
    match request_parts.uri.path() {
        "/" => {
            let full_path = PathBuf::from(selected_root_dir).join("index.html");

            file_send(full_path).await
        }
        _ => {
            match (
                request_parts.method,
                request_parts
                    .uri
                    .path()
                    .strip_prefix('/')
                    .unwrap_or(request_parts.uri.path())
                    .split_once('/'),
            ) {
                (Method::GET, None) => {
                    let uri_path = request_parts
                        .uri
                        .path()
                        .strip_prefix('/')
                        .unwrap_or(request_parts.uri.path());

                    let full_path = PathBuf::from(selected_root_dir).join(uri_path);
                    file_send(full_path).await
                }

                (Method::GET, Some((dir, file))) => {
                    let full_path = PathBuf::from(selected_root_dir).join(dir).join(file);
                    file_send(full_path).await
                }

                _ => Ok(not_found()),
            }
        }
    }
}

async fn main_embassy_ui(req: Request<Body>, ctx: RpcContext) -> Result<Response<Body>, Error> {
    let selected_root_dir = MAIN_UI_WWW_DIR;

    let (request_parts, _body) = req.into_parts();
    match request_parts.uri.path() {
        "/" => {
            let full_path = PathBuf::from(selected_root_dir).join("index.html");

            file_send(full_path).await
        }
        _ => {
            let valid_session = HasValidSession::from_request_parts(&request_parts, &ctx).await;

            match valid_session {
                Ok(_valid) => {
                    match (
                        request_parts.method,
                        request_parts
                            .uri
                            .path()
                            .strip_prefix('/')
                            .unwrap_or(request_parts.uri.path())
                            .split_once('/'),
                    ) {
                        (Method::GET, Some(("public", path))) => {
                            let sub_path = Path::new(path);
                            if let Ok(rest) = sub_path.strip_prefix("package-data") {
                                file_send(ctx.datadir.join(PKG_PUBLIC_DIR).join(rest)).await
                            } else if let Ok(rest) = sub_path.strip_prefix("eos") {
                                match rest.to_str() {
                                    Some("local.crt") => {
                                        file_send(crate::net::ssl::ROOT_CA_STATIC_PATH).await
                                    }
                                    None => Ok(bad_request()),
                                    _ => Ok(not_found()),
                                }
                            } else {
                                Ok(not_found())
                            }
                        }
                        (Method::GET, Some(("eos", "local.crt"))) => {
                            file_send(PathBuf::from(crate::net::ssl::ROOT_CA_STATIC_PATH)).await
                        }

                        (Method::GET, None) => {
                            let uri_path = request_parts
                                .uri
                                .path()
                                .strip_prefix('/')
                                .unwrap_or(request_parts.uri.path());

                            let full_path = PathBuf::from(selected_root_dir).join(uri_path);
                            file_send(full_path).await
                        }

                        (Method::GET, Some((dir, file))) => {
                            let full_path = PathBuf::from(selected_root_dir).join(dir).join(file);
                            file_send(full_path).await
                        }

                        _ => Ok(not_found()),
                    }
                }
                Err(err) => {
                    match (
                        request_parts.method,
                        request_parts
                            .uri
                            .path()
                            .strip_prefix('/')
                            .unwrap_or(request_parts.uri.path())
                            .split_once('/'),
                    ) {
                        (Method::GET, Some(("public", _path))) => {
                            un_authorized(err, request_parts.uri.path())
                        }
                        (Method::GET, Some(("eos", "local.crt"))) => {
                            un_authorized(err, request_parts.uri.path())
                        }
                        (Method::GET, None) => {
                            let uri_path = request_parts
                                .uri
                                .path()
                                .strip_prefix('/')
                                .unwrap_or(request_parts.uri.path());

                            let full_path = PathBuf::from(selected_root_dir).join(uri_path);
                            file_send(full_path).await
                        }

                        (Method::GET, Some((dir, file))) => {
                            let full_path = PathBuf::from(selected_root_dir).join(dir).join(file);
                            file_send(full_path).await
                        }

                        _ => Ok(not_found()),
                    }
                }
            }
        }
    }
}

fn un_authorized(err: Error, path: &str) -> Result<Response<Body>, Error> {
    tracing::warn!("unauthorized for {} @{:?}", err, path);
    tracing::debug!("{:?}", err);
    Ok(Response::builder()
        .status(StatusCode::UNAUTHORIZED)
        .body(NOT_AUTHORIZED.into())
        .unwrap())
}

/// HTTP status code 404
fn not_found() -> Response<Body> {
    Response::builder()
        .status(StatusCode::NOT_FOUND)
        .body(NOT_FOUND.into())
        .unwrap()
}

fn server_error(err: Error) -> Response<Body> {
    Response::builder()
        .status(StatusCode::INTERNAL_SERVER_ERROR)
        .body(err.to_string().into())
        .unwrap()
}

fn bad_request() -> Response<Body> {
    Response::builder()
        .status(StatusCode::BAD_REQUEST)
        .body(Body::empty())
        .unwrap()
}

async fn file_send(path: impl AsRef<Path>) -> Result<Response<Body>, Error> {
    // Serve a file by asynchronously reading it by chunks using tokio-util crate.

    let path = path.as_ref();

    if let Ok(file) = File::open(path).await {
        let metadata = file.metadata().await.with_kind(ErrorKind::Filesystem)?;

        match IsNonEmptyFile::new(&metadata, path) {
            Some(a) => a,
            None => return Ok(not_found()),
        };

        let mut builder = Response::builder().status(StatusCode::OK);
        builder = with_e_tag(path, &metadata, builder)?;
        builder = with_content_type(path, builder);
        builder = with_content_length(&metadata, builder);
        let stream = FramedRead::new(file, BytesCodec::new());
        let body = Body::wrap_stream(stream);
        return builder.body(body).with_kind(ErrorKind::Network);
    }
    tracing::debug!("File not found: {:?}", path);

    Ok(not_found())
}

struct IsNonEmptyFile(());
impl IsNonEmptyFile {
    fn new(metadata: &Metadata, path: &Path) -> Option<Self> {
        let length = metadata.len();
        if !metadata.is_file() || length == 0 {
            tracing::debug!("File is empty: {:?}", path);
            return None;
        }
        Some(Self(()))
    }
}

fn with_e_tag(path: &Path, metadata: &Metadata, builder: Builder) -> Result<Builder, Error> {
    let modified = metadata.modified().with_kind(ErrorKind::Filesystem)?;
    let mut hasher = sha2::Sha256::new();
    hasher.update(format!("{:?}", path).as_bytes());
    hasher.update(
        format!(
            "{}",
            modified
                .duration_since(UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs()
        )
        .as_bytes(),
    );
    let res = hasher.finalize();
    Ok(builder.header(
        "ETag",
        base32::encode(base32::Alphabet::RFC4648 { padding: false }, res.as_slice()).to_lowercase(),
    ))
}
///https://en.wikipedia.org/wiki/Media_type
fn with_content_type(path: &Path, builder: Builder) -> Builder {
    let content_type = match path.extension() {
        Some(os_str) => match os_str.to_str() {
            Some("apng") => "image/apng",
            Some("avif") => "image/avif",
            Some("flif") => "image/flif",
            Some("gif") => "image/gif",
            Some("jpg") | Some("jpeg") | Some("jfif") | Some("pjpeg") | Some("pjp") => "image/jpeg",
            Some("jxl") => "image/jxl",
            Some("png") => "image/png",
            Some("svg") => "image/svg+xml",
            Some("webp") => "image/webp",
            Some("mng") | Some("x-mng") => "image/x-mng",
            Some("css") => "text/css",
            Some("csv") => "text/csv",
            Some("html") => "text/html",
            Some("php") => "text/php",
            Some("plain") | Some("md") | Some("txt") => "text/plain",
            Some("xml") => "text/xml",
            Some("js") => "text/javascript",
            Some("wasm") => "application/wasm",
            None | Some(_) => "text/plain",
        },
        None => "text/plain",
    };
    builder.header("Content-Type", content_type)
}

fn with_content_length(metadata: &Metadata, builder: Builder) -> Builder {
    builder.header(http::header::CONTENT_LENGTH, metadata.len())
}
