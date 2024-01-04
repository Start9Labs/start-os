use std::fs::Metadata;
use std::future::Future;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::UNIX_EPOCH;

use async_compression::tokio::bufread::GzipEncoder;
use axum::{
    extract::{self as x, State},
    routing::{any, any_service},
    Router,
};
use digest::Digest;
use futures::future::ready;
use futures::FutureExt;
use http::header::ACCEPT_ENCODING;
use http::request::Parts as RequestParts;
use http_body_util::BodyStream;
use hyper::body::Incoming;
use hyper::{Method, Request, Response, StatusCode};
use include_dir::{include_dir, Dir};
use new_mime_guess::MimeGuess;
use openssl::hash::MessageDigest;
use openssl::x509::X509;
use rpc_toolkit::Server;
use tokio::fs::File;
use tokio::io::BufReader;
use tokio_util::io::ReaderStream;

use crate::context::{DiagnosticContext, InstallContext, RpcContext, SetupContext};
use crate::core::rpc_continuations::RequestGuid;
use crate::db::subscribe;
use crate::hostname::Hostname;
use crate::install::PKG_PUBLIC_DIR;
use crate::middleware::auth::{Auth, HasValidSession};
use crate::middleware::cors::Cors;
use crate::middleware::db::SyncDb;
use crate::middleware::diagnostic::DiagnosticMode;
use crate::net::HttpHandler;
use crate::{diagnostic_api, install_api, main_api, setup_api, Error, ErrorKind, ResultExt};

static NOT_FOUND: &[u8] = b"Not Found";
static METHOD_NOT_ALLOWED: &[u8] = b"Method Not Allowed";
static NOT_AUTHORIZED: &[u8] = b"Not Authorized";

static EMBEDDED_UIS: Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/../../web/dist/static");

const PROXY_STRIP_HEADERS: &[&str] = &["cookie", "host", "origin", "referer", "user-agent"];

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

impl UiMode {
    fn path(&self, path: &str) -> PathBuf {
        match self {
            Self::Setup => Path::new("setup-wizard").join(path),
            Self::Diag => Path::new("diagnostic-ui").join(path),
            Self::Install => Path::new("install-wizard").join(path),
            Self::Main => Path::new("ui").join(path),
        }
    }
}

pub fn setup_ui_file_router(ctx: SetupContext) -> Router {
    Router::new()
        .route_service(
            "/rpc/*path",
            any_service(
                Server::new(move || ready(Ok(ctx.clone())), setup_api()).middleware(Cors::new()),
            ),
        )
        .fallback(any(|request: Request| alt_ui(request, UiMode::Setup)))
}

pub fn diag_ui_file_router(ctx: DiagnosticContext) -> Router {
    Router::new()
        .route(
            "/rpc/*path",
            any(
                Server::new(move || ready(Ok(ctx.clone())), diagnostic_api())
                    .middleware(Cors::new())
                    .middleware(DiagnosticMode::new()),
            ),
        )
        .fallback(any(|request: Request| alt_ui(request, UiMode::Diag)))
}

pub async fn install_ui_file_router(ctx: InstallContext) -> Router {
    Router::new()
        .route(
            "/rpc/*path",
            any(Server::new(move || ready(Ok(ctx.clone())), install_api()).middleware(Cors::new())),
        )
        .fallback(any(|request: Request| alt_ui(request, UiMode::Install)))
}

pub async fn main_ui_server_router(ctx: RpcContext) -> Router<RpcContext> {
    Router::new()
        .with_state(ctx.clone())
        .route(
            "/rpc/*path",
            any(Server::new(move || ready(Ok(ctx.clone())), main_api())
                .middleware(Cors::new())
                .middleware(Auth::new())),
        )
        .route(
            "/ws/db",
            any(|request: Request, State(ctx): State<RpcContext>| subscribe(ctx, request)),
        )
        .route(
            "/ws/rpc/*path",
            any(
                |request: Request,
                 x::Path(path): x::Path<String>,
                 State(ctx): State<RpcContext>| async move {
                    match RequestGuid::from(&path) {
                        None => {
                            tracing::debug!("No Guid Path");
                            Ok::<_, Error>(bad_request())
                        }
                        Some(guid) => match ctx.get_ws_continuation_handler(&guid).await {
                            Some(cont) => match cont(request).await {
                                Ok::<_, Error>(r) => Ok::<_, Error>(r),
                                Err(err) => Ok::<_, Error>(server_error(err)),
                            },
                            _ => Ok::<_, Error>(not_found()),
                        },
                    }
                },
            ),
        )
        .route(
            "/rest/rpc/*path",
            any(
                |request: Request,
                 x::Path(path): x::Path<String>,
                 State(ctx): State<RpcContext>| async move {
                    match RequestGuid::from(&path) {
                        None => {
                            tracing::debug!("No Guid Path");
                            Ok::<_, Error>(bad_request())
                        }
                        Some(guid) => match ctx.get_rest_continuation_handler(&guid).await {
                            None => Ok::<_, Error>(not_found()),
                            Some(cont) => match cont(request).await {
                                Ok::<_, Error>(r) => Ok::<_, Error>(r),
                                Err(e) => Ok::<_, Error>(server_error(e)),
                            },
                        },
                    }
                },
            ),
        )
        .fallback(any(|request: Request| {
            main_embassy_ui(request, UiMode::Main)
        }))
}

async fn alt_ui(req: Request<Incoming>, ui_mode: UiMode) -> Result<Response<BoxBody>, Error> {
    let (request_parts, _body) = req.into_parts();
    match &request_parts.method {
        &Method::GET => {
            let uri_path = ui_mode.path(
                request_parts
                    .uri
                    .path()
                    .strip_prefix('/')
                    .unwrap_or(request_parts.uri.path()),
            );

            let file = EMBEDDED_UIS
                .get_file(&*uri_path)
                .or_else(|| EMBEDDED_UIS.get_file(&*ui_mode.path("index.html")));

            if let Some(file) = file {
                FileData::from_embedded(&request_parts, file)
                    .into_response(&request_parts)
                    .await
            } else {
                Ok(not_found())
            }
        }
        _ => Ok(method_not_allowed()),
    }
}

async fn if_authorized<
    F: FnOnce() -> Fut,
    Fut: Future<Output = Result<Response<BoxBody>, Error>> + Send + Sync,
>(
    ctx: &RpcContext,
    parts: &RequestParts,
    f: F,
) -> Result<Response<BoxBody>, Error> {
    if let Err(e) = HasValidSession::from_request_parts(parts, ctx).await {
        un_authorized(e, parts.uri.path())
    } else {
        f().await
    }
}

async fn main_embassy_ui(
    req: Request<Incoming>,
    ctx: RpcContext,
) -> Result<Response<BoxBody>, Error> {
    let (request_parts, _body) = req.into_parts();
    match (
        &request_parts.method,
        request_parts
            .uri
            .path()
            .strip_prefix('/')
            .unwrap_or(request_parts.uri.path())
            .split_once('/'),
    ) {
        (&Method::GET, Some(("public", path))) => {
            if_authorized(&ctx, &request_parts, || async {
                let sub_path = Path::new(path);
                if let Ok(rest) = sub_path.strip_prefix("package-data") {
                    FileData::from_path(
                        &request_parts,
                        &ctx.datadir.join(PKG_PUBLIC_DIR).join(rest),
                    )
                    .await?
                    .into_response(&request_parts)
                    .await
                } else {
                    Ok(not_found())
                }
            })
            .await
        }
        (&Method::GET, Some(("proxy", target))) => {
            if_authorized(&ctx, &request_parts, || async {
                let target = urlencoding::decode(target)?;
                let res = ctx
                    .client
                    .get(target.as_ref())
                    .headers(
                        request_parts
                            .headers
                            .iter()
                            .filter(|(h, _)| {
                                !PROXY_STRIP_HEADERS
                                    .iter()
                                    .any(|bad| h.as_str().eq_ignore_ascii_case(bad))
                            })
                            .map(|(h, v)| (h.clone(), v.clone()))
                            .collect(),
                    )
                    .send()
                    .await
                    .with_kind(crate::ErrorKind::Network)?;
                let mut hres = Response::builder().status(res.status());
                for (h, v) in res.headers().clone() {
                    if let Some(h) = h {
                        hres = hres.header(h, v);
                    }
                }
                hres.body(BoxBody::new(BodyStream::new(res.bytes_stream())))
                    .with_kind(crate::ErrorKind::Network)
            })
            .await
        }
        (&Method::GET, Some(("eos", "local.crt"))) => {
            let account = ctx.account.read().await;
            cert_send(&account.root_ca_cert, &account.hostname)
        }
        (&Method::GET, _) => {
            let uri_path = UiMode::Main.path(
                request_parts
                    .uri
                    .path()
                    .strip_prefix('/')
                    .unwrap_or(request_parts.uri.path()),
            );

            let file = EMBEDDED_UIS
                .get_file(&*uri_path)
                .or_else(|| EMBEDDED_UIS.get_file(&*UiMode::Main.path("index.html")));

            if let Some(file) = file {
                FileData::from_embedded(&request_parts, file)
                    .into_response(&request_parts)
                    .await
            } else {
                Ok(not_found())
            }
        }
        _ => Ok(method_not_allowed()),
    }
}

fn un_authorized(err: Error, path: &str) -> Result<Response<BoxBody>, Error> {
    tracing::warn!("unauthorized for {} @{:?}", err, path);
    tracing::debug!("{:?}", err);
    Ok(Response::builder()
        .status(StatusCode::UNAUTHORIZED)
        .body(NOT_AUTHORIZED.into())
        .unwrap())
}

/// HTTP status code 404
fn not_found() -> Response<BoxBody> {
    Response::builder()
        .status(StatusCode::NOT_FOUND)
        .body(NOT_FOUND.into())
        .unwrap()
}

/// HTTP status code 405
fn method_not_allowed() -> Response<BoxBody> {
    Response::builder()
        .status(StatusCode::METHOD_NOT_ALLOWED)
        .body(METHOD_NOT_ALLOWED.into())
        .unwrap()
}

fn server_error(err: Error) -> Response<BoxBody> {
    Response::builder()
        .status(StatusCode::INTERNAL_SERVER_ERROR)
        .body(err.to_string().into())
        .unwrap()
}

fn bad_request() -> Response<BoxBody> {
    Response::builder()
        .status(StatusCode::BAD_REQUEST)
        .body(BoxBody::empty())
        .unwrap()
}

fn cert_send(cert: &X509, hostname: &Hostname) -> Result<Response<BoxBody>, Error> {
    let pem = cert.to_pem()?;
    Response::builder()
        .status(StatusCode::OK)
        .header(
            http::header::ETAG,
            base32::encode(
                base32::Alphabet::RFC4648 { padding: false },
                &*cert.digest(MessageDigest::sha256())?,
            )
            .to_lowercase(),
        )
        .header(http::header::CONTENT_TYPE, "application/x-x509-ca-cert")
        .header(http::header::CONTENT_LENGTH, pem.len())
        .header(
            http::header::CONTENT_DISPOSITION,
            format!("attachment; filename={}.crt", &hostname.0),
        )
        .body(BoxBody::new(pem))
        .with_kind(ErrorKind::Network)
}

struct FileData {
    data: BoxBody,
    len: Option<u64>,
    encoding: Option<&'static str>,
    e_tag: String,
    mime: Option<String>,
}
impl FileData {
    fn from_embedded(req: &RequestParts, file: &'static include_dir::File<'static>) -> Self {
        let path = file.path();
        let (encoding, data) = req
            .headers
            .get_all(ACCEPT_ENCODING)
            .into_iter()
            .filter_map(|h| h.to_str().ok())
            .flat_map(|s| s.split(","))
            .filter_map(|s| s.split(";").next())
            .map(|s| s.trim())
            .fold((None, file.contents()), |acc, e| {
                if let Some(file) = (e == "br")
                    .then_some(())
                    .and_then(|_| EMBEDDED_UIS.get_file(format!("{}.br", path.display())))
                {
                    (Some("br"), file.contents())
                } else if let Some(file) = (e == "gzip" && acc.0 != Some("br"))
                    .then_some(())
                    .and_then(|_| EMBEDDED_UIS.get_file(format!("{}.gz", path.display())))
                {
                    (Some("gzip"), file.contents())
                } else {
                    acc
                }
            });

        Self {
            len: Some(data.len() as u64),
            encoding,
            data: data.into(),
            e_tag: e_tag(path, None),
            mime: MimeGuess::from_path(path)
                .first()
                .map(|m| m.essence_str().to_owned()),
        }
    }

    async fn from_path(req: &RequestParts, path: &Path) -> Result<Self, Error> {
        let encoding = req
            .headers
            .get_all(ACCEPT_ENCODING)
            .into_iter()
            .filter_map(|h| h.to_str().ok())
            .flat_map(|s| s.split(","))
            .filter_map(|s| s.split(";").next())
            .map(|s| s.trim())
            .any(|e| e == "gzip")
            .then_some("gzip");

        let file = File::open(path)
            .await
            .with_ctx(|_| (ErrorKind::Filesystem, path.display().to_string()))?;
        let metadata = file
            .metadata()
            .await
            .with_ctx(|_| (ErrorKind::Filesystem, path.display().to_string()))?;

        let e_tag = e_tag(path, Some(&metadata));

        let (len, data) = if encoding == Some("gzip") {
            (
                None,
                BoxBody::new(BodyStream::new(ReaderStream::new(GzipEncoder::new(
                    BufReader::new(file),
                )))),
            )
        } else {
            (
                Some(metadata.len()),
                BoxBody::new(BodyStream::new(ReaderStream::new(file))),
            )
        };

        Ok(Self {
            data,
            len,
            encoding,
            e_tag,
            mime: MimeGuess::from_path(path)
                .first()
                .map(|m| m.essence_str().to_owned()),
        })
    }

    async fn into_response(self, req: &RequestParts) -> Result<Response<BoxBody>, Error> {
        let mut builder = Response::builder();
        if let Some(mime) = self.mime {
            builder = builder.header(http::header::CONTENT_TYPE, &*mime);
        }
        builder = builder.header(http::header::ETAG, &*self.e_tag);
        builder = builder.header(
            http::header::CACHE_CONTROL,
            "public, max-age=21000000, immutable",
        );

        if req
            .headers
            .get_all(http::header::CONNECTION)
            .iter()
            .flat_map(|s| s.to_str().ok())
            .flat_map(|s| s.split(","))
            .any(|s| s.trim() == "keep-alive")
        {
            builder = builder.header(http::header::CONNECTION, "keep-alive");
        }

        if req
            .headers
            .get("if-none-match")
            .and_then(|h| h.to_str().ok())
            == Some(self.e_tag.as_ref())
        {
            builder = builder.status(StatusCode::NOT_MODIFIED);
            builder.body(BoxBody::new(http_body_util::Empty::new()))
        } else {
            if let Some(len) = self.len {
                builder = builder.header(http::header::CONTENT_LENGTH, len);
            }
            if let Some(encoding) = self.encoding {
                builder = builder.header(http::header::CONTENT_ENCODING, encoding);
            }

            builder.body(self.data)
        }
        .with_kind(ErrorKind::Network)
    }
}

fn e_tag(path: &Path, metadata: Option<&Metadata>) -> String {
    let mut hasher = sha2::Sha256::new();
    hasher.update(format!("{:?}", path).as_bytes());
    if let Some(modified) = metadata.and_then(|m| m.modified().ok()) {
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
    }
    let res = hasher.finalize();
    format!(
        "\"{}\"",
        base32::encode(base32::Alphabet::RFC4648 { padding: false }, res.as_slice()).to_lowercase()
    )
}
