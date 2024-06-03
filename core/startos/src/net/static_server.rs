use std::future::Future;
use std::path::{Path, PathBuf};
use std::time::UNIX_EPOCH;

use async_compression::tokio::bufread::GzipEncoder;
use axum::body::Body;
use axum::extract::{self as x, Request};
use axum::response::Response;
use axum::routing::{any, get, post};
use axum::Router;
use digest::Digest;
use futures::future::ready;
use http::header::ACCEPT_ENCODING;
use http::request::Parts as RequestParts;
use http::{Method, StatusCode};
use imbl_value::InternedString;
use include_dir::Dir;
use new_mime_guess::MimeGuess;
use openssl::hash::MessageDigest;
use openssl::x509::X509;
use rpc_toolkit::{Context, HttpServer, Server};
use tokio::fs::File;
use tokio::io::BufReader;
use tokio_util::io::ReaderStream;

use crate::context::{DiagnosticContext, InitContext, InstallContext, RpcContext, SetupContext};
use crate::hostname::Hostname;
use crate::middleware::auth::{Auth, HasValidSession};
use crate::middleware::cors::Cors;
use crate::middleware::db::SyncDb;
use crate::rpc_continuations::{Guid, RpcContinuations};
use crate::{
    diagnostic_api, init_api, install_api, main_api, setup_api, Error, ErrorKind, ResultExt,
};

const NOT_FOUND: &[u8] = b"Not Found";
const METHOD_NOT_ALLOWED: &[u8] = b"Method Not Allowed";
const NOT_AUTHORIZED: &[u8] = b"Not Authorized";
const INTERNAL_SERVER_ERROR: &[u8] = b"Internal Server Error";

#[cfg(all(feature = "daemon", not(feature = "test")))]
const EMBEDDED_UIS: Dir<'_> =
    include_dir::include_dir!("$CARGO_MANIFEST_DIR/../../web/dist/static");
#[cfg(not(all(feature = "daemon", not(feature = "test"))))]
const EMBEDDED_UIS: Dir<'_> = Dir::new("", &[]);

const PROXY_STRIP_HEADERS: &[&str] = &["cookie", "host", "origin", "referer", "user-agent"];

#[derive(Clone)]
pub enum UiMode {
    Setup,
    Install,
    Main,
}

impl UiMode {
    fn path(&self, path: &str) -> PathBuf {
        match self {
            Self::Setup => Path::new("setup-wizard").join(path),
            Self::Install => Path::new("install-wizard").join(path),
            Self::Main => Path::new("ui").join(path),
        }
    }
}

pub fn rpc_router<C: Context + Clone + AsRef<RpcContinuations>>(
    ctx: C,
    server: HttpServer<C>,
) -> Router {
    Router::new()
        .route("/rpc/*path", post(server))
        .route(
            "/ws/rpc/:guid",
            get({
                let ctx = ctx.clone();
                move |x::Path(guid): x::Path<Guid>,
                      ws: axum::extract::ws::WebSocketUpgrade| async move {
                    match AsRef::<RpcContinuations>::as_ref(&ctx).get_ws_handler(&guid).await {
                        Some(cont) => ws.on_upgrade(cont),
                        _ => not_found(),
                    }
                }
            }),
        )
        .route(
            "/rest/rpc/:guid",
            any({
                let ctx = ctx.clone();
                move |x::Path(guid): x::Path<Guid>, request: x::Request| async move {
                    match AsRef::<RpcContinuations>::as_ref(&ctx).get_rest_handler(&guid).await {
                        None => not_found(),
                        Some(cont) => cont(request).await.unwrap_or_else(server_error),
                    }
                }
            }),
        )
}

fn serve_ui(req: Request, ui_mode: UiMode) -> Result<Response, Error> {
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
                FileData::from_embedded(&request_parts, file).into_response(&request_parts)
            } else {
                Ok(not_found())
            }
        }
        _ => Ok(method_not_allowed()),
    }
}

pub fn setup_ui_router(ctx: SetupContext) -> Router {
    rpc_router(
        ctx.clone(),
        Server::new(move || ready(Ok(ctx.clone())), setup_api()).middleware(Cors::new()),
    )
    .fallback(any(|request: Request| async move {
        serve_ui(request, UiMode::Setup).unwrap_or_else(server_error)
    }))
}

pub fn diagnostic_ui_router(ctx: DiagnosticContext) -> Router {
    rpc_router(
        ctx.clone(),
        Server::new(move || ready(Ok(ctx.clone())), diagnostic_api()).middleware(Cors::new()),
    )
    .fallback(any(|request: Request| async move {
        serve_ui(request, UiMode::Main).unwrap_or_else(server_error)
    }))
}

pub fn install_ui_router(ctx: InstallContext) -> Router {
    rpc_router(
        ctx.clone(),
        Server::new(move || ready(Ok(ctx.clone())), install_api()).middleware(Cors::new()),
    )
    .fallback(any(|request: Request| async move {
        serve_ui(request, UiMode::Install).unwrap_or_else(server_error)
    }))
}

pub fn init_ui_router(ctx: InitContext) -> Router {
    rpc_router(
        ctx.clone(),
        Server::new(move || ready(Ok(ctx.clone())), init_api()).middleware(Cors::new()),
    )
    .fallback(any(|request: Request| async move {
        serve_ui(request, UiMode::Main).unwrap_or_else(server_error)
    }))
}

pub fn main_ui_router(ctx: RpcContext) -> Router {
    rpc_router(
        ctx.clone(),
        Server::new(move || ready(Ok(ctx.clone())), main_api::<RpcContext>())
            .middleware(Cors::new())
            .middleware(Auth::new())
            .middleware(SyncDb::new()),
    )
    // TODO: cert
    .fallback(any(|request: Request| async move {
        serve_ui(request, UiMode::Main).unwrap_or_else(server_error)
    }))
}

pub fn refresher() -> Router {
    Router::new().fallback(get(|request: Request| async move {
        let res = include_bytes!("./refresher.html");
        FileData {
            data: Body::from(&res[..]),
            e_tag: None,
            encoding: None,
            len: Some(res.len() as u64),
            mime: Some("text/html".into()),
        }
        .into_response(&request.into_parts().0)
        .unwrap_or_else(server_error)
    }))
}

async fn if_authorized<
    F: FnOnce() -> Fut,
    Fut: Future<Output = Result<Response, Error>> + Send + Sync,
>(
    ctx: &RpcContext,
    parts: &RequestParts,
    f: F,
) -> Result<Response, Error> {
    if let Err(e) = HasValidSession::from_header(parts.headers.get(http::header::COOKIE), ctx).await
    {
        Ok(unauthorized(e, parts.uri.path()))
    } else {
        f().await
    }
}

pub fn unauthorized(err: Error, path: &str) -> Response {
    tracing::warn!("unauthorized for {} @{:?}", err, path);
    tracing::debug!("{:?}", err);
    Response::builder()
        .status(StatusCode::UNAUTHORIZED)
        .body(NOT_AUTHORIZED.into())
        .unwrap()
}

/// HTTP status code 404
pub fn not_found() -> Response {
    Response::builder()
        .status(StatusCode::NOT_FOUND)
        .body(NOT_FOUND.into())
        .unwrap()
}

/// HTTP status code 405
pub fn method_not_allowed() -> Response {
    Response::builder()
        .status(StatusCode::METHOD_NOT_ALLOWED)
        .body(METHOD_NOT_ALLOWED.into())
        .unwrap()
}

pub fn server_error(err: Error) -> Response {
    tracing::error!("internal server error: {}", err);
    tracing::debug!("{:?}", err);
    Response::builder()
        .status(StatusCode::INTERNAL_SERVER_ERROR)
        .body(INTERNAL_SERVER_ERROR.into())
        .unwrap()
}

pub fn bad_request() -> Response {
    Response::builder()
        .status(StatusCode::BAD_REQUEST)
        .body(Body::empty())
        .unwrap()
}

fn cert_send(cert: &X509, hostname: &Hostname) -> Result<Response, Error> {
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
        .body(Body::from(pem))
        .with_kind(ErrorKind::Network)
}

struct FileData {
    data: Body,
    len: Option<u64>,
    encoding: Option<&'static str>,
    e_tag: Option<String>,
    mime: Option<InternedString>,
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
            e_tag: file.metadata().map(|metadata| {
                e_tag(
                    path,
                    format!(
                        "{}",
                        metadata
                            .modified()
                            .duration_since(UNIX_EPOCH)
                            .map(|d| d.as_secs() as i64)
                            .unwrap_or_else(|e| e.duration().as_secs() as i64 * -1),
                    )
                    .as_bytes(),
                )
            }),
            mime: MimeGuess::from_path(path)
                .first()
                .map(|m| m.essence_str().into()),
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

        let e_tag = Some(e_tag(
            path,
            format!(
                "{}",
                metadata
                    .modified()?
                    .duration_since(UNIX_EPOCH)
                    .map(|d| d.as_secs() as i64)
                    .unwrap_or_else(|e| e.duration().as_secs() as i64 * -1)
            )
            .as_bytes(),
        ));

        let (len, data) = if encoding == Some("gzip") {
            (
                None,
                Body::from_stream(ReaderStream::new(GzipEncoder::new(BufReader::new(file)))),
            )
        } else {
            (
                Some(metadata.len()),
                Body::from_stream(ReaderStream::new(file)),
            )
        };

        Ok(Self {
            data,
            len,
            encoding,
            e_tag,
            mime: MimeGuess::from_path(path)
                .first()
                .map(|m| m.essence_str().into()),
        })
    }

    fn into_response(self, req: &RequestParts) -> Result<Response, Error> {
        let mut builder = Response::builder();
        if let Some(mime) = self.mime {
            builder = builder.header(http::header::CONTENT_TYPE, &*mime);
        }
        if let Some(e_tag) = &self.e_tag {
            builder = builder.header(http::header::ETAG, &**e_tag);
        }
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
            == self.e_tag.as_deref()
        {
            builder = builder.status(StatusCode::NOT_MODIFIED);
            builder.body(Body::empty())
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

lazy_static::lazy_static! {
    static ref INSTANCE_NONCE: u64 = rand::random();
}

fn e_tag(path: &Path, modified: impl AsRef<[u8]>) -> String {
    let mut hasher = sha2::Sha256::new();
    hasher.update(format!("{:?}", path).as_bytes());
    hasher.update(modified.as_ref());
    let res = hasher.finalize();
    format!(
        "\"{}\"",
        base32::encode(base32::Alphabet::RFC4648 { padding: false }, res.as_slice()).to_lowercase()
    )
}
