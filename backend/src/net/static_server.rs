use std::borrow::Cow;
use std::fs::Metadata;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::UNIX_EPOCH;

use async_compression::tokio::bufread::GzipEncoder;
use color_eyre::eyre::eyre;
use digest::Digest;
use futures::FutureExt;
use http::header::ACCEPT_ENCODING;
use http::request::Parts as RequestParts;
use http::response::Builder;
use hyper::{Body, Method, Request, Response, StatusCode};
use include_dir::{include_dir, Dir};
use new_mime_guess::MimeGuess;
use openssl::hash::MessageDigest;
use openssl::x509::X509;
use rpc_toolkit::rpc_handler;
use tokio::fs::File;
use tokio::io::BufReader;
use tokio_util::io::ReaderStream;

use crate::context::{DiagnosticContext, InstallContext, RpcContext, SetupContext};
use crate::core::rpc_continuations::RequestGuid;
use crate::db::subscribe;
use crate::install::PKG_PUBLIC_DIR;
use crate::middleware::auth::{auth as auth_middleware, HasValidSession};
use crate::middleware::cors::cors;
use crate::middleware::db::db as db_middleware;
use crate::middleware::diagnostic::diagnostic as diagnostic_middleware;
use crate::net::HttpHandler;
use crate::{diagnostic_api, install_api, main_api, setup_api, Error, ErrorKind, ResultExt};

static NOT_FOUND: &[u8] = b"Not Found";
static METHOD_NOT_ALLOWED: &[u8] = b"Method Not Allowed";
static NOT_AUTHORIZED: &[u8] = b"Not Authorized";

static EMBEDDED_UIS: Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/../frontend/dist/static");

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

pub async fn setup_ui_file_router(ctx: SetupContext) -> Result<HttpHandler, Error> {
    let handler: HttpHandler = Arc::new(move |req| {
        let ctx = ctx.clone();

        let ui_mode = UiMode::Setup;
        async move {
            let res = match req.uri().path() {
                path if path.starts_with("/rpc/") => {
                    let rpc_handler = rpc_handler!({
                        command: setup_api,
                        context: ctx,
                        status: status_fn,
                        middleware: [
                            cors,
                        ]
                    });

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
                    let rpc_handler = rpc_handler!({
                        command: diagnostic_api,
                        context: ctx,
                        status: status_fn,
                        middleware: [
                            cors,
                            diagnostic_middleware,
                        ]
                    });

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
                    let rpc_handler = rpc_handler!({
                        command: install_api,
                        context: ctx,
                        status: status_fn,
                        middleware: [
                            cors,
                        ]
                    });

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
                    let auth_middleware = auth_middleware(ctx.clone());
                    let db_middleware = db_middleware(ctx.clone());
                    let rpc_handler = rpc_handler!({
                        command: main_api,
                        context: ctx,
                        status: status_fn,
                        middleware: [
                            cors,
                            auth_middleware,
                            db_middleware,
                        ]
                    });

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
    let (request_parts, _body) = req.into_parts();
    let accept_encoding = request_parts
        .headers
        .get_all(ACCEPT_ENCODING)
        .into_iter()
        .filter_map(|h| h.to_str().ok())
        .flat_map(|s| s.split(","))
        .filter_map(|s| s.split(";").next())
        .map(|s| s.trim())
        .collect::<Vec<_>>();
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

async fn main_embassy_ui(req: Request<Body>, ctx: RpcContext) -> Result<Response<Body>, Error> {
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
            match HasValidSession::from_request_parts(&request_parts, &ctx).await {
                Ok(_) => {
                    let sub_path = Path::new(path);
                    if let Ok(rest) = sub_path.strip_prefix("package-data") {
                        FileData::from_path(
                            &request_parts,
                            &ctx.datadir.join(PKG_PUBLIC_DIR).join(rest),
                        )
                        .await?
                        .into_response(&request_parts)
                        .await
                    } else if let Ok(rest) = sub_path.strip_prefix("eos") {
                        match rest.to_str() {
                            Some("local.crt") => cert_send(&ctx.account.read().await.root_ca_cert),
                            None => Ok(bad_request()),
                            _ => Ok(not_found()),
                        }
                    } else {
                        Ok(not_found())
                    }
                }
                Err(e) => un_authorized(e, &format!("public/{path}")),
            }
        }
        (&Method::GET, Some(("eos", "local.crt"))) => {
            match HasValidSession::from_request_parts(&request_parts, &ctx).await {
                Ok(_) => cert_send(&ctx.account.read().await.root_ca_cert),
                Err(e) => un_authorized(e, "eos/local.crt"),
            }
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

/// HTTP status code 405
fn method_not_allowed() -> Response<Body> {
    Response::builder()
        .status(StatusCode::METHOD_NOT_ALLOWED)
        .body(METHOD_NOT_ALLOWED.into())
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

fn cert_send(cert: &X509) -> Result<Response<Body>, Error> {
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
        .header(http::header::CONTENT_TYPE, "application/x-pem-file")
        .header(http::header::CONTENT_LENGTH, pem.len())
        .body(Body::from(pem))
        .with_kind(ErrorKind::Network)
}

struct FileData {
    data: Body,
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
                Body::wrap_stream(ReaderStream::new(GzipEncoder::new(BufReader::new(file)))),
            )
        } else {
            (
                Some(metadata.len()),
                Body::wrap_stream(ReaderStream::new(file)),
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

    async fn into_response(self, req: &RequestParts) -> Result<Response<Body>, Error> {
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

#[test]
fn test_packed_html() {
    assert!(MainUi::get("index.html").is_some())
}
