use std::fs::Metadata;
use std::future::Future;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::UNIX_EPOCH;

use digest::Digest;
use futures::FutureExt;
use helpers::NonDetachingJoinHandle;
use http::response::Builder;
use hyper::service::{make_service_fn, service_fn};
use hyper::{Body, Error as HyperError, Method, Request, Response, Server, StatusCode};
use tokio::fs::File;
use tokio_util::codec::{BytesCodec, FramedRead};
use tracing::error;

use crate::context::RpcContext;
use crate::install::PKG_PUBLIC_DIR;
use crate::middleware::auth::HasValidSession;
use crate::net::HttpHandler;
use crate::{Error, ErrorKind, ResultExt};

static NOT_FOUND: &[u8] = b"Not Found";
static NOT_AUTHORIZED: &[u8] = b"Not Authorized";

pub const WWW_DIR: &str = "/var/www/html/main";

pub async fn file_server_router(ctx: RpcContext) -> Result<HttpHandler, Error> {
    let handler: HttpHandler = Arc::new(move |req| {
        let ctx = ctx.clone();
        async move {
            dbg!(req.uri());
            let (request_parts, _body) = req.into_parts();
            let valid_session = HasValidSession::from_request_parts(&request_parts, &ctx).await;
            if request_parts.uri.path() == "/" {
                let full_path = PathBuf::from(WWW_DIR).join("index.html");
                dbg!(full_path.clone().display());

                match file_send(full_path).await {
                    Ok(resp) => Ok(resp),
                    Err(err) => Ok(server_error(err)),
                }
            } else {
                let t = match valid_session {
                    Ok(_valid) => {
                        let test123 = request_parts
                            .uri
                            .path()
                            .strip_prefix('/')
                            .unwrap_or(request_parts.uri.path())
                            .split_once('/');

                        dbg!(test123);
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
                                file_send(ctx.datadir.join(PKG_PUBLIC_DIR).join(path)).await
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
                                dbg!(uri_path);

                                let full_path = PathBuf::from(WWW_DIR).join(uri_path);
                                dbg!(full_path.clone().display());
                                file_send(full_path).await
                            }

                            (Method::GET, Some((dir, file))) => {
                                let full_path = PathBuf::from(WWW_DIR).join(dir).join(file);
                                dbg!(full_path.clone().display());
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
                                dbg!(uri_path);

                                let full_path = PathBuf::from(WWW_DIR).join(uri_path);
                                dbg!(full_path.clone().display());
                                file_send(full_path).await
                            }

                            (Method::GET, Some((dir, file))) => {
                                let full_path = PathBuf::from(WWW_DIR).join(dir).join(file);
                                dbg!(full_path.clone().display());
                                file_send(full_path).await
                            }

                            _ => Ok(not_found()),
                        }
                    }
                };

                match t {
                    Ok(data) => Ok(data),
                    Err(err) => Ok(server_error(err)),
                }
            }
        }
        .boxed()
    });

    Ok(handler)
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

async fn file_send(path: impl AsRef<Path>) -> Result<Response<Body>, Error> {
    // Serve a file by asynchronously reading it by chunks using tokio-util crate.

    let path = path.as_ref();

    if let Ok(file) = File::open(path).await {
        let metadata = file.metadata().await.with_kind(ErrorKind::Filesystem)?;
        let _is_non_empty = match IsNonEmptyFile::new(&metadata, path) {
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
