use std::fs::Metadata;
use std::future::Future;
use std::path::{Path, PathBuf};
use std::time::UNIX_EPOCH;

use digest::Digest;
use http::response::Builder;
use hyper::service::{make_service_fn, service_fn};
use hyper::{Body, Error as HyperError, Method, Request, Response, Server, StatusCode};
use tokio::fs::File;
use tokio_util::codec::{BytesCodec, FramedRead};

use crate::context::RpcContext;
use crate::install::PKG_PUBLIC_DIR;
use crate::middleware::auth::HasValidSession;
use crate::{Error, ErrorKind, ResultExt};

static NOT_FOUND: &[u8] = b"Not Found";
static NOT_AUTHORIZED: &[u8] = b"Not Authorized";

pub fn init(
    ctx: RpcContext,
    shutdown: impl Future<Output = ()> + Send + 'static,
) -> impl Future<Output = Result<(), HyperError>> {
    let addr = ctx.bind_static;

    let make_service = make_service_fn(move |_| {
        let ctx = ctx.clone();
        async move {
            Ok::<_, HyperError>(service_fn(move |req| {
                let ctx = ctx.clone();
                async move {
                    match file_server_router(req, ctx).await {
                        Ok(x) => Ok::<_, HyperError>(x),
                        Err(err) => {
                            tracing::error!("{:?}", err);
                            Ok(server_error())
                        }
                    }
                }
            }))
        }
    });

    Server::bind(&addr)
        .serve(make_service)
        .with_graceful_shutdown(shutdown)
}

async fn file_server_router(req: Request<Body>, ctx: RpcContext) -> Result<Response<Body>, Error> {
    let (request_parts, _body) = req.into_parts();
    let valid_session = HasValidSession::from_request_parts(&request_parts, &ctx).await;
    match (
        valid_session,
        request_parts.method,
        request_parts
            .uri
            .path()
            .strip_prefix("/")
            .unwrap_or(request_parts.uri.path())
            .split_once("/"),
    ) {
        (Err(error), _, _) => {
            tracing::warn!("unauthorized for {} @{:?}", error, request_parts.uri.path());
            tracing::debug!("{:?}", error);
            return Ok(Response::builder()
                .status(StatusCode::UNAUTHORIZED)
                .body(NOT_AUTHORIZED.into())
                .unwrap());
        }
        (Ok(valid_session), Method::GET, Some(("package-data", path))) => {
            file_send(
                valid_session,
                &ctx,
                ctx.datadir.join(PKG_PUBLIC_DIR).join(path),
            )
            .await
        }
        (Ok(valid_session), Method::GET, Some(("eos", "local.crt"))) => {
            file_send(
                valid_session,
                &ctx,
                PathBuf::from(crate::net::ssl::ROOT_CA_STATIC_PATH),
            )
            .await
        }
        _ => Ok(not_found()),
    }
}

/// HTTP status code 404
fn not_found() -> Response<Body> {
    Response::builder()
        .status(StatusCode::NOT_FOUND)
        .body(NOT_FOUND.into())
        .unwrap()
}

/// HTTP status code 500
fn server_error() -> Response<Body> {
    Response::builder()
        .status(StatusCode::INTERNAL_SERVER_ERROR)
        .body("".into())
        .unwrap()
}
async fn file_send(
    _valid_session: HasValidSession,
    _ctx: &RpcContext,
    path: impl AsRef<Path>,
) -> Result<Response<Body>, Error> {
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
        return Ok(builder.body(body).with_kind(ErrorKind::Network)?);
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
            None | Some(_) => "text/plain",
        },
        None => "text/plain",
    };
    builder.header("Content-Type", content_type)
}
fn with_content_length(metadata: &Metadata, builder: Builder) -> Builder {
    builder.header("Content-Length", metadata.len())
}
