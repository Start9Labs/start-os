use crate::context::RpcContext;
use crate::install::PKG_PUBLIC_DIR;
use crate::middleware::auth::ValidSession;
use crate::{Error, ErrorKind, ResultExt};
use digest::Digest;
use http::response::Builder;
use hyper::service::{make_service_fn, service_fn};
use hyper::{Body, Error as HyperError, Method, Request, Response, Server, StatusCode};
use std::future::Future;
use std::path::PathBuf;
use std::time::{SystemTime, UNIX_EPOCH};
use tokio::fs::File;
use tokio_util::codec::{BytesCodec, FramedRead};

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
    println!("Request = {:?}", req);
    let (request_parts, _body) = req.into_parts();
    let valid_session = ValidSession::from_request_parts(&request_parts, &ctx).await;
    match (
        valid_session,
        request_parts.method,
        request_parts.uri.path().strip_prefix("/"),
    ) {
        (Err(error), _, _) => {
            tracing::warn!(
                "unauthorized for {:?} @{:?}",
                error,
                request_parts.uri.path()
            );
            return Ok(Response::builder()
                .status(StatusCode::UNAUTHORIZED)
                .body(NOT_AUTHORIZED.into())
                .unwrap());
        }
        (Ok(valid_session), Method::GET, Some(path)) => {
            file_send(valid_session, &ctx, PathBuf::from(path)).await
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
    _valid_session: ValidSession,
    ctx: &RpcContext,
    filename: PathBuf,
) -> Result<Response<Body>, Error> {
    // Serve a file by asynchronously reading it by chunks using tokio-util crate.
    let path = ctx.datadir.join(PKG_PUBLIC_DIR).join(filename);

    if let Ok(file) = File::open(path.clone()).await {
        let mut builder = Response::builder().status(StatusCode::OK);

        let metadata = file.metadata().await.with_kind(ErrorKind::Filesystem)?;
        let length = metadata.len();
        if !metadata.is_file() || length == 0 {
            return Ok(not_found());
        }
        let modified = metadata.modified().with_kind(ErrorKind::Filesystem)?;
        builder = with_e_tag(path, modified, builder);
        let stream = FramedRead::new(file, BytesCodec::new());
        let body = Body::wrap_stream(stream);
        return Ok(builder.body(body).with_kind(ErrorKind::Network)?);
    }

    Ok(not_found())
}

fn with_e_tag(path: PathBuf, modified: SystemTime, builder: Builder) -> Builder {
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
    builder.header(
        "ETag",
        base32::encode(base32::Alphabet::RFC4648 { padding: false }, res.as_slice()).to_lowercase(),
    )
}
