use basic_cookies::Cookie;
use color_eyre::eyre::eyre;
use digest::Digest;
use futures::future::BoxFuture;
use futures::{FutureExt, TryFutureExt};
use http::StatusCode;
use rpc_toolkit::command_helpers::prelude::RequestParts;
use rpc_toolkit::hyper::header::COOKIE;
use rpc_toolkit::hyper::http::Error as HttpError;
use rpc_toolkit::hyper::{Body, Request, Response};
use rpc_toolkit::rpc_server_helpers::{noop3, to_response, DynMiddleware, DynMiddlewareStage2};
use rpc_toolkit::yajrc::RpcMethod;
use rpc_toolkit::Metadata;
use sha2::Sha256;

use crate::context::RpcContext;
use crate::{Error, ResultExt};

pub fn get_id(req: &RequestParts) -> Result<String, Error> {
    if let Some(cookie_header) = req.headers.get(COOKIE) {
        let cookies = Cookie::parse(
            cookie_header
                .to_str()
                .with_kind(crate::ErrorKind::Authorization)?,
        )
        .with_kind(crate::ErrorKind::Authorization)?;
        if let Some(session) = cookies.iter().find(|c| c.get_name() == "session") {
            return Ok(hash_token(session.get_value()));
        }
    }
    Err(Error::new(
        eyre!("UNAUTHORIZED"),
        crate::ErrorKind::Authorization,
    ))
}

pub fn hash_token(token: &str) -> String {
    let mut hasher = Sha256::new();
    hasher.update(token.as_bytes());
    base32::encode(
        base32::Alphabet::RFC4648 { padding: false },
        hasher.finalize().as_slice(),
    )
    .to_lowercase()
}

pub async fn is_authed(ctx: &RpcContext, id: &str) -> Result<(), Error> {
    let session = sqlx::query!("UPDATE session SET last_active = CURRENT_TIMESTAMP WHERE id = ? AND logged_out IS NULL OR logged_out > CURRENT_TIMESTAMP", id)
        .execute(&mut ctx.secret_store.acquire().await?)
        .await?;
    if session.rows_affected() == 0 {
        return Err(Error::new(
            eyre!("UNAUTHORIZED"),
            crate::ErrorKind::Authorization,
        ));
    }
    Ok(())
}

pub fn auth<M: Metadata>(ctx: RpcContext) -> DynMiddleware<M> {
    Box::new(
        move |req: &mut Request<Body>,
              metadata: M|
              -> BoxFuture<Result<Result<DynMiddlewareStage2, Response<Body>>, HttpError>> {
            let ctx = ctx.clone();
            async move {
                let mut header_stub = Request::new(Body::empty());
                *header_stub.headers_mut() = req.headers().clone();
                let m2: DynMiddlewareStage2 = Box::new(move |req, rpc_req| {
                    async move {
                        if metadata
                            .get(rpc_req.method.as_str(), "authenticated")
                            .unwrap_or(true)
                        {
                            if let Err(e) = async { get_id(req) }
                                .and_then(|id| async move { is_authed(&ctx, &id).await })
                                .await
                            {
                                let (res_parts, _) = Response::new(()).into_parts();
                                return Ok(Err(to_response(
                                    &req.headers,
                                    res_parts,
                                    Err(e.into()),
                                    |_| StatusCode::OK,
                                )?));
                            }
                        }
                        Ok(Ok(noop3()))
                    }
                    .boxed()
                });
                Ok(Ok(m2))
            }
            .boxed()
        },
    )
}
