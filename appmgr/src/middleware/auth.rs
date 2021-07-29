use anyhow::anyhow;
use basic_cookies::Cookie;
use chrono::Utc;
use digest::Digest;
use futures::future::BoxFuture;
use futures::FutureExt;
use rpc_toolkit::command_helpers::prelude::RequestParts;
use rpc_toolkit::hyper::header::COOKIE;
use rpc_toolkit::hyper::http::Error as HttpError;
use rpc_toolkit::hyper::{Body, Request, Response};
use rpc_toolkit::rpc_server_helpers::{
    noop3, noop4, DynMiddleware, DynMiddlewareStage2, DynMiddlewareStage3,
};
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
        anyhow!("UNAUTHORIZED"),
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

async fn is_authed(ctx: &RpcContext, req: &RequestParts) -> Result<(), Error> {
    let id = get_id(req)?;
    let exp = sqlx::query!("SELECT logged_out FROM session WHERE id = ?", id)
        .fetch_one(&mut ctx.secret_store.acquire().await?)
        .await?;
    match exp.logged_out {
        Some(exp) if exp >= Utc::now().naive_utc() => Err(Error::new(
            anyhow!("UNAUTHORIZED"),
            crate::ErrorKind::Authorization,
        )),
        _ => Ok(()),
    }
}

pub async fn auth<M: Metadata>(ctx: RpcContext) -> DynMiddleware<M> {
    Box::new(
        |req: &mut Request<Body>,
         metadata: M|
         -> BoxFuture<Result<Result<DynMiddlewareStage2, Response<Body>>, HttpError>> {
            async move {
                let mut header_stub = Request::new(Body::empty());
                *header_stub.headers_mut() = req.headers().clone();
                let m2: DynMiddlewareStage2 = Box::new(move |req, rpc_req| {
                    async move {
                        if metadata
                            .get(rpc_req.method.as_str(), "authenticated")
                            .unwrap_or(true)
                        {
                            if let Err(e) = is_authed(&ctx, req).await {
                                let m3: DynMiddlewareStage3 = Box::new(|_, rpc_res| {
                                    async move {
                                        *rpc_res = Err(e.into());
                                        Ok(Ok(noop4()))
                                    }
                                    .boxed()
                                });
                                return Ok(Ok(m3));
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
