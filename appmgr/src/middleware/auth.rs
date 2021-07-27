use anyhow::anyhow;
use basic_cookies::Cookie;
use chrono::Utc;
use futures::future::BoxFuture;
use futures::FutureExt;
use rpc_toolkit::hyper::header::COOKIE;
use rpc_toolkit::hyper::http::Error as HttpError;
use rpc_toolkit::hyper::{Body, Request, Response, StatusCode};
use rpc_toolkit::rpc_server_helpers::{
    noop2, noop3, DynMiddleware, DynMiddlewareStage2, DynMiddlewareStage3,
};
use rpc_toolkit::yajrc::RpcMethod;
use rpc_toolkit::Metadata;
use serde::Deserialize;

use crate::context::RpcContext;
use crate::{Error, ResultExt};
async fn is_authed(ctx: &RpcContext, req: &Request<Body>) -> Result<bool, Error> {
    if let Some(cookie_header) = req.headers().get(COOKIE) {
        let cookies = Cookie::parse(
            cookie_header
                .to_str()
                .with_kind(crate::ErrorKind::Authorization)?,
        )
        .with_kind(crate::ErrorKind::Authorization)?;
        if let Some(session) = cookies.iter().find(|c| c.get_name() == "session") {
            let id = session.get_value();
            let exp = sqlx::query!("SELECT expires_at FROM session WHERE id = ?", id)
                .fetch_one(&mut ctx.secret_store.acquire().await?)
                .await?;
            if exp.expires_at < Utc::now().naive_utc() {
                return Ok(true);
            }
        }
    }
    Ok(false)
}

pub async fn auth<Params: for<'de> Deserialize<'de> + 'static, M: Metadata>(
    ctx: RpcContext,
) -> DynMiddleware<Params, M> {
    Box::new(
        |req: &mut Request<Body>,
         metadata: M|
         -> BoxFuture<
            Result<Result<DynMiddlewareStage2<Params>, Response<Body>>, HttpError>,
        > {
            async move {
                match is_authed(&ctx, req).await {
                    Ok(true) => Ok(Ok(noop2())),
                    Ok(false) => Ok(Ok({
                        let mut fake_req = Request::new(Body::empty());
                        *fake_req.headers_mut() = req.headers().clone();
                        let m2: DynMiddlewareStage2<Params> =
                            Box::new(move |rpc_req| {
                                let method = rpc_req.method.as_str();
                                let res: Result<
                                    Result<DynMiddlewareStage3, Response<Body>>,
                                    HttpError,
                                > = if metadata.get(method, "login").unwrap_or(false) {
                                    todo!("set cookie on success")
                                } else if !metadata.get(method, "authenticated").unwrap_or(true) {
                                    Ok(Ok(noop3()))
                                } else {
                                    rpc_toolkit::rpc_server_helpers::to_response(
                                        &fake_req,
                                        Ok((
                                            rpc_req.id.clone(),
                                            Err(Error::new(
                                                anyhow!("UNAUTHORIZED"),
                                                crate::ErrorKind::Authorization,
                                            )
                                            .into()),
                                        )),
                                        |_| StatusCode::OK,
                                    )
                                    .map(|a| Err(a))
                                };
                                async { res }.boxed()
                            });
                        m2
                    })),
                    Err(e) => Ok(Ok({
                        let mut fake_req = Request::new(Body::empty());
                        *fake_req.headers_mut() = req.headers().clone();
                        let m2: DynMiddlewareStage2<Params> = Box::new(move |rpc_req| {
                            let res: Result<
                                Result<DynMiddlewareStage3, Response<Body>>,
                                HttpError,
                            > = rpc_toolkit::rpc_server_helpers::to_response(
                                &fake_req,
                                Ok((rpc_req.id.clone(), Err(e.into()))),
                                |_| StatusCode::OK,
                            )
                            .map(|a| Err(a));
                            async { res }.boxed()
                        });
                        m2
                    })),
                }
            }
            .boxed()
        },
    )
}
