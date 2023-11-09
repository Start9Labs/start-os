use futures::future::BoxFuture;
use futures::FutureExt;
use http::HeaderValue;
use rpc_toolkit::hyper::http::Error as HttpError;
use rpc_toolkit::hyper::{Body, Request, Response};
use rpc_toolkit::rpc_server_helpers::{
    noop4, DynMiddleware, DynMiddlewareStage2, DynMiddlewareStage3,
};
use rpc_toolkit::yajrc::RpcMethod;
use rpc_toolkit::Metadata;

use crate::context::RpcContext;

pub fn db<M: Metadata>(ctx: RpcContext) -> DynMiddleware<M> {
    Box::new(
        move |_: &mut Request<Body>,
              metadata: M|
              -> BoxFuture<Result<Result<DynMiddlewareStage2, Response<Body>>, HttpError>> {
            let ctx = ctx.clone();
            async move {
                let m2: DynMiddlewareStage2 = Box::new(move |_req, rpc_req| {
                    async move {
                        let sync_db = metadata
                            .get(rpc_req.method.as_str(), "sync_db")
                            .unwrap_or(false);

                        let m3: DynMiddlewareStage3 = Box::new(move |res, _| {
                            async move {
                                if sync_db {
                                    res.headers.append(
                                        "X-Patch-Sequence",
                                        HeaderValue::from_str(
                                            &ctx.db.sequence().await.to_string(),
                                        )?,
                                    );
                                }
                                Ok(Ok(noop4()))
                            }
                            .boxed()
                        });
                        Ok(Ok(m3))
                    }
                    .boxed()
                });
                Ok(Ok(m2))
            }
            .boxed()
        },
    )
}
