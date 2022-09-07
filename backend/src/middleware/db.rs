use color_eyre::eyre::eyre;
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
use crate::{Error, ResultExt};

pub fn db<M: Metadata>(ctx: RpcContext) -> DynMiddleware<M> {
    Box::new(
        move |_: &mut Request<Body>,
              metadata: M|
              -> BoxFuture<Result<Result<DynMiddlewareStage2, Response<Body>>, HttpError>> {
            let ctx = ctx.clone();
            async move {
                let m2: DynMiddlewareStage2 = Box::new(move |req, rpc_req| {
                    async move {
                        let seq = req.headers.remove("x-patch-sequence");
                        let sync_db = metadata
                            .get(rpc_req.method.as_str(), "sync_db")
                            .unwrap_or(false);

                        let m3: DynMiddlewareStage3 = Box::new(move |res, _| {
                            async move {
                                if sync_db && seq.is_some() {
                                    match async {
                                        let seq = seq
                                            .ok_or_else(|| {
                                                Error::new(
                                                    eyre!("Missing X-Patch-Sequence"),
                                                    crate::ErrorKind::InvalidRequest,
                                                )
                                            })?
                                            .to_str()
                                            .with_kind(crate::ErrorKind::InvalidRequest)?
                                            .parse()?;
                                        let res = ctx.db.sync(seq).await?;
                                        let json = match res {
                                            Ok(revs) => serde_json::to_vec(&revs),
                                            Err(dump) => serde_json::to_vec(&[dump]),
                                        }
                                        .with_kind(crate::ErrorKind::Serialization)?;
                                        Ok::<_, Error>(
                                            url::form_urlencoded::byte_serialize(&json)
                                                .collect::<String>(),
                                        )
                                    }
                                    .await
                                    {
                                        Ok(a) => res
                                            .headers
                                            .append("X-Patch-Updates", HeaderValue::from_str(&a)?),
                                        Err(e) => res.headers.append(
                                            "X-Patch-Error",
                                            HeaderValue::from_str(
                                                &url::form_urlencoded::byte_serialize(
                                                    e.to_string().as_bytes(),
                                                )
                                                .collect::<String>(),
                                            )?,
                                        ),
                                    };
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
