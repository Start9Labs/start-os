use futures::FutureExt;
use rpc_toolkit::hyper::http::Error as HttpError;
use rpc_toolkit::hyper::{Body, Request, Response};
use rpc_toolkit::rpc_server_helpers::{noop4, DynMiddlewareStage2, DynMiddlewareStage3};
use rpc_toolkit::yajrc::RpcMethod;
use rpc_toolkit::Metadata;

use crate::Error;

pub async fn diagnostic<M: Metadata>(
    _req: &mut Request<Body>,
    _metadata: M,
) -> Result<Result<DynMiddlewareStage2, Response<Body>>, HttpError> {
    Ok(Ok(Box::new(|_, rpc_req| {
        let method = rpc_req.method.as_str().to_owned();
        async move {
            let res: DynMiddlewareStage3 = Box::new(|_, rpc_res| {
                async move {
                    if let Err(e) = rpc_res {
                        if e.code == -32601 {
                            *e = Error::new(
                                color_eyre::eyre::eyre!(
                                    "{} is not available on the Diagnostic API",
                                    method
                                ),
                                crate::ErrorKind::DiagnosticMode,
                            )
                            .into();
                        }
                    }
                    Ok(Ok(noop4()))
                }
                .boxed()
            });
            Ok::<_, HttpError>(Ok(res))
        }
        .boxed()
    })))
}
