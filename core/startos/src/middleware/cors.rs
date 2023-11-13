use futures::FutureExt;
use http::HeaderValue;
use hyper::header::HeaderMap;
use rpc_toolkit::hyper::http::Error as HttpError;
use rpc_toolkit::hyper::{Body, Method, Request, Response};
use rpc_toolkit::rpc_server_helpers::{
    DynMiddlewareStage2, DynMiddlewareStage3, DynMiddlewareStage4,
};
use rpc_toolkit::Metadata;

fn get_cors_headers(req: &Request<Body>) -> HeaderMap {
    let mut res = HeaderMap::new();
    if let Some(origin) = req.headers().get("Origin") {
        res.insert("Access-Control-Allow-Origin", origin.clone());
    }
    if let Some(method) = req.headers().get("Access-Control-Request-Method") {
        res.insert("Access-Control-Allow-Methods", method.clone());
    }
    if let Some(headers) = req.headers().get("Access-Control-Request-Headers") {
        res.insert("Access-Control-Allow-Headers", headers.clone());
    }
    res.insert(
        "Access-Control-Allow-Credentials",
        HeaderValue::from_static("true"),
    );
    res
}

pub async fn cors<M: Metadata>(
    req: &mut Request<Body>,
    _metadata: M,
) -> Result<Result<DynMiddlewareStage2, Response<Body>>, HttpError> {
    let headers = get_cors_headers(req);
    if req.method() == Method::OPTIONS {
        Ok(Err({
            let mut res = Response::new(Body::empty());
            res.headers_mut().extend(headers.into_iter());
            res
        }))
    } else {
        Ok(Ok(Box::new(|_, _| {
            async move {
                let res: DynMiddlewareStage3 = Box::new(|_, _| {
                    async move {
                        let res: DynMiddlewareStage4 = Box::new(|res| {
                            async move {
                                res.headers_mut().extend(headers.into_iter());
                                Ok::<_, HttpError>(())
                            }
                            .boxed()
                        });
                        Ok::<_, HttpError>(Ok(res))
                    }
                    .boxed()
                });
                Ok::<_, HttpError>(Ok(res))
            }
            .boxed()
        })))
    }
}
