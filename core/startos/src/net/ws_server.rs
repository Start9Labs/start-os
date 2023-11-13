use crate::context::RpcContext;

pub async fn ws_server_handle(rpc_ctx: RpcContext) {

    let ws_ctx = rpc_ctx.clone();
    let ws_server_handle = {
        let builder = Server::bind(&ws_ctx.bind_ws);

        let make_svc = ::rpc_toolkit::hyper::service::make_service_fn(move |_| {
            let ctx = ws_ctx.clone();
            async move {
                Ok::<_, ::rpc_toolkit::hyper::Error>(::rpc_toolkit::hyper::service::service_fn(
                    move |req| {
                        let ctx = ctx.clone();
                        async move {
                            tracing::debug!("Request to {}", req.uri().path());
                            match req.uri().path() {
                                "/ws/db" => {
                                    Ok(subscribe(ctx, req).await.unwrap_or_else(err_to_500))
                                }
                                path if path.starts_with("/ws/rpc/") => {
                                     match RequestGuid::from(
                                        path.strip_prefix("/ws/rpc/").unwrap(),
                                    ) {
                                        None => {
                                            tracing::debug!("No Guid Path");
                                            Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(Body::empty())
                                        }
                                        Some(guid) => {
                                            match ctx.get_ws_continuation_handler(&guid).await {
                                                Some(cont) => match cont(req).await {
                                                    Ok(r) => Ok(r),
                                                    Err(e) => Response::builder()
                                                        .status(
                                                            StatusCode::INTERNAL_SERVER_ERROR,
                                                        )
                                                        .body(Body::from(format!("{}", e))),
                                                },
                                                _ => Response::builder()
                                                    .status(StatusCode::NOT_FOUND)
                                                    .body(Body::empty()),
                                            }
                                        }
                                    }
                                }
                                path if path.starts_with("/rest/rpc/") => {
                                    match RequestGuid::from(
                                        path.strip_prefix("/rest/rpc/").unwrap(),
                                    ) {
                                        None => {
                                            tracing::debug!("No Guid Path");
                                            Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(Body::empty())
                                        }
                                        Some(guid) => {
                                            match ctx.get_rest_continuation_handler(&guid).await
                                            {
                                                None => Response::builder()
                                                    .status(StatusCode::NOT_FOUND)
                                                    .body(Body::empty()),
                                                Some(cont) => match cont(req).await {
                                                    Ok(r) => Ok(r),
                                                    Err(e) => Response::builder()
                                                        .status(
                                                            StatusCode::INTERNAL_SERVER_ERROR,
                                                        )
                                                        .body(Body::from(format!("{}", e))),
                                                },
                                            }
                                        }
                                    }
                                }
                                _ => Response::builder()
                                    .status(StatusCode::NOT_FOUND)
                                    .body(Body::empty()),
                            }
                        }
                    },
                ))
            }
        });
        builder.serve(make_svc)
    }
    .with_graceful_shutdown({
        let mut shutdown = rpc_ctx.shutdown.subscribe();
        async move {
            shutdown.recv().await.expect("context dropped");
        }
    });

}