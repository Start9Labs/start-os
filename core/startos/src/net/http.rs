use axum::extract::Request;
use hyper_util::rt::TokioIo;

use crate::net::static_server::server_error;
use crate::prelude::*;
use crate::util::io::ReadWriter;

pub async fn handle_http_on_https(stream: impl ReadWriter + Unpin + 'static) -> Result<(), Error> {
    use axum::body::Body;
    use axum::extract::Request;
    use axum::response::Response;
    use http::Uri;

    use crate::net::static_server::server_error;

    hyper_util::server::conn::auto::Builder::new(hyper_util::rt::TokioExecutor::new())
        .serve_connection(
            hyper_util::rt::TokioIo::new(stream),
            hyper_util::service::TowerToHyperService::new(axum::Router::new().fallback(
                axum::routing::method_routing::any(move |req: Request| async move {
                    match async move {
                        let host = req
                            .headers()
                            .get(http::header::HOST)
                            .and_then(|host| host.to_str().ok());
                        if let Some(host) = host {
                            let uri = Uri::from_parts({
                                let mut parts = req.uri().to_owned().into_parts();
                                parts.scheme = Some("https".parse()?);
                                parts.authority = Some(host.parse()?);
                                parts
                            })?;
                            Response::builder()
                                .status(http::StatusCode::TEMPORARY_REDIRECT)
                                .header(http::header::LOCATION, uri.to_string())
                                .body(Body::default())
                        } else {
                            Response::builder()
                                .status(http::StatusCode::BAD_REQUEST)
                                .body(Body::from("Host header required"))
                        }
                    }
                    .await
                    {
                        Ok(a) => a,
                        Err(e) => {
                            tracing::warn!("Error redirecting http request on ssl port: {e}");
                            tracing::error!("{e:?}");
                            server_error(Error::new(e, ErrorKind::Network))
                        }
                    }
                }),
            )),
        )
        .await
        .map_err(|e| Error::new(color_eyre::eyre::Report::msg(e), ErrorKind::Network))
}

pub async fn run_http_proxy_bidirectional<C, S>(client: C, server: S) -> Result<(), Error>
where
    C: ReadWriter + Unpin + Send + 'static,
    S: ReadWriter + Unpin + Send + 'static,
{
    let client_io = TokioIo::new(client);
    let server_io = TokioIo::new(server);

    hyper_util::server::conn::auto::Builder::new(hyper_util::rt::TokioExecutor::new())
        .title_case_headers(true)
        .preserve_header_case(true)
        .serve_connection_with_upgrades(
            client_io,
            hyper_util::service::TowerToHyperService::new(axum::Router::new().fallback(
                axum::routing::method_routing::any(move |req: Request| async move {
                    match async move { Err(eyre!("todo")) }.await {
                        Ok(a) => a,
                        Err(e) => {
                            tracing::warn!("Error serving http proxy: {e}");
                            tracing::error!("{e:?}");
                            server_error(Error::new(e, ErrorKind::Network))
                        }
                    }
                }),
            )),
        )
        .await
        .map_err(|e| Error::new(color_eyre::eyre::Report::msg(e), ErrorKind::Network))
}
