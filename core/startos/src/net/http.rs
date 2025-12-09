use std::net::IpAddr;
use std::sync::Arc;

use futures::future::Either;
use http::HeaderValue;
use hyper::service::service_fn;
use hyper_util::rt::{TokioExecutor, TokioIo, TokioTimer};
use tokio::sync::Mutex;

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

pub async fn run_http_proxy<F, T>(
    from: F,
    to: T,
    negotiated_alpn: Option<Vec<u8>>,
    src_ip: Option<IpAddr>,
) -> Result<(), Error>
where
    F: ReadWriter + Unpin + Send + 'static,
    T: ReadWriter + Unpin + Send + 'static,
{
    if negotiated_alpn
        .as_ref()
        .map(|alpn| alpn.as_slice() == b"h2")
        .unwrap_or(false)
    {
        run_http2_proxy(from, to, src_ip).await
    } else {
        run_http1_proxy(from, to, src_ip).await
    }
}

pub async fn run_http2_proxy<F, T>(from: F, to: T, src_ip: Option<IpAddr>) -> Result<(), Error>
where
    F: ReadWriter + Unpin + Send + 'static,
    T: ReadWriter + Unpin + Send + 'static,
{
    let (client, to) = hyper::client::conn::http2::Builder::new(TokioExecutor::new())
        .timer(TokioTimer::new())
        .handshake(TokioIo::new(to))
        .await?;
    let from = hyper::server::conn::http2::Builder::new(TokioExecutor::new())
        .timer(TokioTimer::new())
        .serve_connection(
            TokioIo::new(from),
            service_fn(|mut req| {
                let mut client = client.clone();
                async move {
                    req.headers_mut()
                        .insert("X-Forwarded-Proto", HeaderValue::from_static("https"));
                    if let Some(src_ip) = src_ip
                        .map(|s| s.to_string())
                        .as_deref()
                        .and_then(|s| HeaderValue::from_str(s).ok())
                    {
                        req.headers_mut().insert("X-Forwarded-For", src_ip);
                    }

                    client.send_request(req).await
                }
            }),
        );
    futures::future::try_select(from, to)
        .await
        .map_err(|e| match e {
            Either::Left((e, _)) => e,
            Either::Right((e, _)) => e,
        })?;

    Ok(())
}

pub async fn run_http1_proxy<F, T>(from: F, to: T, src_ip: Option<IpAddr>) -> Result<(), Error>
where
    F: ReadWriter + Unpin + Send + 'static,
    T: ReadWriter + Unpin + Send + 'static,
{
    let (client, to) = hyper::client::conn::http1::Builder::new()
        .title_case_headers(true)
        .preserve_header_case(true)
        .handshake(TokioIo::new(to))
        .await?;
    let client = Arc::new(Mutex::new(client));
    let from = hyper::server::conn::http1::Builder::new()
        .timer(TokioTimer::new())
        .serve_connection(
            TokioIo::new(from),
            service_fn(|mut req| {
                let client = client.clone();
                async move {
                    req.headers_mut()
                        .insert("X-Forwarded-Proto", HeaderValue::from_static("https"));
                    if let Some(src_ip) = src_ip
                        .map(|s| s.to_string())
                        .as_deref()
                        .and_then(|s| HeaderValue::from_str(s).ok())
                    {
                        req.headers_mut().insert("X-Forwarded-For", src_ip);
                    }

                    client.lock().await.send_request(req).await
                }
            }),
        );
    futures::future::try_select(from, to)
        .await
        .map_err(|e| match e {
            Either::Left((e, _)) => e,
            Either::Right((e, _)) => e,
        })?;

    Ok(())
}
