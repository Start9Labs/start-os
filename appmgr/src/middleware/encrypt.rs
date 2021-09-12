use std::sync::Arc;

use aes::cipher::{CipherKey, NewCipher, Nonce, StreamCipher};
use aes::Aes256Ctr;
use anyhow::anyhow;
use futures::future::BoxFuture;
use futures::{FutureExt, Stream};
use hmac::Hmac;
use http::{HeaderMap, HeaderValue};
use pbkdf2::pbkdf2;
use rpc_toolkit::hyper::http::Error as HttpError;
use rpc_toolkit::hyper::{self, Body, Request, Response, StatusCode};
use rpc_toolkit::rpc_server_helpers::{
    to_response, DynMiddleware, DynMiddlewareStage2, DynMiddlewareStage3, DynMiddlewareStage4,
};
use rpc_toolkit::yajrc::RpcMethod;
use rpc_toolkit::Metadata;
use sha2::Sha256;

use crate::util::Apply;
use crate::Error;

#[pin_project::pin_project]
pub struct DecryptStream {
    key: Arc<String>,
    #[pin]
    body: Body,
    ctr: Vec<u8>,
    salt: Vec<u8>,
    aes: Option<Aes256Ctr>,
}
impl DecryptStream {
    pub fn new(key: Arc<String>, body: Body) -> Self {
        DecryptStream {
            key,
            body,
            ctr: Vec::new(),
            salt: Vec::new(),
            aes: None,
        }
    }
}
impl Stream for DecryptStream {
    type Item = hyper::Result<hyper::body::Bytes>;
    fn poll_next(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Self::Item>> {
        let this = self.project();
        match this.body.poll_next(cx) {
            std::task::Poll::Pending => std::task::Poll::Pending,
            std::task::Poll::Ready(Some(Ok(bytes))) => std::task::Poll::Ready(Some(Ok({
                let mut buf = &*bytes;
                if let Some(aes) = this.aes.as_mut() {
                    let mut res = buf.to_vec();
                    aes.apply_keystream(&mut res);
                    res.into()
                } else {
                    if this.ctr.len() < 16 && buf.len() > 0 {
                        let to_read = std::cmp::min(16 - this.ctr.len(), buf.len());
                        this.ctr.extend_from_slice(&buf[0..to_read]);
                        buf = &buf[to_read..];
                    }
                    if this.salt.len() < 16 && buf.len() > 0 {
                        let to_read = std::cmp::min(16 - this.salt.len(), buf.len());
                        this.salt.extend_from_slice(&buf[0..to_read]);
                        buf = &buf[to_read..];
                    }
                    if this.ctr.len() == 16 && this.salt.len() == 16 {
                        let mut aeskey = CipherKey::<Aes256Ctr>::default();
                        pbkdf2::<Hmac<Sha256>>(
                            this.key.as_bytes(),
                            &this.salt,
                            100_000,
                            aeskey.as_mut_slice(),
                        );
                        let ctr = Nonce::<Aes256Ctr>::from_slice(&this.ctr);
                        *this.aes = Some(Aes256Ctr::new(&aeskey, &ctr));
                        buf.to_vec().into()
                    } else {
                        hyper::body::Bytes::new()
                    }
                }
            }))),
            std::task::Poll::Ready(a) => std::task::Poll::Ready(a),
        }
    }
}

#[pin_project::pin_project]
pub struct EncryptStream {
    #[pin]
    body: Body,
    aes: Aes256Ctr,
    prefix: Option<[u8; 32]>,
}
impl EncryptStream {
    pub fn new(key: &str, body: Body) -> Self {
        let prefix: [u8; 32] = rand::random();
        let mut aeskey = CipherKey::<Aes256Ctr>::default();
        pbkdf2::<Hmac<Sha256>>(
            key.as_bytes(),
            &prefix[16..],
            100_000,
            aeskey.as_mut_slice(),
        );
        let ctr = Nonce::<Aes256Ctr>::from_slice(&prefix[..16]);
        let aes = Aes256Ctr::new(&aeskey, &ctr);
        EncryptStream {
            body,
            aes,
            prefix: Some(prefix),
        }
    }
}
impl Stream for EncryptStream {
    type Item = hyper::Result<hyper::body::Bytes>;
    fn poll_next(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Self::Item>> {
        let this = self.project();
        if let Some(prefix) = this.prefix.take() {
            std::task::Poll::Ready(Some(Ok(prefix.to_vec().into())))
        } else {
            match this.body.poll_next(cx) {
                std::task::Poll::Pending => std::task::Poll::Pending,
                std::task::Poll::Ready(Some(Ok(bytes))) => std::task::Poll::Ready(Some(Ok({
                    let mut res = bytes.to_vec();
                    this.aes.apply_keystream(&mut res);
                    res.into()
                }))),
                std::task::Poll::Ready(a) => std::task::Poll::Ready(a),
            }
        }
    }
}

fn encrypted(headers: &HeaderMap) -> bool {
    headers
        .get("Content-Encoding")
        .and_then(|h| {
            h.to_str()
                .ok()?
                .split(",")
                .any(|s| s == "aesctr256")
                .apply(Some)
        })
        .unwrap_or_default()
}

pub fn encrypt<M: Metadata>(key: Arc<String>) -> DynMiddleware<M> {
    Box::new(
        move |req: &mut Request<Body>,
              metadata: M|
              -> BoxFuture<Result<Result<DynMiddlewareStage2, Response<Body>>, HttpError>> {
            let key = key.clone();
            async move {
                let encrypted = encrypted(req.headers());
                if encrypted {
                    let body = std::mem::take(req.body_mut());
                    *req.body_mut() = Body::wrap_stream(DecryptStream::new(key.clone(), body));
                };
                let res: DynMiddlewareStage2 = Box::new(move |req, rpc_req| {
                    async move {
                        if !encrypted
                            && metadata
                                .get(&rpc_req.method.as_str(), "authenticated")
                                .unwrap_or(true)
                        {
                            let (res_parts, _) = Response::new(()).into_parts();
                            Ok(Err(to_response(
                                &req.headers,
                                res_parts,
                                Err(Error::new(
                                    anyhow!("Must be encrypted"),
                                    crate::ErrorKind::Authorization,
                                )
                                .into()),
                                |_| StatusCode::OK,
                            )?))
                        } else {
                            let res: DynMiddlewareStage3 = Box::new(move |_, _| {
                                async move {
                                    let res: DynMiddlewareStage4 = Box::new(move |res| {
                                        async move {
                                            if encrypted {
                                                res.headers_mut().insert(
                                                    "Content-Encoding",
                                                    HeaderValue::from_static("aesctr256"),
                                                );
                                                let body = std::mem::take(res.body_mut());
                                                *res.body_mut() = Body::wrap_stream(
                                                    EncryptStream::new(&*key, body),
                                                );
                                            }
                                            Ok(())
                                        }
                                        .boxed()
                                    });
                                    Ok(Ok(res))
                                }
                                .boxed()
                            });
                            Ok(Ok(res))
                        }
                    }
                    .boxed()
                });
                Ok(Ok(res))
            }
            .boxed()
        },
    )
}
