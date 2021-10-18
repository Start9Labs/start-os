use std::future::Future;
use std::sync::Arc;

use aes::cipher::{CipherKey, NewCipher, Nonce, StreamCipher};
use aes::Aes256Ctr;
use color_eyre::eyre::eyre;
use futures::future::BoxFuture;
use futures::{FutureExt, Stream};
use hmac::Hmac;
use http::{HeaderMap, HeaderValue};
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

pub fn pbkdf2(password: impl AsRef<[u8]>, salt: impl AsRef<[u8]>) -> CipherKey<Aes256Ctr> {
    let mut aeskey = CipherKey::<Aes256Ctr>::default();
    pbkdf2::pbkdf2::<Hmac<Sha256>>(
        password.as_ref(),
        salt.as_ref(),
        1000,
        aeskey.as_mut_slice(),
    );
    aeskey
}

pub fn encrypt_slice(input: impl AsRef<[u8]>, password: impl AsRef<[u8]>) -> Vec<u8> {
    let prefix: [u8; 32] = rand::random();
    let aeskey = pbkdf2(password.as_ref(), &prefix[16..]);
    let ctr = Nonce::<Aes256Ctr>::from_slice(&prefix[..16]);
    let mut aes = Aes256Ctr::new(&aeskey, &ctr);
    let mut res = Vec::with_capacity(32 + input.as_ref().len());
    res.extend_from_slice(&prefix[..]);
    res.extend_from_slice(input.as_ref());
    aes.apply_keystream(&mut res[32..]);
    res
}

pub fn decrypt_slice(input: impl AsRef<[u8]>, password: impl AsRef<[u8]>) -> Vec<u8> {
    if input.as_ref().len() < 32 {
        return Vec::new();
    }
    let (prefix, rest) = input.as_ref().split_at(32);
    let aeskey = pbkdf2(password.as_ref(), &prefix[16..]);
    let ctr = Nonce::<Aes256Ctr>::from_slice(&prefix[..16]);
    let mut aes = Aes256Ctr::new(&aeskey, &ctr);
    let mut res = rest.to_vec();
    aes.apply_keystream(&mut res);
    res
}

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
                        let aeskey = pbkdf2(this.key.as_bytes(), &this.salt);
                        let ctr = Nonce::<Aes256Ctr>::from_slice(&this.ctr);
                        let mut aes = Aes256Ctr::new(&aeskey, &ctr);
                        let mut res = buf.to_vec();
                        aes.apply_keystream(&mut res);
                        *this.aes = Some(aes);
                        res.into()
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
        let aeskey = pbkdf2(key.as_bytes(), &prefix[16..]);
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

pub fn encrypt<
    F: Fn() -> Fut + Send + Sync + Clone + 'static,
    Fut: Future<Output = Result<Arc<String>, Error>> + Send + Sync + 'static,
    M: Metadata,
>(
    keysource: F,
) -> DynMiddleware<M> {
    Box::new(
        move |req: &mut Request<Body>,
              metadata: M|
              -> BoxFuture<Result<Result<DynMiddlewareStage2, Response<Body>>, HttpError>> {
            let keysource = keysource.clone();
            async move {
                let encrypted = encrypted(req.headers());
                let key = if encrypted {
                    let key = match keysource().await {
                        Ok(s) => s,
                        Err(e) => {
                            let (res_parts, _) = Response::new(()).into_parts();
                            return Ok(Err(to_response(
                                req.headers(),
                                res_parts,
                                Err(e.into()),
                                |_| StatusCode::OK,
                            )?));
                        }
                    };
                    let body = std::mem::take(req.body_mut());
                    *req.body_mut() = Body::wrap_stream(DecryptStream::new(key.clone(), body));
                    Some(key)
                } else {
                    None
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
                                    eyre!("Must be encrypted"),
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
                                            if let Some(key) = key {
                                                res.headers_mut().insert(
                                                    "Content-Encoding",
                                                    HeaderValue::from_static("aesctr256"),
                                                );
                                                if let Some(len_header) =
                                                    res.headers_mut().get_mut("Content-Length")
                                                {
                                                    if let Some(len) = len_header
                                                        .to_str()
                                                        .ok()
                                                        .and_then(|l| l.parse::<u64>().ok())
                                                    {
                                                        *len_header = HeaderValue::from(len + 32);
                                                    }
                                                }
                                                let body = std::mem::take(res.body_mut());
                                                *res.body_mut() = Body::wrap_stream(
                                                    EncryptStream::new(key.as_ref(), body),
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
