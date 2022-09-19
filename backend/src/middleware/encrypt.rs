use std::sync::Arc;

use aes::cipher::{CipherKey, NewCipher, Nonce, StreamCipher};
use aes::Aes256Ctr;
use futures::Stream;
use hmac::Hmac;
use josekit::jwk::Jwk;
use rpc_toolkit::hyper::{self, Body};
use serde::{Deserialize, Serialize};
use sha2::Sha256;

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
    let mut aes = Aes256Ctr::new(&aeskey, ctr);
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
    let mut aes = Aes256Ctr::new(&aeskey, ctr);
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
                    if this.ctr.len() < 16 && !buf.is_empty() {
                        let to_read = std::cmp::min(16 - this.ctr.len(), buf.len());
                        this.ctr.extend_from_slice(&buf[0..to_read]);
                        buf = &buf[to_read..];
                    }
                    if this.salt.len() < 16 && !buf.is_empty() {
                        let to_read = std::cmp::min(16 - this.salt.len(), buf.len());
                        this.salt.extend_from_slice(&buf[0..to_read]);
                        buf = &buf[to_read..];
                    }
                    if this.ctr.len() == 16 && this.salt.len() == 16 {
                        let aeskey = pbkdf2(this.key.as_bytes(), &this.salt);
                        let ctr = Nonce::<Aes256Ctr>::from_slice(this.ctr);
                        let mut aes = Aes256Ctr::new(&aeskey, ctr);
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
        let aes = Aes256Ctr::new(&aeskey, ctr);
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
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct EncryptedWire {
    encrypted: serde_json::Value,
}
impl EncryptedWire {
    pub fn decrypt(self, current_secret: impl AsRef<Jwk>) -> Option<String> {
        let current_secret = current_secret.as_ref();

        let decrypter = josekit::jwe::alg::ecdh_es::EcdhEsJweAlgorithm::EcdhEs
            .decrypter_from_jwk(current_secret)
            .unwrap();
        let encrypted = serde_json::to_string(&self.encrypted).ok()?;
        let (decoded, _) = josekit::jwe::deserialize_json(&encrypted, &decrypter).ok()?;
        String::from_utf8(decoded).ok()
    }
}

/// We created this test by first making the private key, then restoring from this private key for recreatability.
/// After this the frontend then encoded an password, then we are testing that the output that we got (hand coded)
/// will be the shape we want.
#[test]
fn test_gen_awk() {
    let private_key: Jwk = serde_json::from_str(
        r#"{
            "kty": "EC",
            "crv": "P-256",
            "d": "3P-MxbUJtEhdGGpBCRFXkUneGgdyz_DGZWfIAGSCHOU",
            "x": "yHTDYSfjU809fkSv9MmN4wuojf5c3cnD7ZDN13n-jz4",
            "y": "8Mpkn744A5KDag0DmX2YivB63srjbugYZzWc3JOpQXI"
          }"#,
    )
    .unwrap();
    let encrypted: EncryptedWire =  serde_json::from_str(r#"{
        "encrypted":     {        "protected": "eyJlbmMiOiJBMTI4Q0JDLUhTMjU2IiwiYWxnIjoiRUNESC1FUyIsImtpZCI6ImgtZnNXUVh2Tm95dmJEazM5dUNsQ0NUdWc5N3MyZnJockJnWUVBUWVtclUiLCJlcGsiOnsia3R5IjoiRUMiLCJjcnYiOiJQLTI1NiIsIngiOiJmRkF0LXNWYWU2aGNkdWZJeUlmVVdUd3ZvWExaTkdKRHZIWVhIckxwOXNNIiwieSI6IjFvVFN6b00teHlFZC1SLUlBaUFHdXgzS1dJZmNYZHRMQ0JHLUh6MVkzY2sifX0",        "iv": "NbwvfvWOdLpZfYRIZUrkcw",        "ciphertext": "Zc5Br5kYOlhPkIjQKOLMJw",        "tag": "EPoch52lDuCsbUUulzZGfg"    }
      }"#).unwrap();
    assert_eq!(
        "testing12345",
        &encrypted.decrypt(Arc::new(private_key)).unwrap()
    );
}
