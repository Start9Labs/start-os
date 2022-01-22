use std::time::Instant;

use futures::future::BoxFuture;
use http::{Request, Response};
use hyper::Body;
use rand::RngCore;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize)]
pub struct RequestGuid<T: AsRef<str> = String>(T);
impl RequestGuid {
    pub fn new() -> Self {
        let mut buf = [0; 40];
        rand::thread_rng().fill_bytes(&mut buf);
        RequestGuid(base32::encode(
            base32::Alphabet::RFC4648 { padding: false },
            &buf,
        ))
    }

    pub fn from(r: &str) -> Option<RequestGuid> {
        if r.len() != 64 {
            return None;
        }
        for c in r.chars() {
            if !(c >= 'A' && c <= 'Z' || c >= '2' && c <= '7') {
                return None;
            }
        }
        Some(RequestGuid(r.to_owned()))
    }
}
#[test]
fn parse_guid() {
    println!(
        "{:?}",
        RequestGuid::from(&format!("{}", RequestGuid::new()))
    )
}

impl<T: AsRef<str>> std::fmt::Display for RequestGuid<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.as_ref().fmt(f)
    }
}

pub struct RpcContinuation {
    pub created_at: Instant,
    pub handler: Box<
        dyn FnOnce(Request<Body>) -> BoxFuture<'static, Result<Response<Body>, crate::Error>>
            + Send
            + Sync,
    >,
}
