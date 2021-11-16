use futures::future::BoxFuture;
use http::{Request, Response};
use hyper::service::Service;
use hyper::Body;
use rand::RngCore;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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
            if !(c >= '0' && c <= '9' || c >= 'A' && c <= 'V') {
                return None;
            }
        }
        Some(RequestGuid(r.to_owned()))
    }
}

pub struct RpcContinuation {
    expires_at: u32,
    handler: Box<
        dyn Service<
                Request<Body>,
                Response = Response<Body>,
                Error = crate::Error,
                Future = BoxFuture<'static, Result<Response<Body>, crate::Error>>,
            > + Send
            + Sync,
    >,
}

impl Service<Request<Body>> for RpcContinuation {
    type Response = Response<Body>;
    type Error = crate::Error;
    type Future = BoxFuture<'static, Result<Response<Body>, crate::Error>>;

    fn poll_ready(
        &mut self,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), Self::Error>> {
        self.handler.poll_ready(cx)
    }

    fn call(&mut self, req: Request<Body>) -> Self::Future {
        self.handler.call(req)
    }
}
