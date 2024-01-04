use bytes::Bytes;
use http::HeaderValue;
use hyper::header::HeaderMap;
use rpc_toolkit::hyper::{Request, Response};
use rpc_toolkit::{BoxBody, Empty, Middleware};

#[derive(Clone)]
pub struct Cors {
    headers: HeaderMap,
}
impl Cors {
    pub fn new() -> Self {
        let mut headers = HeaderMap::new();
        headers.insert(
            "Access-Control-Allow-Credentials",
            HeaderValue::from_static("true"),
        );
        Self { headers }
    }
    fn get_cors_headers(&mut self, req: &Request<BoxBody>) {
        if let Some(origin) = req.headers().get("Origin") {
            self.headers
                .insert("Access-Control-Allow-Origin", origin.clone());
        } else {
            self.headers
                .insert("Access-Control-Allow-Origin", HeaderValue::from_static("*"));
        }
        if let Some(method) = req.headers().get("Access-Control-Request-Method") {
            self.headers
                .insert("Access-Control-Allow-Methods", method.clone());
        } else {
            self.headers.insert(
                "Access-Control-Allow-Methods",
                HeaderValue::from_static("*"),
            );
        }
        if let Some(headers) = req.headers().get("Access-Control-Request-Headers") {
            self.headers
                .insert("Access-Control-Allow-Headers", headers.clone());
        } else {
            self.headers.insert(
                "Access-Control-Allow-Headers",
                HeaderValue::from_static("*"),
            );
        }
    }
}
#[async_trait::async_trait]
impl<Context: Send + 'static> Middleware<Context> for Cors {
    type Metadata = Empty;
    async fn process_http_request(
        &mut self,
        context: &Context,
        request: &mut Request<BoxBody>,
    ) -> Result<(), Response<Bytes>> {
        self.get_cors_headers(request);
        Ok(())
    }
    async fn process_http_response(&mut self, context: &Context, response: &mut Response<Bytes>) {
        response
            .headers_mut()
            .extend(std::mem::take(&mut self.headers))
    }
}
