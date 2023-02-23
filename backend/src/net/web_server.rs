use std::convert::Infallible;
use std::net::SocketAddr;

use futures::future::ready;
use futures::FutureExt;
use helpers::NonDetachingJoinHandle;
use hyper::service::{make_service_fn, service_fn};
use hyper::Server;
use tokio::sync::oneshot;

use crate::context::{DiagnosticContext, InstallContext, RpcContext, SetupContext};
use crate::net::static_server::{
    diag_ui_file_router, install_ui_file_router, main_ui_server_router, setup_ui_file_router,
};
use crate::net::HttpHandler;
use crate::prelude::*;

pub struct WebServer {
    shutdown: oneshot::Sender<()>,
    thread: NonDetachingJoinHandle<()>,
}
impl WebServer {
    pub fn new(bind: SocketAddr, router: HttpHandler) -> Self {
        let (shutdown, shutdown_recv) = oneshot::channel();
        let thread = NonDetachingJoinHandle::from(tokio::spawn(async move {
            let server = Server::bind(&bind)
                .http1_preserve_header_case(true)
                .http1_title_case_headers(true)
                .serve(make_service_fn(move |_| {
                    let router = router.clone();
                    ready(Ok::<_, Infallible>(service_fn(move |req| router(req))))
                }))
                .with_graceful_shutdown(shutdown_recv.map(|_| ()));
            if let Err(e) = server.await {
                tracing::error!("Spawning hyper server error: {}", e);
            }
        }));
        Self { shutdown, thread }
    }

    pub async fn shutdown(self) {
        self.shutdown.send(()).unwrap_or_default();
        self.thread.await.unwrap()
    }

    pub async fn main(bind: SocketAddr, ctx: RpcContext) -> Result<Self, Error> {
        Ok(Self::new(bind, main_ui_server_router(ctx).await?))
    }

    pub async fn setup(bind: SocketAddr, ctx: SetupContext) -> Result<Self, Error> {
        Ok(Self::new(bind, setup_ui_file_router(ctx).await?))
    }

    pub async fn diagnostic(bind: SocketAddr, ctx: DiagnosticContext) -> Result<Self, Error> {
        Ok(Self::new(bind, diag_ui_file_router(ctx).await?))
    }

    pub async fn install(bind: SocketAddr, ctx: InstallContext) -> Result<Self, Error> {
        Ok(Self::new(bind, install_ui_file_router(ctx).await?))
    }
}
