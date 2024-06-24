use std::convert::Infallible;
use std::net::SocketAddr;
use std::task::Poll;
use std::time::Duration;

use axum::extract::Request;
use axum::Router;
use axum_server::Handle;
use bytes::Bytes;
use futures::future::ready;
use futures::FutureExt;
use helpers::NonDetachingJoinHandle;
use tokio::sync::{oneshot, watch};

use crate::context::{DiagnosticContext, InitContext, InstallContext, RpcContext, SetupContext};
use crate::net::static_server::{
    diagnostic_ui_router, init_ui_router, install_ui_router, main_ui_router, refresher,
    setup_ui_router,
};
use crate::prelude::*;

#[derive(Clone)]
pub struct SwappableRouter(watch::Sender<Router>);
impl SwappableRouter {
    pub fn new(router: Router) -> Self {
        Self(watch::channel(router).0)
    }
    pub fn swap(&self, router: Router) {
        let _ = self.0.send_replace(router);
    }
}

#[derive(Clone)]
pub struct SwappableRouterService(watch::Receiver<Router>);
impl<B> tower_service::Service<Request<B>> for SwappableRouterService
where
    B: axum::body::HttpBody<Data = Bytes> + Send + 'static,
    B::Error: Into<axum::BoxError>,
{
    type Response = <Router as tower_service::Service<Request<B>>>::Response;
    type Error = <Router as tower_service::Service<Request<B>>>::Error;
    type Future = <Router as tower_service::Service<Request<B>>>::Future;
    #[inline]
    fn poll_ready(&mut self, cx: &mut std::task::Context<'_>) -> Poll<Result<(), Self::Error>> {
        let mut changed = self.0.changed().boxed();
        if changed.poll_unpin(cx).is_ready() {
            return Poll::Ready(Ok(()));
        }
        drop(changed);
        tower_service::Service::<Request<B>>::poll_ready(&mut self.0.borrow().clone(), cx)
    }
    fn call(&mut self, req: Request<B>) -> Self::Future {
        self.0.borrow().clone().call(req)
    }
}

impl<T> tower_service::Service<T> for SwappableRouter {
    type Response = SwappableRouterService;
    type Error = Infallible;
    type Future = futures::future::Ready<Result<Self::Response, Self::Error>>;
    #[inline]
    fn poll_ready(
        &mut self,
        _: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), Self::Error>> {
        Poll::Ready(Ok(()))
    }
    fn call(&mut self, _: T) -> Self::Future {
        ready(Ok(SwappableRouterService(self.0.subscribe())))
    }
}

pub struct WebServer {
    shutdown: oneshot::Sender<()>,
    router: SwappableRouter,
    thread: NonDetachingJoinHandle<()>,
}
impl WebServer {
    pub fn new(bind: SocketAddr) -> Self {
        let router = SwappableRouter::new(refresher());
        let thread_router = router.clone();
        let (shutdown, shutdown_recv) = oneshot::channel();
        let thread = NonDetachingJoinHandle::from(tokio::spawn(async move {
            let handle = Handle::new();
            let mut server = axum_server::bind(bind).handle(handle.clone());
            server.http_builder().http1().preserve_header_case(true);
            server.http_builder().http1().title_case_headers(true);

            if let (Err(e), _) = tokio::join!(server.serve(thread_router), async {
                let _ = shutdown_recv.await;
                handle.graceful_shutdown(Some(Duration::from_secs(0)));
            }) {
                tracing::error!("Spawning hyper server error: {}", e);
            }
        }));
        Self {
            shutdown,
            router,
            thread,
        }
    }

    pub async fn shutdown(self) {
        self.shutdown.send(()).unwrap_or_default();
        self.thread.await.unwrap()
    }

    pub fn serve_router(&mut self, router: Router) {
        self.router.swap(router)
    }

    pub fn serve_main(&mut self, ctx: RpcContext) {
        self.serve_router(main_ui_router(ctx))
    }

    pub fn serve_setup(&mut self, ctx: SetupContext) {
        self.serve_router(setup_ui_router(ctx))
    }

    pub fn serve_diagnostic(&mut self, ctx: DiagnosticContext) {
        self.serve_router(diagnostic_ui_router(ctx))
    }

    pub fn serve_install(&mut self, ctx: InstallContext) {
        self.serve_router(install_ui_router(ctx))
    }

    pub fn serve_init(&mut self, ctx: InitContext) {
        self.serve_router(init_ui_router(ctx))
    }
}
