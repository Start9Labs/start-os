use std::net::SocketAddr;
use std::time::Duration;

use axum::Router;
use axum_server::Handle;
use helpers::NonDetachingJoinHandle;
use tokio::sync::oneshot;

use crate::context::{DiagnosticContext, InstallContext, RpcContext, SetupContext};
use crate::net::static_server::{
    diag_ui_file_router, install_ui_file_router, main_ui_server_router, setup_ui_file_router,
};
use crate::Error;

pub struct WebServer {
    shutdown: oneshot::Sender<()>,
    thread: NonDetachingJoinHandle<()>,
}
impl WebServer {
    pub fn new(bind: SocketAddr, router: Router) -> Self {
        let (shutdown, shutdown_recv) = oneshot::channel();
        let thread = NonDetachingJoinHandle::from(tokio::spawn(async move {
            let handle = Handle::new();
            let mut server = axum_server::bind(bind).handle(handle.clone());
            server.http_builder().http1().preserve_header_case(true);
            server.http_builder().http1().title_case_headers(true);

            if let (Err(e), _) = tokio::join!(server.serve(router.into_make_service()), async {
                shutdown_recv.await;
                handle.graceful_shutdown(Some(Duration::from_secs(0)));
            }) {
                tracing::error!("Spawning hyper server error: {}", e);
            }
        }));
        Self { shutdown, thread }
    }

    pub async fn shutdown(self) {
        self.shutdown.send(()).unwrap_or_default();
        self.thread.await.unwrap()
    }

    pub fn main(bind: SocketAddr, ctx: RpcContext) -> Result<Self, Error> {
        Ok(Self::new(bind, main_ui_server_router(ctx)))
    }

    pub fn setup(bind: SocketAddr, ctx: SetupContext) -> Result<Self, Error> {
        Ok(Self::new(bind, setup_ui_file_router(ctx)))
    }

    pub fn diagnostic(bind: SocketAddr, ctx: DiagnosticContext) -> Result<Self, Error> {
        Ok(Self::new(bind, diag_ui_file_router(ctx)))
    }

    pub fn install(bind: SocketAddr, ctx: InstallContext) -> Result<Self, Error> {
        Ok(Self::new(bind, install_ui_file_router(ctx)))
    }
}
