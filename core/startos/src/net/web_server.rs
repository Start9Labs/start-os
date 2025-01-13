use std::future::Future;
use std::net::SocketAddr;
use std::ops::Deref;
use std::sync::atomic::AtomicBool;
use std::sync::{Arc, RwLock};
use std::task::Poll;
use std::time::Duration;

use axum::Router;
use futures::future::{BoxFuture, Either};
use futures::FutureExt;
use helpers::NonDetachingJoinHandle;
use hyper_util::rt::{TokioIo, TokioTimer};
use tokio::net::{TcpListener, TcpStream};
use tokio::sync::{oneshot, watch};

use crate::context::{DiagnosticContext, InitContext, InstallContext, RpcContext, SetupContext};
use crate::net::network_interface::NetworkInterfaceListener;
use crate::net::static_server::{
    diagnostic_ui_router, init_ui_router, install_ui_router, main_ui_router, redirecter, refresher,
    setup_ui_router,
};
use crate::prelude::*;
use crate::util::actor::background::BackgroundJobQueue;

pub struct Accepted {
    pub https_redirect: bool,
    pub stream: TcpStream,
}

pub trait Accept {
    fn poll_accept(&mut self, cx: &mut std::task::Context<'_>) -> Poll<Result<Accepted, Error>>;
}

impl Accept for Vec<TcpListener> {
    fn poll_accept(&mut self, cx: &mut std::task::Context<'_>) -> Poll<Result<Accepted, Error>> {
        for listener in &*self {
            if let Poll::Ready((stream, _)) = listener.poll_accept(cx)? {
                return Poll::Ready(Ok(Accepted {
                    https_redirect: false,
                    stream,
                }));
            }
        }
        Poll::Pending
    }
}
impl Accept for NetworkInterfaceListener {
    fn poll_accept(&mut self, cx: &mut std::task::Context<'_>) -> Poll<Result<Accepted, Error>> {
        NetworkInterfaceListener::poll_accept(self, cx, true).map(|res| {
            res.map(|a| Accepted {
                https_redirect: a.is_public,
                stream: a.stream,
            })
        })
    }
}

impl<A: Accept, B: Accept> Accept for Either<A, B> {
    fn poll_accept(&mut self, cx: &mut std::task::Context<'_>) -> Poll<Result<Accepted, Error>> {
        match self {
            Either::Left(a) => a.poll_accept(cx),
            Either::Right(b) => b.poll_accept(cx),
        }
    }
}
impl<A: Accept> Accept for Option<A> {
    fn poll_accept(&mut self, cx: &mut std::task::Context<'_>) -> Poll<Result<Accepted, Error>> {
        match self {
            None => Poll::Pending,
            Some(a) => a.poll_accept(cx),
        }
    }
}

#[pin_project::pin_project]
pub struct Acceptor<A: Accept> {
    acceptor: (watch::Sender<A>, watch::Receiver<A>),
    changed: Option<BoxFuture<'static, ()>>,
}
impl<A: Accept + Send + Sync + 'static> Acceptor<A> {
    pub fn new(acceptor: A) -> Self {
        Self {
            acceptor: watch::channel(acceptor),
            changed: None,
        }
    }

    fn poll_changed(&mut self, cx: &mut std::task::Context<'_>) -> Poll<()> {
        let mut changed = if let Some(changed) = self.changed.take() {
            changed
        } else {
            let mut recv = self.acceptor.1.clone();
            async move {
                let _ = recv.changed().await;
            }
            .boxed()
        };
        let res = changed.poll_unpin(cx);
        if res.is_pending() {
            self.changed = Some(changed);
        }
        res
    }

    fn poll_accept(&mut self, cx: &mut std::task::Context<'_>) -> Poll<Result<Accepted, Error>> {
        let _ = self.poll_changed(cx);
        let mut res = Poll::Pending;
        self.acceptor.0.send_if_modified(|a| {
            res = a.poll_accept(cx);
            false
        });
        res
    }

    async fn accept(&mut self) -> Result<Accepted, Error> {
        std::future::poll_fn(|cx| self.poll_accept(cx)).await
    }
}
impl Acceptor<Vec<TcpListener>> {
    pub async fn bind(listen: impl IntoIterator<Item = SocketAddr>) -> Result<Self, Error> {
        Ok(Self::new(
            futures::future::try_join_all(listen.into_iter().map(TcpListener::bind)).await?,
        ))
    }
}

pub type UpgradableListener = Option<Either<Vec<TcpListener>, NetworkInterfaceListener>>;

impl Acceptor<UpgradableListener> {
    pub async fn bind_upgradable(
        listen: impl IntoIterator<Item = SocketAddr>,
    ) -> Result<Self, Error> {
        Ok(Self::new(Some(Either::Left(
            futures::future::try_join_all(listen.into_iter().map(TcpListener::bind)).await?,
        ))))
    }
}

pub struct WebServerAcceptorSetter<A: Accept> {
    acceptor: watch::Sender<A>,
}
impl<A: Accept, B: Accept> WebServerAcceptorSetter<Option<Either<A, B>>> {
    pub fn try_upgrade<F: FnOnce(A) -> Result<B, Error>>(&self, f: F) -> Result<(), Error> {
        let mut res = Ok(());
        self.acceptor.send_modify(|a| {
            *a = match a.take() {
                Some(Either::Left(a)) => match f(a) {
                    Ok(b) => Some(Either::Right(b)),
                    Err(e) => {
                        res = Err(e);
                        None
                    }
                },
                x => x,
            }
        });
        res
    }
}
impl<A: Accept> Deref for WebServerAcceptorSetter<A> {
    type Target = watch::Sender<A>;
    fn deref(&self) -> &Self::Target {
        &self.acceptor
    }
}

pub struct WebServer<A: Accept> {
    shutdown: oneshot::Sender<()>,
    router: watch::Sender<Option<Router>>,
    acceptor: watch::Sender<A>,
    thread: NonDetachingJoinHandle<()>,
}
impl<A: Accept + Send + Sync + 'static> WebServer<A> {
    pub fn acceptor_setter(&self) -> WebServerAcceptorSetter<A> {
        WebServerAcceptorSetter {
            acceptor: self.acceptor.clone(),
        }
    }

    pub fn new(mut acceptor: Acceptor<A>) -> Self {
        let acceptor_send = acceptor.acceptor.0.clone();
        let (router, service) = watch::channel::<Option<Router>>(None);
        let (shutdown, shutdown_recv) = oneshot::channel();
        let thread = NonDetachingJoinHandle::from(tokio::spawn(async move {
            #[derive(Clone)]
            struct QueueRunner {
                queue: Arc<RwLock<Option<BackgroundJobQueue>>>,
            }
            impl<Fut> hyper::rt::Executor<Fut> for QueueRunner
            where
                Fut: Future + Send + 'static,
            {
                fn execute(&self, fut: Fut) {
                    if let Some(q) = &*self.queue.read().unwrap() {
                        q.add_job(fut);
                    } else {
                        tracing::warn!("job queued after shutdown");
                    }
                }
            }

            struct SwappableRouter(watch::Receiver<Option<Router>>, bool);
            impl hyper::service::Service<hyper::Request<hyper::body::Incoming>> for SwappableRouter {
                type Response = <Router as tower_service::Service<
                    hyper::Request<hyper::body::Incoming>,
                >>::Response;
                type Error = <Router as tower_service::Service<
                    hyper::Request<hyper::body::Incoming>,
                >>::Error;
                type Future = <Router as tower_service::Service<
                    hyper::Request<hyper::body::Incoming>,
                >>::Future;

                fn call(&self, req: hyper::Request<hyper::body::Incoming>) -> Self::Future {
                    use tower_service::Service;

                    if self.1 {
                        redirecter().call(req)
                    } else {
                        let router = { self.0.borrow().clone() };
                        if let Some(mut router) = router {
                            router.call(req)
                        } else {
                            refresher().call(req)
                        }
                    }
                }
            }

            let accept = AtomicBool::new(true);
            let queue_cell = Arc::new(RwLock::new(None));
            let graceful = hyper_util::server::graceful::GracefulShutdown::new();
            let mut server = hyper_util::server::conn::auto::Builder::new(QueueRunner {
                queue: queue_cell.clone(),
            });
            server
                .http1()
                .timer(TokioTimer::new())
                .title_case_headers(true)
                .preserve_header_case(true)
                .http2()
                .timer(TokioTimer::new())
                .enable_connect_protocol()
                .keep_alive_interval(Duration::from_secs(60))
                .keep_alive_timeout(Duration::from_secs(300));
            let (queue, mut runner) = BackgroundJobQueue::new();
            *queue_cell.write().unwrap() = Some(queue.clone());

            let handler = async {
                loop {
                    if let Err(e) = async {
                        let accepted = acceptor.accept().await?;
                        queue.add_job(
                            graceful.watch(
                                server
                                    .serve_connection_with_upgrades(
                                        TokioIo::new(accepted.stream),
                                        SwappableRouter(service.clone(), accepted.https_redirect),
                                    )
                                    .into_owned(),
                            ),
                        );

                        Ok::<_, Error>(())
                    }
                    .await
                    {
                        tracing::error!("Error accepting HTTP connection: {e}");
                        tracing::debug!("{e:?}");
                    }
                }
            }
            .boxed();

            tokio::select! {
                _ = shutdown_recv => (),
                _ = handler => (),
                _ = &mut runner => (),
            }

            accept.store(false, std::sync::atomic::Ordering::SeqCst);
            drop(queue);
            drop(queue_cell.write().unwrap().take());

            if !runner.is_empty() {
                runner.await;
            }
        }));
        Self {
            shutdown,
            router,
            thread,
            acceptor: acceptor_send,
        }
    }

    pub async fn shutdown(self) {
        self.shutdown.send(()).unwrap_or_default();
        self.thread.await.unwrap()
    }

    pub fn serve_router(&mut self, router: Router) {
        self.router.send_replace(Some(router));
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
