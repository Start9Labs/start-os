use std::any::Any;
use std::future::ready;
use std::time::Duration;

use futures::future::BoxFuture;
use futures::{Future, FutureExt, TryFutureExt};
use helpers::NonDetachingJoinHandle;
use tokio::sync::oneshot::error::TryRecvError;
use tokio::sync::{mpsc, oneshot};

use crate::prelude::*;
use crate::util::Never;

pub trait Actor: Send {}

#[async_trait::async_trait]
pub trait Handler<M>: Actor {
    type Response: Any + Send;
    async fn handle(&mut self, msg: M, jobs: &mut BackgroundJobs) -> Self::Response;
}

#[async_trait::async_trait]
trait Message<A>: Send {
    async fn handle_with(
        self: Box<Self>,
        actor: &mut A,
        jobs: &mut BackgroundJobs,
    ) -> Box<dyn Any + Send>;
}
#[async_trait::async_trait]
impl<M: Send, A: Actor> Message<A> for M
where
    A: Handler<M>,
{
    async fn handle_with(
        self: Box<Self>,
        actor: &mut A,
        jobs: &mut BackgroundJobs,
    ) -> Box<dyn Any + Send> {
        Box::new(actor.handle(*self, jobs).await)
    }
}

type Request<A> = (Box<dyn Message<A>>, oneshot::Sender<Box<dyn Any + Send>>);

#[derive(Default)]
pub struct BackgroundJobs {
    jobs: Vec<BoxFuture<'static, ()>>,
}
impl BackgroundJobs {
    pub fn add_job(&mut self, fut: impl Future<Output = ()> + Send + 'static) {
        self.jobs.push(fut.boxed());
    }
}
impl Future for BackgroundJobs {
    type Output = Never;
    fn poll(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        let complete =
            self.jobs
                .iter_mut()
                .enumerate()
                .filter_map(|(i, f)| match f.poll_unpin(cx) {
                    std::task::Poll::Pending => None,
                    std::task::Poll::Ready(_) => Some(i),
                });
        for idx in complete.into_iter().rev() {
            #[allow(clippy::let_underscore_future)]
            let _ = self.jobs.swap_remove(idx);
        }
        std::task::Poll::Pending
    }
}

pub struct SimpleActor<A: Actor> {
    shutdown: oneshot::Sender<()>,
    runtime: NonDetachingJoinHandle<()>,
    messenger: mpsc::UnboundedSender<Request<A>>,
}
impl<A: Actor> SimpleActor<A> {
    pub fn new(actor: A) -> Self {
        let (shutdown_send, shutdown_recv) = oneshot::channel();
        let (messenger_send, messenger_recv) = mpsc::unbounded_channel::<Request<A>>();
        let runtime = NonDetachingJoinHandle::from(tokio::spawn(async move {
            let mut bg = BackgroundJobs::default();
            loop {
                tokio::select! {
                    _ = bg => (),
                    msg = messenger_recv.recv() => match msg {
                        Some((msg, reply)) if shutdown_recv.try_recv() == Err(TryRecvError::Empty) => {
                            let mut new_bg = BackgroundJobs::default();
                            tokio::select! {
                                res = msg.handle_with(&mut actor, &mut new_bg) => { reply.send(res); },
                                _ = bg => (),
                            }
                            bg.jobs.append(&mut new_bg.jobs);
                        }
                        _ => break,
                    },
                }
            }
        }));
        Self {
            shutdown: shutdown_send,
            runtime,
            messenger: messenger_send,
        }
    }

    /// Message is guaranteed to be queued immediately
    pub fn queue<M: Send + 'static>(
        &self,
        message: M,
    ) -> impl Future<Output = Result<A::Response, Error>>
    where
        A: Handler<M>,
    {
        if self.runtime.is_finished() {
            return futures::future::Either::Left(ready(Err(Error::new(
                eyre!("actor runtime has exited"),
                ErrorKind::Unknown,
            ))));
        }
        let (reply_send, reply_recv) = oneshot::channel();
        self.messenger.send((Box::new(message), reply_send));
        futures::future::Either::Right(
            reply_recv
                .map_err(|_| Error::new(eyre!("actor runtime has exited"), ErrorKind::Unknown))
                .and_then(|a| {
                    ready(
                        a.downcast()
                            .map_err(|_| {
                                Error::new(
                                    eyre!("received incorrect type in response"),
                                    ErrorKind::Incoherent,
                                )
                            })
                            .map(|a| *a),
                    )
                }),
        )
    }

    pub async fn send<M: Send + 'static>(&self, message: M) -> Result<A::Response, Error>
    where
        A: Handler<M>,
    {
        self.queue(message).await
    }

    pub async fn shutdown(self, strategy: PendingMessageStrategy) {
        drop(self.messenger);
        let timeout = match strategy {
            PendingMessageStrategy::CancelAll => {
                self.shutdown.send(());
                Some(Duration::from_secs(0))
            }
            PendingMessageStrategy::FinishCurrentCancelPending { timeout } => {
                self.shutdown.send(());
                timeout
            }
            PendingMessageStrategy::FinishAll { timeout } => timeout,
        };
        let aborter = if let Some(timeout) = timeout {
            let hdl = self.runtime.abort_handle();
            async move {
                tokio::time::sleep(timeout).await;
                hdl.abort();
            }
            .boxed()
        } else {
            futures::future::pending().boxed()
        };
        tokio::select! {
            _ = aborter => (),
            _ = self.runtime => (),
        }
    }
}

pub enum PendingMessageStrategy {
    CancelAll,
    FinishCurrentCancelPending { timeout: Option<Duration> },
    FinishAll { timeout: Option<Duration> },
}
