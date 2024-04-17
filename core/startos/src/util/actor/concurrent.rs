use std::any::Any;
use std::sync::Arc;
use std::time::Duration;

use futures::future::{ready, BoxFuture};
use futures::{Future, FutureExt, TryFutureExt};
use helpers::NonDetachingJoinHandle;
use tokio::sync::{mpsc, oneshot};

use crate::prelude::*;
use crate::util::actor::background::{BackgroundJobQueue, BackgroundJobRunner};
use crate::util::actor::{Actor, ConflictFn, Handler, PendingMessageStrategy, Request};

#[pin_project::pin_project]
struct ConcurrentRunner<A> {
    actor: A,
    shutdown: Option<oneshot::Receiver<()>>,
    waiting: Vec<Request<A>>,
    recv: mpsc::UnboundedReceiver<Request<A>>,
    handlers: Vec<(
        Arc<ConflictFn<A>>,
        oneshot::Sender<Box<dyn Any + Send>>,
        BoxFuture<'static, Box<dyn Any + Send>>,
    )>,
    queue: BackgroundJobQueue,
    #[pin]
    bg_runner: BackgroundJobRunner,
}
impl<A: Actor + Clone> Future for ConcurrentRunner<A> {
    type Output = ();
    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        let mut this = self.project();
        *this.shutdown = this.shutdown.take().and_then(|mut s| {
            if s.poll_unpin(cx).is_pending() {
                Some(s)
            } else {
                None
            }
        });
        if this.shutdown.is_some() {
            while let std::task::Poll::Ready(Some((msg, reply))) = this.recv.poll_recv(cx) {
                if this.handlers.iter().any(|(f, _, _)| f(&*msg)) {
                    this.waiting.push((msg, reply));
                } else {
                    let mut actor = this.actor.clone();
                    let queue = this.queue.clone();
                    this.handlers.push((
                        msg.conflicts_with(),
                        reply,
                        async move { msg.handle_with(&mut actor, &queue).await }.boxed(),
                    ))
                }
            }
        }
        // handlers
        while {
            let mut cont = false;
            let complete = this
                .handlers
                .iter_mut()
                .enumerate()
                .filter_map(|(i, (_, _, f))| match f.poll_unpin(cx) {
                    std::task::Poll::Pending => None,
                    std::task::Poll::Ready(res) => Some((i, res)),
                })
                .collect::<Vec<_>>();
            for (idx, res) in complete.into_iter().rev() {
                #[allow(clippy::let_underscore_future)]
                let (f, reply, _) = this.handlers.swap_remove(idx);
                let _ = reply.send(res);
                // TODO: replace with Vec::extract_if once stable
                if this.shutdown.is_some() {
                    let mut i = 0;
                    while i < this.waiting.len() {
                        if f(&*this.waiting[i].0)
                            && !this.handlers.iter().any(|(f, _, _)| f(&*this.waiting[i].0))
                        {
                            let (msg, reply) = this.waiting.remove(i);
                            let mut actor = this.actor.clone();
                            let queue = this.queue.clone();
                            this.handlers.push((
                                msg.conflicts_with(),
                                reply,
                                async move { msg.handle_with(&mut actor, &queue).await }.boxed(),
                            ));
                            cont = true;
                        } else {
                            i += 1;
                        }
                    }
                }
            }
            cont
        } {}
        let _ = this.bg_runner.as_mut().poll(cx);
        if this.waiting.is_empty() && this.handlers.is_empty() && this.recv.is_closed() {
            std::task::Poll::Ready(())
        } else {
            std::task::Poll::Pending
        }
    }
}

pub struct ConcurrentActor<A: Actor + Clone> {
    shutdown: oneshot::Sender<()>,
    runtime: NonDetachingJoinHandle<()>,
    messenger: mpsc::UnboundedSender<Request<A>>,
}
impl<A: Actor + Clone> ConcurrentActor<A> {
    pub fn new(mut actor: A) -> Self {
        let (shutdown_send, shutdown_recv) = oneshot::channel();
        let (messenger_send, messenger_recv) = mpsc::unbounded_channel::<Request<A>>();
        let runtime = NonDetachingJoinHandle::from(tokio::spawn(async move {
            let (queue, runner) = BackgroundJobQueue::new();
            actor.init(&queue);
            ConcurrentRunner {
                actor,
                shutdown: Some(shutdown_recv),
                waiting: Vec::new(),
                recv: messenger_recv,
                handlers: Vec::new(),
                queue,
                bg_runner: runner,
            }
            .await
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
        self.messenger
            .send((Box::new(message), reply_send))
            .unwrap();
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
                self.shutdown.send(()).unwrap();
                Some(Duration::from_secs(0))
            }
            PendingMessageStrategy::FinishCurrentCancelPending { timeout } => {
                self.shutdown.send(()).unwrap();
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
