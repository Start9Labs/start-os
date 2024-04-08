use std::time::Duration;

use futures::future::ready;
use futures::{Future, FutureExt, TryFutureExt};
use helpers::NonDetachingJoinHandle;
use tokio::sync::oneshot::error::TryRecvError;
use tokio::sync::{mpsc, oneshot};

use crate::prelude::*;
use crate::util::actor::background::BackgroundJobQueue;
use crate::util::actor::{Actor, Handler, PendingMessageStrategy, Request};

pub struct SimpleActor<A: Actor> {
    shutdown: oneshot::Sender<()>,
    runtime: NonDetachingJoinHandle<()>,
    messenger: mpsc::UnboundedSender<Request<A>>,
}
impl<A: Actor> SimpleActor<A> {
    pub fn new(mut actor: A) -> Self {
        let (shutdown_send, mut shutdown_recv) = oneshot::channel();
        let (messenger_send, mut messenger_recv) = mpsc::unbounded_channel::<Request<A>>();
        let runtime = NonDetachingJoinHandle::from(tokio::spawn(async move {
            let (queue, mut runner) = BackgroundJobQueue::new();
            actor.init(&queue);
            loop {
                tokio::select! {
                    _ = &mut runner => (),
                    msg = messenger_recv.recv() => match msg {
                        Some((msg, reply)) if shutdown_recv.try_recv() == Err(TryRecvError::Empty) => {
                            tokio::select! {
                                res = msg.handle_with(&mut actor, &queue) => { let _ = reply.send(res); },
                                _ = &mut runner => (),
                            }
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
