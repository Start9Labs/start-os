use futures::future::BoxFuture;
use futures::{Future, FutureExt};
use tokio::sync::mpsc;

#[derive(Clone)]
pub struct BackgroundJobQueue(mpsc::UnboundedSender<BoxFuture<'static, ()>>);
impl BackgroundJobQueue {
    pub fn new() -> (Self, BackgroundJobRunner) {
        let (send, recv) = mpsc::unbounded_channel();
        (
            Self(send),
            BackgroundJobRunner {
                recv,
                jobs: Vec::new(),
            },
        )
    }
    pub fn add_job(&self, fut: impl Future<Output = ()> + Send + 'static) {
        let _ = self.0.send(fut.boxed());
    }
}

pub struct BackgroundJobRunner {
    recv: mpsc::UnboundedReceiver<BoxFuture<'static, ()>>,
    jobs: Vec<BoxFuture<'static, ()>>,
}
impl BackgroundJobRunner {
    pub fn is_empty(&self) -> bool {
        self.recv.is_empty() && self.jobs.is_empty()
    }
}
impl Future for BackgroundJobRunner {
    type Output = ();
    fn poll(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        while let std::task::Poll::Ready(Some(job)) = self.recv.poll_recv(cx) {
            self.jobs.push(job);
        }
        let complete = self
            .jobs
            .iter_mut()
            .enumerate()
            .filter_map(|(i, f)| match f.poll_unpin(cx) {
                std::task::Poll::Pending => None,
                std::task::Poll::Ready(_) => Some(i),
            })
            .collect::<Vec<_>>();
        for idx in complete.into_iter().rev() {
            #[allow(clippy::let_underscore_future)]
            let _ = self.jobs.swap_remove(idx);
        }
        if self.jobs.is_empty() && self.recv.is_closed() {
            std::task::Poll::Ready(())
        } else {
            std::task::Poll::Pending
        }
    }
}
