use std::pin::Pin;
use std::task::{Context, Poll};

use futures::future::abortable;
use futures::stream::{AbortHandle, Abortable};
use futures::{Future, FutureExt};
use tokio::sync::watch;

#[pin_project::pin_project(PinnedDrop)]
pub struct DropSignaling<F> {
    #[pin]
    fut: F,
    on_drop: watch::Sender<bool>,
}
impl<F> DropSignaling<F> {
    pub fn new(fut: F) -> Self {
        Self {
            fut,
            on_drop: watch::channel(false).0,
        }
    }
    pub fn subscribe(&self) -> DropHandle {
        DropHandle(self.on_drop.subscribe())
    }
}
impl<F> Future for DropSignaling<F>
where
    F: Future,
{
    type Output = F::Output;
    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.project();
        this.fut.poll(cx)
    }
}
#[pin_project::pinned_drop]
impl<F> PinnedDrop for DropSignaling<F> {
    fn drop(self: Pin<&mut Self>) {
        let _ = self.on_drop.send(true);
    }
}

#[derive(Clone)]
pub struct DropHandle(watch::Receiver<bool>);
impl DropHandle {
    pub async fn wait(&mut self) {
        self.0.wait_for(|a| *a).await;
    }
}

#[pin_project::pin_project]
pub struct RemoteCancellable<F> {
    #[pin]
    fut: Abortable<DropSignaling<F>>,
    on_drop: DropHandle,
    handle: AbortHandle,
}
impl<F: Future> RemoteCancellable<F> {
    pub fn new(fut: F) -> Self {
        let sig_fut = DropSignaling::new(fut);
        let on_drop = sig_fut.subscribe();
        let (fut, handle) = abortable(sig_fut);
        Self {
            fut,
            on_drop,
            handle,
        }
    }
}
impl<F> RemoteCancellable<F> {
    pub fn cancellation_handle(&self) -> CancellationHandle {
        CancellationHandle {
            on_drop: self.on_drop.clone(),
            handle: self.handle.clone(),
        }
    }
}
impl<F> Future for RemoteCancellable<F>
where
    F: Future,
{
    type Output = Option<F::Output>;
    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.project();
        this.fut.poll(cx).map(|a| a.ok())
    }
}

#[derive(Clone)]
pub struct CancellationHandle {
    on_drop: DropHandle,
    handle: AbortHandle,
}
impl CancellationHandle {
    pub fn cancel(&mut self) {
        self.handle.abort();
    }

    pub async fn cancel_and_wait(&mut self) {
        self.handle.abort();
        self.on_drop.wait().await
    }
}

#[tokio::test]
async fn test_cancellable() {
    use std::sync::Arc;

    let arc = Arc::new(());
    let weak = Arc::downgrade(&arc);
    let cancellable = RemoteCancellable::new(async move {
        futures::future::pending::<()>().await;
        drop(arc)
    });
    let handle = cancellable.cancellation_handle();
    let a = tokio::spawn(cancellable);
    handle.cancel_and_wait().await;
    assert!(weak.strong_count() == 0);
}
