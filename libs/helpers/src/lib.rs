use std::future::Future;

use tokio::task::{JoinError, JoinHandle};

mod script_dir;
pub use script_dir::*;

#[pin_project::pin_project(PinnedDrop)]
pub struct NonDetachingJoinHandle<T>(#[pin] JoinHandle<T>);
impl<T> From<JoinHandle<T>> for NonDetachingJoinHandle<T> {
    fn from(t: JoinHandle<T>) -> Self {
        NonDetachingJoinHandle(t)
    }
}
#[pin_project::pinned_drop]
impl<T> PinnedDrop for NonDetachingJoinHandle<T> {
    fn drop(self: std::pin::Pin<&mut Self>) {
        let this = self.project();
        this.0.into_ref().get_ref().abort()
    }
}
impl<T> Future for NonDetachingJoinHandle<T> {
    type Output = Result<T, JoinError>;
    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        let this = self.project();
        this.0.poll(cx)
    }
}
