use std::collections::VecDeque;
use std::pin::Pin;
use std::sync::atomic::AtomicUsize;
use std::sync::{Arc, Weak};
use std::task::{Poll, Waker};

use futures::stream::BoxStream;
use futures::Stream;

#[derive(Debug, Default)]
pub struct SyncMutex<T>(std::sync::Mutex<T>);
impl<T> SyncMutex<T> {
    pub fn new(t: T) -> Self {
        Self(std::sync::Mutex::new(t))
    }
    pub fn mutate<F: FnOnce(&mut T) -> U, U>(&self, f: F) -> U {
        f(&mut *self.0.lock().unwrap())
    }
    pub fn peek<F: FnOnce(&T) -> U, U>(&self, f: F) -> U {
        f(&*self.0.lock().unwrap())
    }
}

struct WatchShared<T> {
    version: u64,
    data: T,
    wakers: Vec<Waker>,
}
impl<T> WatchShared<T> {
    fn modified(&mut self) {
        self.version += 1;
        for waker in self.wakers.drain(..) {
            waker.wake();
        }
    }
}

#[pin_project::pin_project]
pub struct Watch<T> {
    shared: Arc<SyncMutex<WatchShared<T>>>,
    version: u64,
}
impl<T> Clone for Watch<T> {
    fn clone(&self) -> Self {
        Self {
            shared: self.shared.clone(),
            version: self.version,
        }
    }
}
impl<T> Watch<T> {
    pub fn new(init: T) -> Self {
        Self {
            shared: Arc::new(SyncMutex::new(WatchShared {
                version: 1,
                data: init,
                wakers: Vec::new(),
            })),
            version: 0,
        }
    }
    pub fn clone_unseen(&self) -> Self {
        Self {
            shared: self.shared.clone(),
            version: 0,
        }
    }
    pub fn poll_changed(&mut self, cx: &mut std::task::Context<'_>) -> Poll<()> {
        self.shared.mutate(|shared| {
            if shared.version != self.version {
                self.version = shared.version;
                Poll::Ready(())
            } else {
                let waker = cx.waker();
                if !shared.wakers.iter().any(|w| w.will_wake(waker)) {
                    shared.wakers.push(waker.clone());
                }
                Poll::Pending
            }
        })
    }
    pub async fn changed(&mut self) {
        futures::future::poll_fn(|cx| self.poll_changed(cx)).await
    }
    pub async fn wait_for<F: FnMut(&T) -> bool>(&mut self, mut f: F) {
        loop {
            if self.peek(&mut f) {
                break;
            }
            self.changed().await;
        }
    }
    pub fn send_if_modified<F: FnOnce(&mut T) -> bool>(&self, modify: F) -> bool {
        self.shared.mutate(|shared| {
            let changed = modify(&mut shared.data);
            if changed {
                shared.modified();
            }
            changed
        })
    }
    pub fn send_modify<U, F: FnOnce(&mut T) -> U>(&self, modify: F) -> U {
        self.shared.mutate(|shared| {
            let res = modify(&mut shared.data);
            shared.modified();
            res
        })
    }
    pub fn send_replace(&self, new: T) -> T {
        self.send_modify(|a| std::mem::replace(a, new))
    }
    pub fn send(&self, new: T) {
        self.send_replace(new);
    }
    pub fn mark_changed(&self) {
        self.shared.mutate(|shared| shared.modified())
    }
    pub fn mark_unseen(&mut self) {
        self.version = 0;
    }
    pub fn mark_seen(&mut self) {
        self.shared.peek(|shared| {
            self.version = shared.version;
        })
    }
    pub fn peek<U, F: FnOnce(&T) -> U>(&self, f: F) -> U {
        self.shared.peek(|shared| f(&shared.data))
    }
    pub fn peek_and_mark_seen<U, F: FnOnce(&T) -> U>(&mut self, f: F) -> U {
        self.shared.peek(|shared| {
            self.version = shared.version;
            f(&shared.data)
        })
    }
    pub fn peek_mut<U, F: FnOnce(&mut T) -> U>(&self, f: F) -> U {
        self.shared.mutate(|shared| f(&mut shared.data))
    }
}
impl<T: Clone> Watch<T> {
    pub fn read(&self) -> T {
        self.peek(|a| a.clone())
    }
}
impl<T: Clone> futures::Stream for Watch<T> {
    type Item = T;
    fn poll_next(
        self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Option<Self::Item>> {
        let this = self.project();
        this.shared.mutate(|shared| {
            if shared.version != *this.version {
                *this.version = shared.version;
                Poll::Ready(Some(shared.data.clone()))
            } else {
                let waker = cx.waker();
                if !shared.wakers.iter().any(|w| w.will_wake(waker)) {
                    shared.wakers.push(waker.clone());
                }
                Poll::Pending
            }
        })
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        (1, None)
    }
}

struct DupState<T, Upstream = BoxStream<'static, T>>
where
    T: Clone,
    Upstream: Stream<Item = T> + Unpin,
{
    buffer: VecDeque<T>,
    upstream: Upstream,
    pos: usize,
    pos_refs: Vec<Weak<AtomicUsize>>,
    wakers: Vec<Waker>,
}
impl<T: Clone, Upstream: Stream<Item = T> + Unpin> DupState<T, Upstream> {
    fn poll_next(&mut self, cx: &mut std::task::Context<'_>) -> Poll<Option<T>> {
        use futures::stream::StreamExt;
        let Some(next) = futures::ready!(self.upstream.poll_next_unpin(cx)) else {
            return Poll::Ready(None);
        };
        self.pos += 1;
        self.buffer.push_back(next.clone());
        for waker in self.wakers.drain(..) {
            if !waker.will_wake(cx.waker()) {
                waker.wake();
            }
        }

        Poll::Ready(Some(next))
    }
}

pub struct DupStream<T, Upstream = BoxStream<'static, T>>
where
    T: Clone,
    Upstream: Stream<Item = T> + Unpin,
{
    state: Arc<SyncMutex<DupState<T, Upstream>>>,
    pos: Arc<AtomicUsize>,
}
impl<T: Clone, Upstream: Stream<Item = T> + Unpin> DupStream<T, Upstream> {
    pub fn new(upstream: Upstream) -> Self {
        let pos = Arc::new(AtomicUsize::new(0));
        Self {
            state: Arc::new(SyncMutex::new(DupState {
                buffer: VecDeque::new(),
                upstream,
                pos: 0,
                pos_refs: vec![Arc::downgrade(&pos)],
                wakers: Vec::new(),
            })),
            pos,
        }
    }
}

impl<T: Clone, Upstream: Stream<Item = T> + Unpin> Clone for DupStream<T, Upstream> {
    fn clone(&self) -> Self {
        let pos = self.state.mutate(|state| {
            let pos = Arc::new(AtomicUsize::new(
                self.pos.load(std::sync::atomic::Ordering::Relaxed),
            ));
            state.pos_refs.push(Arc::downgrade(&pos));
            state.pos_refs.retain(|ptr| ptr.strong_count() > 0);
            pos
        });
        Self {
            state: self.state.clone(),
            pos,
        }
    }
}

impl<T: Clone, Upstream: Stream<Item = T> + Unpin> Stream for DupStream<T, Upstream> {
    type Item = T;
    fn poll_next(
        self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Option<Self::Item>> {
        self.state.mutate(|state| {
            let pos = self.pos.load(std::sync::atomic::Ordering::Relaxed);
            if pos < state.pos {
                self.pos.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                if state
                    .pos_refs
                    .iter()
                    .filter_map(|ptr| ptr.upgrade())
                    .all(|ptr| ptr.load(std::sync::atomic::Ordering::Relaxed) > pos)
                {
                    while let Some(next) = state.buffer.pop_front() {
                        if state.buffer.len() + 1 == state.pos - pos {
                            return Poll::Ready(Some(next));
                        }
                    }
                    Poll::Ready(None)
                } else {
                    Poll::Ready(
                        state
                            .buffer
                            .get(state.buffer.len() + pos - state.pos)
                            .cloned(),
                    )
                }
            } else {
                let res = state.poll_next(cx);
                if res.is_ready() {
                    self.pos
                        .store(state.pos, std::sync::atomic::Ordering::Relaxed);
                } else {
                    let waker = cx.waker();
                    if state.wakers.iter().all(|w| !w.will_wake(waker)) {
                        state.wakers.push(waker.clone());
                    }
                }
                res
            }
        })
    }
}

#[tokio::test]
async fn test_dup_stream() {
    use std::time::Duration;

    use futures::StreamExt;

    let stream = async_stream::stream! {
        for i in 0..100 {
            tokio::time::sleep(Duration::from_nanos(rand::random_range(0..=10000000))).await;
            yield i;
        }
    }
    .boxed();
    let n = rand::random_range(3..10);
    let mut tasks = Vec::with_capacity(n);
    for mut dup_stream in std::iter::repeat_n(DupStream::new(stream), n) {
        tasks.push(tokio::spawn(async move {
            let mut ctr = 0;
            while let Some(i) = dup_stream.next().await {
                assert_eq!(ctr, i);
                ctr += 1;
                tokio::time::sleep(Duration::from_nanos(rand::random_range(0..=10000000))).await;
            }
            assert_eq!(ctr, 100);
        }));
    }
    futures::future::try_join_all(tasks).await.unwrap();
}
