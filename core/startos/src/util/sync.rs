use std::pin::Pin;
use std::sync::Arc;
use std::task::{Poll, Waker};

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
