use std::future::Future;
use std::pin::Pin;
use std::sync::atomic::AtomicUsize;
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

struct WatchData<T> {
    data: T,
    wakers: Vec<Waker>,
}

struct Shared<T> {
    data: SyncMutex<WatchData<T>>,
    version: AtomicUsize,
}

pub struct Watch<T> {
    data: Arc<Shared<T>>,
    seen_version: usize,
}
impl<T> Clone for Watch<T> {
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
            seen_version: self.seen_version,
        }
    }
}
impl<T> Watch<T> {
    pub fn new(init: T) -> Self {
        Self {
            data: Arc::new(Shared {
                data: SyncMutex::new(WatchData {
                    data: init,
                    wakers: Vec::new(),
                }),
                version: AtomicUsize::new(1),
            }),
            seen_version: 1,
        }
    }

    pub fn poll_changed(&mut self, cx: &mut std::task::Context<'_>) -> Poll<()> {
        let mut guard = self.data.data.0.lock().unwrap();
        let version = self.data.version.load(std::sync::atomic::Ordering::SeqCst);
        if version > self.seen_version {
            self.seen_version = version;
            return Poll::Ready(());
        }
        let waker = cx.waker();
        if !guard.wakers.iter().any(|w| w.will_wake(waker)) {
            guard.wakers.push(waker.clone());
        }
        Poll::Pending
    }

    pub async fn changed(&mut self) {
        #[pin_project::pin_project]
        struct Changed<'a, T>(&'a mut Watch<T>);
        impl<'a, T> Future for Changed<'a, T> {
            type Output = ();
            fn poll(self: Pin<&mut Self>, cx: &mut std::task::Context<'_>) -> Poll<Self::Output> {
                let this = self.project();
                this.0.poll_changed(cx)
            }
        }
        Changed(self).await
    }

    pub fn mark_changed(&mut self) {
        self.seen_version = 0;
    }

    pub fn peek<F, U>(&self, peek: F) -> U
    where
        F: FnOnce(&T) -> U,
    {
        self.data.data.peek(|d| peek(&d.data))
    }

    pub fn send_if_modified<F>(&self, modify: F) -> bool
    where
        F: FnOnce(&mut T) -> bool,
    {
        let mut guard = self.data.data.0.lock().unwrap();
        let changed = modify(&mut guard.data);
        if changed {
            self.data
                .version
                .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
            for waker in guard.wakers.drain(..) {
                waker.wake();
            }
        }
        changed
    }

    pub fn send_modify<F>(&self, modify: F)
    where
        F: FnOnce(&mut T),
    {
        self.send_if_modified(|x| {
            modify(x);
            true
        });
    }

    pub fn send_replace(&self, mut value: T) -> T {
        self.send_modify(|x| {
            std::mem::swap(x, &mut value);
        });
        value
    }

    pub fn send(&self, value: T) {
        self.send_replace(value);
    }
}

impl<T: Clone> Watch<T> {
    pub fn read(&self) -> T {
        self.data.data.0.lock().unwrap().data.clone()
    }
}
