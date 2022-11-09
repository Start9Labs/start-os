use std::future::Future;
use std::path::{Path, PathBuf};
use std::time::Duration;

use color_eyre::eyre::{eyre, Context, Error};
use futures::future::BoxFuture;
use futures::FutureExt;
use tokio::fs::File;
use tokio::sync::oneshot;
use tokio::task::{JoinError, JoinHandle, LocalSet};

mod byte_replacement_reader;
mod rsync;
mod script_dir;
pub use byte_replacement_reader::*;
pub use rsync::*;
pub use script_dir::*;

pub fn to_tmp_path(path: impl AsRef<Path>) -> Result<PathBuf, Error> {
    let path = path.as_ref();
    if let (Some(parent), Some(file_name)) =
        (path.parent(), path.file_name().and_then(|f| f.to_str()))
    {
        Ok(parent.join(format!(".{}.tmp", file_name)))
    } else {
        Err(eyre!("invalid path: {}", path.display()))
    }
}

pub async fn canonicalize(
    path: impl AsRef<Path> + Send + Sync,
    create_parent: bool,
) -> Result<PathBuf, Error> {
    fn create_canonical_folder<'a>(
        path: impl AsRef<Path> + Send + Sync + 'a,
    ) -> BoxFuture<'a, Result<PathBuf, Error>> {
        async move {
            let path = canonicalize(path, true).await?;
            tokio::fs::create_dir(&path)
                .await
                .with_context(|| path.display().to_string())?;
            Ok(path)
        }
        .boxed()
    }
    let path = path.as_ref();
    if tokio::fs::metadata(path).await.is_err() {
        if let (Some(parent), Some(file_name)) = (path.parent(), path.file_name()) {
            if create_parent && tokio::fs::metadata(parent).await.is_err() {
                return Ok(create_canonical_folder(parent).await?.join(file_name));
            } else {
                return Ok(tokio::fs::canonicalize(parent)
                    .await
                    .with_context(|| parent.display().to_string())?
                    .join(file_name));
            }
        }
    }
    tokio::fs::canonicalize(&path)
        .await
        .with_context(|| path.display().to_string())
}

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

pub struct AtomicFile {
    tmp_path: PathBuf,
    path: PathBuf,
    file: Option<File>,
}
impl AtomicFile {
    pub async fn new(
        path: impl AsRef<Path> + Send + Sync,
        tmp_path: Option<impl AsRef<Path> + Send + Sync>,
    ) -> Result<Self, Error> {
        let path = canonicalize(&path, true).await?;
        let tmp_path = if let Some(tmp_path) = tmp_path {
            canonicalize(&tmp_path, true).await?
        } else {
            to_tmp_path(&path)?
        };
        let file = File::create(&tmp_path)
            .await
            .with_context(|| tmp_path.display().to_string())?;
        Ok(Self {
            tmp_path,
            path,
            file: Some(file),
        })
    }

    pub async fn rollback(mut self) -> Result<(), Error> {
        drop(self.file.take());
        tokio::fs::remove_file(&self.tmp_path)
            .await
            .with_context(|| format!("rm {}", self.tmp_path.display()))?;
        Ok(())
    }

    pub async fn save(mut self) -> Result<(), Error> {
        use tokio::io::AsyncWriteExt;
        if let Some(file) = self.file.as_mut() {
            file.flush().await?;
            file.shutdown().await?;
            file.sync_all().await?;
        }
        drop(self.file.take());
        tokio::fs::rename(&self.tmp_path, &self.path)
            .await
            .with_context(|| {
                format!("mv {} -> {}", self.tmp_path.display(), self.path.display())
            })?;
        Ok(())
    }
}
impl std::ops::Deref for AtomicFile {
    type Target = File;
    fn deref(&self) -> &Self::Target {
        self.file.as_ref().unwrap()
    }
}
impl std::ops::DerefMut for AtomicFile {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.file.as_mut().unwrap()
    }
}
impl Drop for AtomicFile {
    fn drop(&mut self) {
        if let Some(file) = self.file.take() {
            drop(file);
            let path = std::mem::take(&mut self.tmp_path);
            tokio::spawn(async move { tokio::fs::remove_file(path).await.unwrap() });
        }
    }
}

pub struct TimedResource<T: 'static + Send> {
    handle: NonDetachingJoinHandle<Option<T>>,
    ready: oneshot::Sender<()>,
}
impl<T: 'static + Send> TimedResource<T> {
    pub fn new(resource: T, timer: Duration) -> Self {
        let (send, recv) = oneshot::channel();
        let handle = tokio::spawn(async move {
            tokio::select! {
                _ = tokio::time::sleep(timer) => {
                    drop(resource);
                    None
                },
                _ = recv => Some(resource),
            }
        });
        Self {
            handle: handle.into(),
            ready: send,
        }
    }

    pub fn new_with_destructor<
        Fn: FnOnce(T) -> Fut + Send + 'static,
        Fut: Future<Output = ()> + Send,
    >(
        resource: T,
        timer: Duration,
        destructor: Fn,
    ) -> Self {
        let (send, recv) = oneshot::channel();
        let handle = tokio::spawn(async move {
            tokio::select! {
                _ = tokio::time::sleep(timer) => {
                    destructor(resource).await;
                    None
                },
                _ = recv => Some(resource),
            }
        });
        Self {
            handle: handle.into(),
            ready: send,
        }
    }

    pub async fn get(self) -> Option<T> {
        let _ = self.ready.send(());
        self.handle.await.unwrap()
    }

    pub fn is_timed_out(&self) -> bool {
        self.ready.is_closed()
    }
}

pub async fn spawn_local<
    T: 'static + Send,
    F: FnOnce() -> Fut + Send + 'static,
    Fut: Future<Output = T> + 'static,
>(
    fut: F,
) -> NonDetachingJoinHandle<T> {
    let (send, recv) = tokio::sync::oneshot::channel();
    std::thread::spawn(move || {
        tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .unwrap()
            .block_on(async move {
                let set = LocalSet::new();
                send.send(set.spawn_local(fut()).into())
                    .unwrap_or_else(|_| unreachable!());
                set.await
            })
    });
    recv.await.unwrap()
}
