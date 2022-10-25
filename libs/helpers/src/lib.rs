use std::future::Future;
use std::path::{Path, PathBuf};
use std::pin::Pin;
use std::time::Duration;

use color_eyre::eyre::{eyre, Context, Error};
use futures::future::{pending, BoxFuture};
use futures::FutureExt;
use tokio::fs::File;
use tokio::sync::oneshot;
use tokio::task::{JoinError, JoinHandle};

mod script_dir;
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

type SingThreadTask<T> = futures::future::Select<
    futures::future::Then<
        oneshot::Receiver<T>,
        futures::future::Either<futures::future::Ready<T>, futures::future::Pending<T>>,
        fn(
            Result<T, oneshot::error::RecvError>,
        )
            -> futures::future::Either<futures::future::Ready<T>, futures::future::Pending<T>>,
    >,
    futures::future::Then<
        JoinHandle<()>,
        futures::future::Pending<T>,
        fn(Result<(), JoinError>) -> futures::future::Pending<T>,
    >,
>;

#[pin_project::pin_project(PinnedDrop)]
pub struct SingleThreadJoinHandle<T> {
    abort: Option<oneshot::Sender<()>>,
    #[pin]
    task: SingThreadTask<T>,
}
impl<T: Send + 'static> SingleThreadJoinHandle<T> {
    pub fn new<Fut: Future<Output = T>>(fut: impl FnOnce() -> Fut + Send + 'static) -> Self {
        let (abort, abort_recv) = oneshot::channel();
        let (return_val_send, return_val) = oneshot::channel();
        fn unwrap_recv_or_pending<T>(
            res: Result<T, oneshot::error::RecvError>,
        ) -> futures::future::Either<futures::future::Ready<T>, futures::future::Pending<T>>
        {
            match res {
                Ok(a) => futures::future::Either::Left(futures::future::ready(a)),
                _ => futures::future::Either::Right(pending()),
            }
        }
        fn make_pending<T>(_: Result<(), JoinError>) -> futures::future::Pending<T> {
            pending()
        }
        Self {
            abort: Some(abort),
            task: futures::future::select(
                return_val.then(unwrap_recv_or_pending),
                tokio::task::spawn_blocking(move || {
                    tokio::runtime::Handle::current().block_on(async move {
                        tokio::select! {
                            _ = abort_recv.fuse() => (),
                            res = fut().fuse() => {let _error = return_val_send.send(res);},
                        }
                    })
                })
                .then(make_pending),
            ),
        }
    }
}

impl<T: Send> Future for SingleThreadJoinHandle<T> {
    type Output = T;
    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        let this = self.project();
        this.task.poll(cx).map(|t| t.factor_first().0)
    }
}

#[pin_project::pinned_drop]
impl<T> PinnedDrop for SingleThreadJoinHandle<T> {
    fn drop(self: Pin<&mut Self>) {
        let this = self.project();
        if let Some(abort) = this.abort.take() {
            let _error = abort.send(());
        }
    }
}
