use std::future::Future;
use std::io::SeekFrom;
use std::pin::Pin;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::Arc;
use std::task::{Context, Poll};
use std::time::Duration;

use patch_db::{DbHandle, HasModel, OptionModel, PatchDb, PatchDbHandle};
use serde::{Deserialize, Serialize};
use tokio::io::{AsyncRead, AsyncSeek, AsyncWrite};

use crate::db::model::PackageDataEntry;
use crate::Error;

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct InstallProgress {
    pub size: Option<u64>,
    pub downloaded: AtomicU64,
    pub download_complete: AtomicBool,
    pub validated: AtomicU64,
    pub validation_complete: AtomicBool,
    pub read: AtomicU64,
    pub read_complete: AtomicBool,
}
impl InstallProgress {
    pub fn new(size: Option<u64>) -> Arc<Self> {
        Arc::new(InstallProgress {
            size,
            downloaded: AtomicU64::new(0),
            download_complete: AtomicBool::new(false),
            validated: AtomicU64::new(0),
            validation_complete: AtomicBool::new(false),
            read: AtomicU64::new(0),
            read_complete: AtomicBool::new(false),
        })
    }
    pub fn download_complete(&self) {
        self.download_complete.store(true, Ordering::SeqCst)
    }
    pub async fn track_download<Db: DbHandle>(
        self: Arc<Self>,
        model: OptionModel<InstallProgress>,
        mut db: Db,
    ) -> (Db, Result<(), Error>) {
        while !self.download_complete.load(Ordering::SeqCst) {
            if let Err(e) = model.put(&mut db, &self).await {
                return (db, Err(e.into()));
            }
            tokio::time::sleep(Duration::from_secs(1)).await;
        }
        if let Err(e) = model.put(&mut db, &self).await {
            (db, Err(e.into()))
        } else {
            (db, Ok(()))
        }
    }
    pub async fn track_download_during<
        F: FnOnce() -> Fut,
        Fut: Future<Output = Result<T, Error>>,
        T,
    >(
        self: &Arc<Self>,
        model: OptionModel<InstallProgress>,
        db: &PatchDb,
        handle: &mut PatchDbHandle,
        f: F,
    ) -> Result<T, Error> {
        let local_db = std::mem::replace(handle, db.handle());
        let tracker = tokio::spawn(self.clone().track_download(model.clone(), local_db));
        let res = f().await;
        self.download_complete.store(true, Ordering::SeqCst);
        let (local_db, tracker_res) = tracker.await.unwrap();
        let _ = std::mem::replace(handle, local_db);
        tracker_res?;
        res
    }
    pub async fn track_read<Db: DbHandle>(
        self: Arc<Self>,
        model: OptionModel<InstallProgress>,
        mut db: Db,
        complete: Arc<AtomicBool>,
    ) -> (Db, Result<(), Error>) {
        while !complete.load(Ordering::SeqCst) {
            if let Err(e) = model.put(&mut db, &self).await {
                return (db, Err(e.into()));
            }
            tokio::time::sleep(Duration::from_secs(1)).await;
        }
        (db, Ok(()))
    }
    pub async fn track_read_during<
        F: FnOnce() -> Fut,
        Fut: Future<Output = Result<T, Error>>,
        T,
    >(
        self: &Arc<Self>,
        model: OptionModel<InstallProgress>,
        db: &PatchDb,
        handle: &mut PatchDbHandle,
        f: F,
    ) -> Result<T, Error> {
        let local_db = std::mem::replace(handle, db.handle());
        let complete = Arc::new(AtomicBool::new(false));
        let tracker = tokio::spawn(self.clone().track_read(
            model.clone(),
            local_db,
            complete.clone(),
        ));
        let res = f().await;
        complete.store(true, Ordering::SeqCst);
        let (local_db, tracker_res) = tracker.await.unwrap();
        let _ = std::mem::replace(handle, local_db);
        tracker_res?;
        res
    }
}

#[pin_project::pin_project]
pub struct InstallProgressTracker<RW> {
    #[pin]
    inner: RW,
    validating: bool,
    progress: Arc<InstallProgress>,
}
impl<RW> InstallProgressTracker<RW> {
    pub fn new(inner: RW, progress: Arc<InstallProgress>) -> Self {
        InstallProgressTracker {
            inner,
            validating: true,
            progress,
        }
    }
    pub fn validated(&mut self) {
        self.validating = false;
    }
}
impl<W: AsyncWrite> AsyncWrite for InstallProgressTracker<W> {
    fn poll_write(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &[u8],
    ) -> Poll<Result<usize, std::io::Error>> {
        let this = self.project();
        match this.inner.poll_write(cx, buf) {
            Poll::Ready(Ok(n)) => {
                this.progress
                    .downloaded
                    .fetch_add(n as u64, Ordering::SeqCst);
                Poll::Ready(Ok(n))
            }
            a => a,
        }
    }
    fn poll_flush(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), std::io::Error>> {
        let this = self.project();
        this.inner.poll_flush(cx)
    }
    fn poll_shutdown(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
    ) -> Poll<Result<(), std::io::Error>> {
        let this = self.project();
        this.inner.poll_shutdown(cx)
    }
    fn poll_write_vectored(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        bufs: &[std::io::IoSlice<'_>],
    ) -> Poll<Result<usize, std::io::Error>> {
        let this = self.project();
        match this.inner.poll_write_vectored(cx, bufs) {
            Poll::Ready(Ok(n)) => {
                this.progress
                    .downloaded
                    .fetch_add(n as u64, Ordering::SeqCst);
                Poll::Ready(Ok(n))
            }
            a => a,
        }
    }
}
impl<R: AsyncRead> AsyncRead for InstallProgressTracker<R> {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut tokio::io::ReadBuf<'_>,
    ) -> Poll<std::io::Result<()>> {
        let this = self.project();
        let prev = buf.filled().len() as u64;
        match this.inner.poll_read(cx, buf) {
            Poll::Ready(Ok(())) => {
                if *this.validating {
                    &this.progress.validated
                } else {
                    &this.progress.read
                }
                .fetch_add(buf.filled().len() as u64 - prev, Ordering::SeqCst);

                Poll::Ready(Ok(()))
            }
            a => a,
        }
    }
}
impl<R: AsyncSeek> AsyncSeek for InstallProgressTracker<R> {
    fn start_seek(self: Pin<&mut Self>, position: SeekFrom) -> std::io::Result<()> {
        let this = self.project();
        this.inner.start_seek(position)
    }
    fn poll_complete(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<std::io::Result<u64>> {
        let this = self.project();
        match this.inner.poll_complete(cx) {
            Poll::Ready(Ok(n)) => {
                if *this.validating {
                    &this.progress.validated
                } else {
                    &this.progress.read
                }
                .store(n, Ordering::SeqCst);
                Poll::Ready(Ok(n))
            }
            a => a,
        }
    }
}
