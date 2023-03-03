use std::io::SeekFrom;
use std::pin::Pin;
use std::task::{Context, Poll};
use std::time::{Duration, Instant};

use patch_db::HasModel;
use serde::{Deserialize, Serialize};
use tokio::io::{AsyncRead, AsyncSeek, AsyncWrite};
use tokio::sync::watch;

use crate::prelude::*;
use crate::util::unlikely;

#[derive(Clone, Copy, Debug, Deserialize, Serialize, HasModel, Default)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct InstallProgress {
    pub size: Option<u64>,
    pub downloaded: u64,
    pub download_complete: bool,
    pub validated: u64,
    pub validation_complete: bool,
    pub unpacked: u64,
    pub unpack_complete: bool,
}
impl InstallProgress {
    pub fn new(size: Option<u64>) -> Self {
        InstallProgress {
            size,
            downloaded: 0,
            download_complete: false,
            validated: 0,
            validation_complete: false,
            unpacked: 0,
            unpack_complete: false,
        }
    }
}

#[pin_project::pin_project]
pub struct InstallProgressTracker<RW> {
    #[pin]
    inner: RW,
    interval: Duration,
    last_update: Instant,
    buffered: u64,
    progress: watch::Sender<InstallProgress>,
}
impl<RW> InstallProgressTracker<RW> {
    pub fn new(inner: RW, interval: Duration, size: Option<u64>) -> Self {
        InstallProgressTracker {
            inner,
            interval,
            last_update: Instant::now(),
            buffered: 0,
            progress: watch::channel(InstallProgress::new(size)).0,
        }
    }
    pub fn subscribe(&self) -> watch::Receiver<InstallProgress> {
        self.progress.subscribe()
    }
    pub fn downloaded(&mut self) {
        self.progress.send_modify(|a| {
            a.downloaded += std::mem::take(&mut self.buffered);
            a.download_complete = true
        })
    }
    pub fn validated(&mut self) {
        self.progress.send_modify(|a| {
            a.validated += std::mem::take(&mut self.buffered);
            a.validation_complete = true
        })
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
                *this.buffered += n as u64;
                if unlikely(this.last_update.elapsed() > *this.interval) {
                    let buffered = std::mem::take(this.buffered);
                    this.progress.send_modify(|a| a.downloaded += buffered);
                    *this.last_update = Instant::now();
                }
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
                *this.buffered += n as u64;
                if unlikely(this.last_update.elapsed() > *this.interval) {
                    let buffered = std::mem::take(this.buffered);
                    this.progress.send_modify(|a| a.downloaded += buffered);
                    *this.last_update = Instant::now();
                }
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
                *this.buffered += buf.filled().len() as u64 - prev;
                if unlikely(this.last_update.elapsed() > *this.interval) {
                    let buffered = std::mem::take(this.buffered);
                    this.progress.send_modify(|a| {
                        if a.validation_complete {
                            a.unpacked += buffered
                        } else {
                            a.validated += buffered
                        }
                    });
                    *this.last_update = Instant::now();
                }

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
                this.progress.send_modify(|a| {
                    if a.validation_complete {
                        a.unpacked = n
                    } else {
                        a.validated = n
                    }
                });
                *this.last_update = Instant::now();
                Poll::Ready(Ok(n))
            }
            a => a,
        }
    }
}
