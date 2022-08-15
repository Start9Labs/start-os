use std::collections::BTreeMap;
use std::future::Future;
use std::marker::PhantomData;
use std::path::{Path, PathBuf};
use std::pin::Pin;
use std::process::Stdio;
use std::sync::Arc;
use std::task::{Context, Poll};

use async_trait::async_trait;
use clap::ArgMatches;
use color_eyre::eyre::{self, eyre};
use fd_lock_rs::FdLock;
use helpers::canonicalize;
pub use helpers::NonDetachingJoinHandle;
use lazy_static::lazy_static;
pub use models::Version;
use pin_project::pin_project;
use sha2_old::Digest;
use tokio::fs::File;
use tokio::sync::{Mutex, OwnedMutexGuard, RwLock};
use tracing::instrument;

use crate::shutdown::Shutdown;
use crate::{Error, ErrorKind, ResultExt as _};
pub mod config;
pub mod io;
pub mod http_reader;
pub mod logger;
pub mod serde;

#[derive(Clone, Copy, Debug)]
pub enum Never {}
impl Never {}
impl Never {
    pub fn absurd<T>(self) -> T {
        match self {}
    }
}
impl std::fmt::Display for Never {
    fn fmt(&self, _f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.absurd()
    }
}
impl std::error::Error for Never {}

#[async_trait::async_trait]
pub trait Invoke {
    async fn invoke(&mut self, error_kind: crate::ErrorKind) -> Result<Vec<u8>, Error>;
}
#[async_trait::async_trait]
impl Invoke for tokio::process::Command {
    async fn invoke(&mut self, error_kind: crate::ErrorKind) -> Result<Vec<u8>, Error> {
        self.stdout(Stdio::piped());
        self.stderr(Stdio::piped());
        let res = self.output().await?;
        crate::ensure_code!(
            res.status.success(),
            error_kind,
            "{}",
            std::str::from_utf8(&res.stderr).unwrap_or("Unknown Error")
        );
        Ok(res.stdout)
    }
}

pub trait Apply: Sized {
    fn apply<O, F: FnOnce(Self) -> O>(self, func: F) -> O {
        func(self)
    }
}

pub trait ApplyRef {
    fn apply_ref<O, F: FnOnce(&Self) -> O>(&self, func: F) -> O {
        func(&self)
    }

    fn apply_mut<O, F: FnOnce(&mut Self) -> O>(&mut self, func: F) -> O {
        func(self)
    }
}

impl<T> Apply for T {}
impl<T> ApplyRef for T {}

pub async fn daemon<F: FnMut() -> Fut, Fut: Future<Output = ()> + Send + 'static>(
    mut f: F,
    cooldown: std::time::Duration,
    mut shutdown: tokio::sync::broadcast::Receiver<Option<Shutdown>>,
) -> Result<(), eyre::Error> {
    loop {
        tokio::select! {
            _ = shutdown.recv() => return Ok(()),
            _ = tokio::time::sleep(cooldown) => (),
        }
        match tokio::spawn(f()).await {
            Err(e) if e.is_panic() => return Err(eyre!("daemon panicked!")),
            _ => (),
        }
    }
}

pub trait SOption<T> {}
pub struct SSome<T>(T);
impl<T> SSome<T> {
    pub fn into(self) -> T {
        self.0
    }
}
impl<T> From<T> for SSome<T> {
    fn from(t: T) -> Self {
        SSome(t)
    }
}
impl<T> SOption<T> for SSome<T> {}
pub struct SNone<T>(PhantomData<T>);
impl<T> SNone<T> {
    pub fn new() -> Self {
        SNone(PhantomData)
    }
}
impl<T> SOption<T> for SNone<T> {}

#[async_trait]
pub trait AsyncFileExt: Sized {
    async fn maybe_open<P: AsRef<Path> + Send + Sync>(path: P) -> std::io::Result<Option<Self>>;
    async fn delete<P: AsRef<Path> + Send + Sync>(path: P) -> std::io::Result<()>;
}
#[async_trait]
impl AsyncFileExt for File {
    async fn maybe_open<P: AsRef<Path> + Send + Sync>(path: P) -> std::io::Result<Option<Self>> {
        match File::open(path).await {
            Ok(f) => Ok(Some(f)),
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(None),
            Err(e) => Err(e),
        }
    }
    async fn delete<P: AsRef<Path> + Send + Sync>(path: P) -> std::io::Result<()> {
        if let Ok(m) = tokio::fs::metadata(path.as_ref()).await {
            if m.is_dir() {
                tokio::fs::remove_dir_all(path).await
            } else {
                tokio::fs::remove_file(path).await
            }
        } else {
            Ok(())
        }
    }
}

pub struct FmtWriter<W: std::fmt::Write>(W);
impl<W: std::fmt::Write> std::io::Write for FmtWriter<W> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0
            .write_str(
                std::str::from_utf8(buf)
                    .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?,
            )
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;
        Ok(buf.len())
    }
    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

pub fn display_none<T>(_: T, _: &ArgMatches) {
    ()
}

pub struct Container<T>(RwLock<Option<T>>);
impl<T> Container<T> {
    pub fn new(value: Option<T>) -> Self {
        Container(RwLock::new(value))
    }
    pub async fn set(&self, value: T) -> Option<T> {
        std::mem::replace(&mut *self.0.write().await, Some(value))
    }
    pub async fn take(&self) -> Option<T> {
        std::mem::replace(&mut *self.0.write().await, None)
    }
    pub async fn is_empty(&self) -> bool {
        self.0.read().await.is_none()
    }
    pub async fn drop(&self) {
        *self.0.write().await = None;
    }
}

#[pin_project]
pub struct HashWriter<H: Digest, W: tokio::io::AsyncWrite> {
    hasher: H,
    #[pin]
    writer: W,
}
impl<H: Digest, W: tokio::io::AsyncWrite> HashWriter<H, W> {
    pub fn new(hasher: H, writer: W) -> Self {
        HashWriter { hasher, writer }
    }
    pub fn finish(self) -> (H, W) {
        (self.hasher, self.writer)
    }
    pub fn inner(&self) -> &W {
        &self.writer
    }
    pub fn inner_mut(&mut self) -> &mut W {
        &mut self.writer
    }
}
impl<H: Digest, W: tokio::io::AsyncWrite> tokio::io::AsyncWrite for HashWriter<H, W> {
    fn poll_write(
        self: Pin<&mut Self>,
        cx: &mut Context,
        buf: &[u8],
    ) -> Poll<std::io::Result<usize>> {
        let this = self.project();
        let written = tokio::io::AsyncWrite::poll_write(this.writer, cx, &buf);
        match written {
            // only update the hasher once
            Poll::Ready(res) => {
                if let Ok(n) = res {
                    this.hasher.update(&buf[..n]);
                }
                Poll::Ready(res)
            }
            Poll::Pending => Poll::Pending,
        }
    }
    fn poll_flush(self: Pin<&mut Self>, cx: &mut Context) -> Poll<std::io::Result<()>> {
        self.project().writer.poll_flush(cx)
    }

    fn poll_shutdown(self: Pin<&mut Self>, cx: &mut Context) -> Poll<std::io::Result<()>> {
        self.project().writer.poll_shutdown(cx)
    }
}

pub trait IntoDoubleEndedIterator<T>: IntoIterator<Item = T> {
    type IntoIter: Iterator<Item = T> + DoubleEndedIterator;
    fn into_iter(self) -> <Self as IntoDoubleEndedIterator<T>>::IntoIter;
}
impl<T, U> IntoDoubleEndedIterator<U> for T
where
    T: IntoIterator<Item = U>,
    <T as IntoIterator>::IntoIter: DoubleEndedIterator,
{
    type IntoIter = <T as IntoIterator>::IntoIter;
    fn into_iter(self) -> <Self as IntoDoubleEndedIterator<U>>::IntoIter {
        IntoIterator::into_iter(self)
    }
}

pub struct GeneralGuard<F: FnOnce() -> T, T = ()>(Option<F>);
impl<F: FnOnce() -> T, T> GeneralGuard<F, T> {
    pub fn new(f: F) -> Self {
        GeneralGuard(Some(f))
    }

    pub fn drop(mut self) -> T {
        self.0.take().unwrap()()
    }

    pub fn drop_without_action(mut self) {
        self.0 = None;
    }
}

impl<F: FnOnce() -> T, T> Drop for GeneralGuard<F, T> {
    fn drop(&mut self) {
        if let Some(destroy) = self.0.take() {
            destroy();
        }
    }
}

pub struct FileLock(OwnedMutexGuard<()>, Option<FdLock<File>>);
impl Drop for FileLock {
    fn drop(&mut self) {
        if let Some(fd_lock) = self.1.take() {
            tokio::task::spawn_blocking(|| fd_lock.unlock(true).map_err(|(_, e)| e).unwrap());
        }
    }
}
impl FileLock {
    #[instrument(skip(path))]
    pub async fn new(path: impl AsRef<Path> + Send + Sync, blocking: bool) -> Result<Self, Error> {
        lazy_static! {
            static ref INTERNAL_LOCKS: Mutex<BTreeMap<PathBuf, Arc<Mutex<()>>>> =
                Mutex::new(BTreeMap::new());
        }
        let path = canonicalize(path.as_ref(), true)
            .await
            .with_kind(ErrorKind::Filesystem)?;
        let mut internal_locks = INTERNAL_LOCKS.lock().await;
        if !internal_locks.contains_key(&path) {
            internal_locks.insert(path.clone(), Arc::new(Mutex::new(())));
        }
        let tex = internal_locks.get(&path).unwrap().clone();
        drop(internal_locks);
        let tex_guard = if blocking {
            tex.lock_owned().await
        } else {
            tex.try_lock_owned()
                .with_kind(crate::ErrorKind::Filesystem)?
        };
        let parent = path.parent().unwrap_or(Path::new("/"));
        if tokio::fs::metadata(parent).await.is_err() {
            tokio::fs::create_dir_all(parent)
                .await
                .with_ctx(|_| (crate::ErrorKind::Filesystem, parent.display().to_string()))?;
        }
        let f = File::create(&path)
            .await
            .with_ctx(|_| (crate::ErrorKind::Filesystem, path.display().to_string()))?;
        let file_guard = tokio::task::spawn_blocking(move || {
            fd_lock_rs::FdLock::lock(f, fd_lock_rs::LockType::Exclusive, blocking)
        })
        .await
        .with_kind(crate::ErrorKind::Unknown)?
        .with_kind(crate::ErrorKind::Filesystem)?;
        Ok(FileLock(tex_guard, Some(file_guard)))
    }
    pub async fn unlock(mut self) -> Result<(), Error> {
        if let Some(fd_lock) = self.1.take() {
            tokio::task::spawn_blocking(|| fd_lock.unlock(true).map_err(|(_, e)| e))
                .await
                .with_kind(crate::ErrorKind::Unknown)?
                .with_kind(crate::ErrorKind::Filesystem)?;
        }
        Ok(())
    }
}
