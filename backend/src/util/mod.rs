use std::collections::BTreeMap;
use std::future::Future;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::process::Stdio;
use std::str::FromStr;
use std::sync::Arc;

use ::serde::{Deserialize, Deserializer, Serialize, Serializer};
use async_trait::async_trait;
use clap::ArgMatches;
use color_eyre::eyre::{self, eyre};
use digest::Digest;
use fd_lock_rs::FdLock;
use futures::future::BoxFuture;
use futures::FutureExt;
use lazy_static::lazy_static;
use patch_db::{HasModel, Model};
use tokio::fs::File;
use tokio::sync::{Mutex, OwnedMutexGuard, RwLock};
use tokio::task::{JoinError, JoinHandle};
use tracing::instrument;

use crate::shutdown::Shutdown;
use crate::{Error, ResultExt as _};

pub mod io;
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

#[derive(Debug, Clone)]
pub struct Version {
    version: emver::Version,
    string: String,
}
impl Version {
    pub fn as_str(&self) -> &str {
        self.string.as_str()
    }
}
impl std::fmt::Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.string)
    }
}
impl std::str::FromStr for Version {
    type Err = <emver::Version as FromStr>::Err;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Version {
            string: s.to_owned(),
            version: s.parse()?,
        })
    }
}
impl From<emver::Version> for Version {
    fn from(v: emver::Version) -> Self {
        Version {
            string: v.to_string(),
            version: v,
        }
    }
}
impl From<Version> for emver::Version {
    fn from(v: Version) -> Self {
        v.version
    }
}
impl Default for Version {
    fn default() -> Self {
        Self::from(emver::Version::default())
    }
}
impl Deref for Version {
    type Target = emver::Version;
    fn deref(&self) -> &Self::Target {
        &self.version
    }
}
impl AsRef<emver::Version> for Version {
    fn as_ref(&self) -> &emver::Version {
        &self.version
    }
}
impl AsRef<str> for Version {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}
impl PartialEq for Version {
    fn eq(&self, other: &Version) -> bool {
        self.version.eq(&other.version)
    }
}
impl Eq for Version {}
impl PartialOrd for Version {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.version.partial_cmp(&other.version)
    }
}
impl Ord for Version {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.version.cmp(&other.version)
    }
}
impl Hash for Version {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.version.hash(state)
    }
}
impl<'de> Deserialize<'de> for Version {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let string = String::deserialize(deserializer)?;
        let version = emver::Version::from_str(&string).map_err(::serde::de::Error::custom)?;
        Ok(Self { string, version })
    }
}
impl Serialize for Version {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.string.serialize(serializer)
    }
}
impl HasModel for Version {
    type Model = Model<Version>;
}

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

pub struct HashWriter<H: Digest, W: std::io::Write> {
    hasher: H,
    writer: W,
}
impl<H: Digest, W: std::io::Write> HashWriter<H, W> {
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
impl<H: Digest, W: std::io::Write> std::io::Write for HashWriter<H, W> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let written = self.writer.write(buf)?;
        self.hasher.update(&buf[..written]);
        Ok(written)
    }
    fn flush(&mut self) -> std::io::Result<()> {
        self.writer.flush()
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
                .with_ctx(|_| (crate::ErrorKind::Filesystem, path.display().to_string()))?;
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
                    .with_ctx(|_| (crate::ErrorKind::Filesystem, parent.display().to_string()))?
                    .join(file_name));
            }
        }
    }
    tokio::fs::canonicalize(&path)
        .await
        .with_ctx(|_| (crate::ErrorKind::Filesystem, path.display().to_string()))
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
        let path = canonicalize(path.as_ref(), true).await?;
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

pub struct AtomicFile {
    tmp_path: PathBuf,
    path: PathBuf,
    file: File,
}
impl AtomicFile {
    pub async fn new(path: impl AsRef<Path> + Send + Sync) -> Result<Self, Error> {
        let path = canonicalize(&path, true).await?;
        let tmp_path = if let (Some(parent), Some(file_name)) =
            (path.parent(), path.file_name().and_then(|f| f.to_str()))
        {
            parent.join(format!(".{}.tmp", file_name))
        } else {
            return Err(Error::new(
                eyre!("invalid path: {}", path.display()),
                crate::ErrorKind::Filesystem,
            ));
        };
        let file = File::create(&tmp_path)
            .await
            .with_ctx(|_| (crate::ErrorKind::Filesystem, tmp_path.display().to_string()))?;
        Ok(Self {
            tmp_path,
            path,
            file,
        })
    }

    pub async fn save(mut self) -> Result<(), Error> {
        use tokio::io::AsyncWriteExt;
        self.file.flush().await?;
        self.file.shutdown().await?;
        self.file.sync_all().await?;
        tokio::fs::rename(&self.tmp_path, &self.path)
            .await
            .with_ctx(|_| {
                (
                    crate::ErrorKind::Filesystem,
                    format!("mv {} -> {}", self.tmp_path.display(), self.path.display()),
                )
            })
    }
}
impl std::ops::Deref for AtomicFile {
    type Target = File;
    fn deref(&self) -> &Self::Target {
        &self.file
    }
}
impl std::ops::DerefMut for AtomicFile {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.file
    }
}
