use std::fmt;
use std::marker::PhantomData;
use std::path::{Path, PathBuf};

use failure::ResultExt as _;
use file_lock::FileLock;
use tokio::fs::File;
use tokio::io::{AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt};

use crate::Error;
use crate::ResultExt as _;

#[derive(Debug, Clone)]
pub struct PersistencePath(PathBuf);
impl PersistencePath {
    pub fn from_ref<P: AsRef<Path>>(p: P) -> Self {
        let path = p.as_ref();
        PersistencePath(if path.has_root() {
            path.strip_prefix("/").unwrap().to_owned()
        } else {
            path.to_owned()
        })
    }

    pub fn new(path: PathBuf) -> Self {
        PersistencePath(if path.has_root() {
            path.strip_prefix("/").unwrap().to_owned()
        } else {
            path.to_owned()
        })
    }

    pub fn join<P: AsRef<Path>>(&self, path: P) -> Self {
        PersistencePath::new(self.0.join(path))
    }

    pub fn tmp(&self) -> PathBuf {
        Path::new(crate::TMP_DIR).join(&self.0)
    }

    pub fn path(&self) -> PathBuf {
        Path::new(crate::PERSISTENCE_DIR).join(&self.0)
    }

    pub async fn lock(&self, for_update: bool) -> Result<FileLock, Error> {
        let path = self.path();
        let lock_path = format!("{}.lock", path.display());
        if tokio::fs::metadata(Path::new(&lock_path)).await.is_err() {
            // !exists
            tokio::fs::File::create(&lock_path)
                .await
                .with_context(|e| format!("{}: {}", lock_path, e))
                .with_code(crate::error::FILESYSTEM_ERROR)?;
        }
        let lock = lock_file(lock_path.clone(), for_update)
            .await
            .with_context(|e| format!("{}: {}", lock_path, e))
            .with_code(crate::error::FILESYSTEM_ERROR)?;
        Ok(lock)
    }

    pub async fn exists(&self) -> bool {
        tokio::fs::metadata(self.path()).await.is_ok()
    }

    pub async fn maybe_read(&self, for_update: bool) -> Option<Result<PersistenceFile, Error>> {
        if self.exists().await {
            // exists
            Some(self.read(for_update).await)
        } else {
            None
        }
    }

    pub async fn read(&self, for_update: bool) -> Result<PersistenceFile, Error> {
        let path = self.path();
        let lock = self.lock(for_update).await?;
        let file = File::open(&path)
            .await
            .with_context(|e| format!("{}: {}", path.display(), e))
            .with_code(crate::error::FILESYSTEM_ERROR)?;
        Ok(PersistenceFile::new(file, lock, None))
    }

    pub async fn write(&self, lock: Option<FileLock>) -> Result<PersistenceFile, Error> {
        let path = self.path();
        if let Some(parent) = path.parent() {
            if tokio::fs::metadata(parent).await.is_err() {
                // !exists
                tokio::fs::create_dir_all(parent).await?;
            }
        }
        let lock = if let Some(lock) = lock {
            lock
        } else {
            self.lock(true).await?
        };
        Ok({
            let path = self.tmp();
            if let Some(parent) = path.parent() {
                if tokio::fs::metadata(parent).await.is_err() {
                    // !exists
                    tokio::fs::create_dir_all(parent).await?;
                }
            }
            PersistenceFile::new(File::create(path).await?, lock, Some(self.clone()))
        })
    }

    pub async fn for_update(self) -> Result<UpdateHandle<ForRead>, Error> {
        UpdateHandle::new(self).await
    }
}

#[derive(Debug)]
pub struct PersistenceFile {
    file: Option<File>,
    lock: Option<FileLock>,
    needs_commit: Option<PersistencePath>,
}
impl PersistenceFile {
    pub fn new(file: File, lock: FileLock, needs_commit: Option<PersistencePath>) -> Self {
        PersistenceFile {
            file: Some(file),
            lock: Some(lock),
            needs_commit,
        }
    }

    pub fn take_lock(&mut self) -> Option<FileLock> {
        self.lock.take()
    }

    /// Commits the file to the persistence directory.
    /// If this fails, the file was not saved.
    pub async fn commit(mut self) -> Result<(), Error> {
        if let Some(mut file) = self.file.take() {
            file.flush().await?;
            file.shutdown().await?;
            drop(file);
        }
        if let Some(path) = self.needs_commit.take() {
            tokio::fs::rename(path.tmp(), path.path())
                .await
                .with_context(|e| {
                    format!(
                        "{} -> {}: {}",
                        path.tmp().display(),
                        path.path().display(),
                        e
                    )
                })
                .with_code(crate::error::FILESYSTEM_ERROR)?;
            if let Some(lock) = self.lock.take() {
                unlock(lock)
                    .await
                    .with_context(|e| format!("{}.lock: {}", path.path().display(), e))
                    .with_code(crate::error::FILESYSTEM_ERROR)?;
                tokio::fs::remove_file(format!("{}.lock", path.path().display()))
                    .await
                    .with_context(|e| format!("{}.lock: {}", path.path().display(), e))
                    .with_code(crate::error::FILESYSTEM_ERROR)?;
            }

            Ok(())
        } else {
            Ok(())
        }
    }
}
impl std::ops::Deref for PersistenceFile {
    type Target = File;

    fn deref(&self) -> &Self::Target {
        self.file.as_ref().unwrap()
    }
}
impl std::ops::DerefMut for PersistenceFile {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.file.as_mut().unwrap()
    }
}
impl AsRef<File> for PersistenceFile {
    fn as_ref(&self) -> &File {
        &*self
    }
}
impl AsMut<File> for PersistenceFile {
    fn as_mut(&mut self) -> &mut File {
        &mut *self
    }
}
impl Drop for PersistenceFile {
    fn drop(&mut self) {
        if let Some(path) = &self.needs_commit {
            log::warn!(
                "{} was dropped without being committed.",
                path.path().display()
            );
        }
    }
}

pub trait UpdateHandleMode {}
pub struct ForRead;
impl UpdateHandleMode for ForRead {}
pub struct ForWrite;
impl UpdateHandleMode for ForWrite {}

pub struct UpdateHandle<Mode: UpdateHandleMode> {
    path: PersistencePath,
    file: PersistenceFile,
    mode: PhantomData<Mode>,
}
impl UpdateHandle<ForRead> {
    pub async fn new(path: PersistencePath) -> Result<Self, Error> {
        if !path.path().exists() {
            tokio::fs::File::create(path.path()).await?;
        }
        Ok(UpdateHandle {
            file: path.read(true).await?,
            path,
            mode: PhantomData,
        })
    }

    pub async fn into_writer(mut self) -> Result<UpdateHandle<ForWrite>, Error> {
        let lock = self.file.take_lock();
        Ok(UpdateHandle {
            file: self.path.write(lock).await?,
            path: self.path,
            mode: PhantomData,
        })
    }
}

impl UpdateHandle<ForWrite> {
    pub async fn commit(self) -> Result<(), Error> {
        self.file.commit().await
    }
}

impl tokio::io::AsyncRead for UpdateHandle<ForRead> {
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut [u8],
    ) -> std::task::Poll<std::io::Result<usize>> {
        unsafe { self.map_unchecked_mut(|a| a.file.file.as_mut().unwrap()) }.poll_read(cx, buf)
    }
}

impl tokio::io::AsyncWrite for UpdateHandle<ForWrite> {
    fn poll_write(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> std::task::Poll<std::io::Result<usize>> {
        tokio::io::AsyncWrite::poll_write(
            unsafe { self.map_unchecked_mut(|a| a.file.file.as_mut().unwrap()) },
            cx,
            buf,
        )
    }
    fn poll_flush(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<std::io::Result<()>> {
        tokio::io::AsyncWrite::poll_flush(
            unsafe { self.map_unchecked_mut(|a| a.file.file.as_mut().unwrap()) },
            cx,
        )
    }
    fn poll_shutdown(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<std::io::Result<()>> {
        tokio::io::AsyncWrite::poll_shutdown(
            unsafe { self.map_unchecked_mut(|a| a.file.file.as_mut().unwrap()) },
            cx,
        )
    }
}

pub struct YamlUpdateHandle<T: serde::Serialize + for<'de> serde::Deserialize<'de>> {
    inner: T,
    handle: UpdateHandle<ForRead>,
    committed: bool,
}
impl<T> YamlUpdateHandle<T>
where
    T: serde::Serialize + for<'de> serde::Deserialize<'de>,
{
    pub async fn new(path: PersistencePath) -> Result<Self, Error> {
        let mut handle = path.for_update().await?;
        let inner = from_yaml_async_reader(&mut handle).await?;
        Ok(YamlUpdateHandle {
            inner,
            handle,
            committed: false,
        })
    }

    pub async fn commit(mut self) -> Result<(), Error> {
        let mut file = self.handle.into_writer().await?;
        to_yaml_async_writer(&mut file, &self.inner)
            .await
            .no_code()?;
        file.commit().await?;
        self.committed = true;
        Ok(())
    }
}

impl<T> YamlUpdateHandle<T>
where
    T: serde::Serialize + for<'de> serde::Deserialize<'de> + Default,
{
    pub async fn new_or_default(path: PersistencePath) -> Result<Self, Error> {
        if !path.path().exists() {
            Ok(YamlUpdateHandle {
                inner: Default::default(),
                handle: path.for_update().await?,
                committed: false,
            })
        } else {
            Self::new(path).await
        }
    }
}

impl<T> std::ops::Deref for YamlUpdateHandle<T>
where
    T: serde::Serialize + for<'de> serde::Deserialize<'de> + Default,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
impl<T> std::ops::DerefMut for YamlUpdateHandle<T>
where
    T: serde::Serialize + for<'de> serde::Deserialize<'de> + Default,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

#[derive(Clone, Debug)]
pub enum Never {}
pub fn absurd<T>(lol: Never) -> T {
    match lol {}
}
impl fmt::Display for Never {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        absurd(self.clone())
    }
}
impl failure::Fail for Never {}

#[derive(Clone, Debug)]
pub struct AsyncCompat<T>(pub T);
impl<T> futures::io::AsyncRead for AsyncCompat<T>
where
    T: tokio::io::AsyncRead,
{
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut [u8],
    ) -> std::task::Poll<std::io::Result<usize>> {
        tokio::io::AsyncRead::poll_read(unsafe { self.map_unchecked_mut(|a| &mut a.0) }, cx, buf)
    }
}
impl<T> tokio::io::AsyncRead for AsyncCompat<T>
where
    T: futures::io::AsyncRead,
{
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut [u8],
    ) -> std::task::Poll<std::io::Result<usize>> {
        futures::io::AsyncRead::poll_read(unsafe { self.map_unchecked_mut(|a| &mut a.0) }, cx, buf)
    }
}
impl<T> futures::io::AsyncWrite for AsyncCompat<T>
where
    T: tokio::io::AsyncWrite,
{
    fn poll_write(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> std::task::Poll<std::io::Result<usize>> {
        tokio::io::AsyncWrite::poll_write(unsafe { self.map_unchecked_mut(|a| &mut a.0) }, cx, buf)
    }
    fn poll_flush(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<std::io::Result<()>> {
        tokio::io::AsyncWrite::poll_flush(unsafe { self.map_unchecked_mut(|a| &mut a.0) }, cx)
    }
    fn poll_close(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<std::io::Result<()>> {
        tokio::io::AsyncWrite::poll_shutdown(unsafe { self.map_unchecked_mut(|a| &mut a.0) }, cx)
    }
}
impl<T> tokio::io::AsyncWrite for AsyncCompat<T>
where
    T: futures::io::AsyncWrite,
{
    fn poll_write(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> std::task::Poll<std::io::Result<usize>> {
        futures::io::AsyncWrite::poll_write(
            unsafe { self.map_unchecked_mut(|a| &mut a.0) },
            cx,
            buf,
        )
    }
    fn poll_flush(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<std::io::Result<()>> {
        futures::io::AsyncWrite::poll_flush(unsafe { self.map_unchecked_mut(|a| &mut a.0) }, cx)
    }
    fn poll_shutdown(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<std::io::Result<()>> {
        futures::io::AsyncWrite::poll_close(unsafe { self.map_unchecked_mut(|a| &mut a.0) }, cx)
    }
}

pub async fn lock_file(filename: String, for_write: bool) -> std::io::Result<FileLock> {
    tokio::task::spawn_blocking(move || FileLock::lock(&filename, true, for_write)).await?
}

pub async fn unlock(lock: FileLock) -> std::io::Result<()> {
    tokio::task::spawn_blocking(move || lock.unlock()).await?
}

pub async fn from_yaml_async_reader<T, R>(mut reader: R) -> Result<T, crate::Error>
where
    T: for<'de> serde::Deserialize<'de>,
    R: AsyncRead + Unpin,
{
    let mut buffer = Vec::new();
    reader.read_to_end(&mut buffer).await?;
    serde_yaml::from_slice(&buffer)
        .map_err(failure::Error::from)
        .with_code(crate::error::SERDE_ERROR)
}

pub async fn to_yaml_async_writer<T, W>(mut writer: W, value: &T) -> Result<(), crate::Error>
where
    T: serde::Serialize,
    W: AsyncWrite + Unpin,
{
    let mut buffer = serde_yaml::to_vec(value).with_code(crate::error::SERDE_ERROR)?;
    buffer.extend_from_slice(b"\n");
    writer.write_all(&buffer).await?;
    Ok(())
}

pub async fn from_cbor_async_reader<T, R>(mut reader: R) -> Result<T, crate::Error>
where
    T: for<'de> serde::Deserialize<'de>,
    R: AsyncRead + Unpin,
{
    let mut buffer = Vec::new();
    reader.read_to_end(&mut buffer).await?;
    serde_cbor::from_slice(&buffer)
        .map_err(failure::Error::from)
        .with_code(crate::error::SERDE_ERROR)
}

pub async fn from_json_async_reader<T, R>(mut reader: R) -> Result<T, crate::Error>
where
    T: for<'de> serde::Deserialize<'de>,
    R: AsyncRead + Unpin,
{
    let mut buffer = Vec::new();
    reader.read_to_end(&mut buffer).await?;
    serde_json::from_slice(&buffer)
        .map_err(failure::Error::from)
        .with_code(crate::error::SERDE_ERROR)
}

pub async fn to_json_async_writer<T, W>(mut writer: W, value: &T) -> Result<(), crate::Error>
where
    T: serde::Serialize,
    W: AsyncWrite + Unpin,
{
    let buffer = serde_json::to_string(value).with_code(crate::error::SERDE_ERROR)?;
    writer.write_all(&buffer.as_bytes()).await?;
    Ok(())
}

pub async fn to_json_pretty_async_writer<T, W>(mut writer: W, value: &T) -> Result<(), crate::Error>
where
    T: serde::Serialize,
    W: AsyncWrite + Unpin,
{
    let mut buffer = serde_json::to_string_pretty(value).with_code(crate::error::SERDE_ERROR)?;
    buffer.push_str("\n");
    writer.write_all(&buffer.as_bytes()).await?;
    Ok(())
}

#[async_trait::async_trait]
pub trait Invoke {
    async fn invoke(&mut self, name: &str) -> Result<Vec<u8>, failure::Error>;
}
#[async_trait::async_trait]
impl Invoke for tokio::process::Command {
    async fn invoke(&mut self, name: &str) -> Result<Vec<u8>, failure::Error> {
        let res = self.output().await?;
        ensure!(
            res.status.success(),
            "{} Error: {}",
            name,
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
