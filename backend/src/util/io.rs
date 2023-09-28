use std::future::Future;
use std::io::Cursor;
use std::os::unix::prelude::MetadataExt;
use std::path::Path;
use std::sync::atomic::AtomicU64;
use std::task::Poll;
use std::time::Duration;

use futures::future::{BoxFuture, Fuse};
use futures::{AsyncSeek, FutureExt, TryStreamExt};
use helpers::NonDetachingJoinHandle;
use nix::unistd::{Gid, Uid};
use tokio::io::{
    duplex, AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt, DuplexStream, ReadBuf, WriteHalf,
};
use tokio::net::TcpStream;
use tokio::time::{Instant, Sleep};

use crate::ResultExt;

pub trait AsyncReadSeek: AsyncRead + AsyncSeek {}
impl<T: AsyncRead + AsyncSeek> AsyncReadSeek for T {}

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
        let mut read_buf = ReadBuf::new(buf);
        tokio::io::AsyncRead::poll_read(
            unsafe { self.map_unchecked_mut(|a| &mut a.0) },
            cx,
            &mut read_buf,
        )
        .map(|res| res.map(|_| read_buf.filled().len()))
    }
}
impl<T> tokio::io::AsyncRead for AsyncCompat<T>
where
    T: futures::io::AsyncRead,
{
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut ReadBuf,
    ) -> std::task::Poll<std::io::Result<()>> {
        futures::io::AsyncRead::poll_read(
            unsafe { self.map_unchecked_mut(|a| &mut a.0) },
            cx,
            buf.initialize_unfilled(),
        )
        .map(|res| res.map(|len| buf.set_filled(len)))
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

pub async fn from_yaml_async_reader<T, R>(mut reader: R) -> Result<T, crate::Error>
where
    T: for<'de> serde::Deserialize<'de>,
    R: AsyncRead + Unpin,
{
    let mut buffer = Vec::new();
    reader.read_to_end(&mut buffer).await?;
    serde_yaml::from_slice(&buffer)
        .map_err(color_eyre::eyre::Error::from)
        .with_kind(crate::ErrorKind::Deserialization)
}

pub async fn to_yaml_async_writer<T, W>(mut writer: W, value: &T) -> Result<(), crate::Error>
where
    T: serde::Serialize,
    W: AsyncWrite + Unpin,
{
    let mut buffer = serde_yaml::to_string(value)
        .with_kind(crate::ErrorKind::Serialization)?
        .into_bytes();
    buffer.extend_from_slice(b"\n");
    writer.write_all(&buffer).await?;
    Ok(())
}

pub async fn from_toml_async_reader<T, R>(mut reader: R) -> Result<T, crate::Error>
where
    T: for<'de> serde::Deserialize<'de>,
    R: AsyncRead + Unpin,
{
    let mut buffer = Vec::new();
    reader.read_to_end(&mut buffer).await?;
    serde_toml::from_slice(&buffer)
        .map_err(color_eyre::eyre::Error::from)
        .with_kind(crate::ErrorKind::Deserialization)
}

pub async fn to_toml_async_writer<T, W>(mut writer: W, value: &T) -> Result<(), crate::Error>
where
    T: serde::Serialize,
    W: AsyncWrite + Unpin,
{
    let mut buffer = serde_toml::to_vec(value).with_kind(crate::ErrorKind::Serialization)?;
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
    serde_cbor::de::from_reader(buffer.as_slice())
        .map_err(color_eyre::eyre::Error::from)
        .with_kind(crate::ErrorKind::Deserialization)
}
pub async fn to_cbor_async_writer<T, W>(mut writer: W, value: &T) -> Result<(), crate::Error>
where
    T: serde::Serialize,
    W: AsyncWrite + Unpin,
{
    let mut buffer = Vec::new();
    serde_cbor::ser::into_writer(value, &mut buffer).with_kind(crate::ErrorKind::Serialization)?;
    buffer.extend_from_slice(b"\n");
    writer.write_all(&buffer).await?;
    Ok(())
}

pub async fn from_json_async_reader<T, R>(mut reader: R) -> Result<T, crate::Error>
where
    T: for<'de> serde::Deserialize<'de>,
    R: AsyncRead + Unpin,
{
    let mut buffer = Vec::new();
    reader.read_to_end(&mut buffer).await?;
    serde_json::from_slice(&buffer)
        .map_err(color_eyre::eyre::Error::from)
        .with_kind(crate::ErrorKind::Deserialization)
}

pub async fn to_json_async_writer<T, W>(mut writer: W, value: &T) -> Result<(), crate::Error>
where
    T: serde::Serialize,
    W: AsyncWrite + Unpin,
{
    let buffer = serde_json::to_string(value).with_kind(crate::ErrorKind::Serialization)?;
    writer.write_all(&buffer.as_bytes()).await?;
    Ok(())
}

pub async fn to_json_pretty_async_writer<T, W>(mut writer: W, value: &T) -> Result<(), crate::Error>
where
    T: serde::Serialize,
    W: AsyncWrite + Unpin,
{
    let mut buffer =
        serde_json::to_string_pretty(value).with_kind(crate::ErrorKind::Serialization)?;
    buffer.push_str("\n");
    writer.write_all(&buffer.as_bytes()).await?;
    Ok(())
}

pub async fn copy_and_shutdown<R: AsyncRead + Unpin, W: AsyncWrite + Unpin>(
    r: &mut R,
    mut w: W,
) -> Result<(), std::io::Error> {
    tokio::io::copy(r, &mut w).await?;
    w.flush().await?;
    w.shutdown().await?;
    Ok(())
}

pub fn dir_size<'a, P: AsRef<Path> + 'a + Send + Sync>(
    path: P,
    ctr: Option<&'a Counter>,
) -> BoxFuture<'a, Result<u64, std::io::Error>> {
    async move {
        tokio_stream::wrappers::ReadDirStream::new(tokio::fs::read_dir(path.as_ref()).await?)
            .try_fold(0, |acc, e| async move {
                let m = e.metadata().await?;
                Ok(acc
                    + if m.is_file() {
                        if let Some(ctr) = ctr {
                            ctr.add(m.len());
                        }
                        m.len()
                    } else if m.is_dir() {
                        dir_size(e.path(), ctr).await?
                    } else {
                        0
                    })
            })
            .await
    }
    .boxed()
}

pub fn response_to_reader(response: reqwest::Response) -> impl AsyncRead + Unpin {
    tokio_util::io::StreamReader::new(response.bytes_stream().map_err(|e| {
        std::io::Error::new(
            if e.is_connect() {
                std::io::ErrorKind::ConnectionRefused
            } else if e.is_timeout() {
                std::io::ErrorKind::TimedOut
            } else {
                std::io::ErrorKind::Other
            },
            e,
        )
    }))
}

#[pin_project::pin_project]
pub struct BufferedWriteReader {
    #[pin]
    hdl: Fuse<NonDetachingJoinHandle<Result<(), std::io::Error>>>,
    #[pin]
    rdr: DuplexStream,
}
impl BufferedWriteReader {
    pub fn new<
        W: FnOnce(WriteHalf<DuplexStream>) -> Fut,
        Fut: Future<Output = Result<(), std::io::Error>> + Send + Sync + 'static,
    >(
        write_fn: W,
        max_buf_size: usize,
    ) -> Self {
        let (w, rdr) = duplex(max_buf_size);
        let (_, w) = tokio::io::split(w);
        BufferedWriteReader {
            hdl: NonDetachingJoinHandle::from(tokio::spawn(write_fn(w))).fuse(),
            rdr,
        }
    }
}
impl AsyncRead for BufferedWriteReader {
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> std::task::Poll<std::io::Result<()>> {
        let this = self.project();
        let res = this.rdr.poll_read(cx, buf);
        match this.hdl.poll(cx) {
            Poll::Ready(Ok(Err(e))) => return Poll::Ready(Err(e)),
            Poll::Ready(Err(e)) => {
                return Poll::Ready(Err(std::io::Error::new(std::io::ErrorKind::BrokenPipe, e)))
            }
            _ => res,
        }
    }
}

pub trait CursorExt {
    fn pure_read(&mut self, buf: &mut ReadBuf<'_>);
}

impl<T: AsRef<[u8]>> CursorExt for Cursor<T> {
    fn pure_read(&mut self, buf: &mut ReadBuf<'_>) {
        let end = self.position() as usize
            + std::cmp::min(
                buf.remaining(),
                self.get_ref().as_ref().len() - self.position() as usize,
            );
        buf.put_slice(&self.get_ref().as_ref()[self.position() as usize..end]);
        self.set_position(end as u64);
    }
}

#[pin_project::pin_project]
#[derive(Debug)]
pub struct BackTrackingReader<T> {
    #[pin]
    reader: T,
    buffer: Cursor<Vec<u8>>,
    buffering: bool,
}
impl<T> BackTrackingReader<T> {
    pub fn new(reader: T) -> Self {
        Self {
            reader,
            buffer: Cursor::new(Vec::new()),
            buffering: false,
        }
    }
    pub fn start_buffering(&mut self) {
        self.buffer.set_position(0);
        self.buffer.get_mut().truncate(0);
        self.buffering = true;
    }
    pub fn stop_buffering(&mut self) {
        self.buffer.set_position(0);
        self.buffer.get_mut().truncate(0);
        self.buffering = false;
    }
    pub fn rewind(&mut self) {
        self.buffering = false;
    }
    pub fn unwrap(self) -> T {
        self.reader
    }
}

impl<T: AsyncRead> AsyncRead for BackTrackingReader<T> {
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> Poll<std::io::Result<()>> {
        let this = self.project();
        if *this.buffering {
            let filled = buf.filled().len();
            let res = this.reader.poll_read(cx, buf);
            this.buffer
                .get_mut()
                .extend_from_slice(&buf.filled()[filled..]);
            res
        } else {
            let mut ready = false;
            if (this.buffer.position() as usize) < this.buffer.get_ref().len() {
                this.buffer.pure_read(buf);
                ready = true;
            }
            if buf.remaining() > 0 {
                match this.reader.poll_read(cx, buf) {
                    Poll::Pending => {
                        if ready {
                            Poll::Ready(Ok(()))
                        } else {
                            Poll::Pending
                        }
                    }
                    a => a,
                }
            } else {
                Poll::Ready(Ok(()))
            }
        }
    }
}

impl<T: AsyncWrite> AsyncWrite for BackTrackingReader<T> {
    fn is_write_vectored(&self) -> bool {
        self.reader.is_write_vectored()
    }
    fn poll_flush(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Result<(), std::io::Error>> {
        self.project().reader.poll_flush(cx)
    }
    fn poll_shutdown(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Result<(), std::io::Error>> {
        self.project().reader.poll_shutdown(cx)
    }
    fn poll_write(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> Poll<Result<usize, std::io::Error>> {
        self.project().reader.poll_write(cx, buf)
    }
    fn poll_write_vectored(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        bufs: &[std::io::IoSlice<'_>],
    ) -> Poll<Result<usize, std::io::Error>> {
        self.project().reader.poll_write_vectored(cx, bufs)
    }
}

pub struct Counter {
    atomic: AtomicU64,
    ordering: std::sync::atomic::Ordering,
}
impl Counter {
    pub fn new(init: u64, ordering: std::sync::atomic::Ordering) -> Self {
        Self {
            atomic: AtomicU64::new(init),
            ordering,
        }
    }
    pub fn load(&self) -> u64 {
        self.atomic.load(self.ordering)
    }
    pub fn add(&self, value: u64) {
        self.atomic.fetch_add(value, self.ordering);
    }
}

#[pin_project::pin_project]
pub struct CountingReader<'a, R> {
    ctr: &'a Counter,
    #[pin]
    rdr: R,
}
impl<'a, R> CountingReader<'a, R> {
    pub fn new(rdr: R, ctr: &'a Counter) -> Self {
        Self { ctr, rdr }
    }
    pub fn into_inner(self) -> R {
        self.rdr
    }
}
impl<'a, R: AsyncRead> AsyncRead for CountingReader<'a, R> {
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> Poll<std::io::Result<()>> {
        let this = self.project();
        let start = buf.filled().len();
        let res = this.rdr.poll_read(cx, buf);
        let len = buf.filled().len() - start;
        if len > 0 {
            this.ctr.add(len as u64);
        }
        res
    }
}

pub fn dir_copy<'a, P0: AsRef<Path> + 'a + Send + Sync, P1: AsRef<Path> + 'a + Send + Sync>(
    src: P0,
    dst: P1,
    ctr: Option<&'a Counter>,
) -> BoxFuture<'a, Result<(), crate::Error>> {
    async move {
        let m = tokio::fs::metadata(&src).await?;
        let dst_path = dst.as_ref();
        tokio::fs::create_dir_all(&dst_path).await.with_ctx(|_| {
            (
                crate::ErrorKind::Filesystem,
                format!("mkdir {}", dst_path.display()),
            )
        })?;
        tokio::fs::set_permissions(&dst_path, m.permissions())
            .await
            .with_ctx(|_| {
                (
                    crate::ErrorKind::Filesystem,
                    format!("chmod {}", dst_path.display()),
                )
            })?;
        let tmp_dst_path = dst_path.to_owned();
        tokio::task::spawn_blocking(move || {
            nix::unistd::chown(
                &tmp_dst_path,
                Some(Uid::from_raw(m.uid())),
                Some(Gid::from_raw(m.gid())),
            )
        })
        .await
        .with_kind(crate::ErrorKind::Unknown)?
        .with_ctx(|_| {
            (
                crate::ErrorKind::Filesystem,
                format!("chown {}", dst_path.display()),
            )
        })?;
        tokio_stream::wrappers::ReadDirStream::new(tokio::fs::read_dir(src.as_ref()).await?)
            .map_err(|e| crate::Error::new(e, crate::ErrorKind::Filesystem))
            .try_for_each(|e| async move {
                let m = e.metadata().await?;
                let src_path = e.path();
                let dst_path = dst_path.join(e.file_name());
                if m.is_file() {
                    let mut dst_file = tokio::fs::File::create(&dst_path).await.with_ctx(|_| {
                        (
                            crate::ErrorKind::Filesystem,
                            format!("create {}", dst_path.display()),
                        )
                    })?;
                    let mut rdr = tokio::fs::File::open(&src_path).await.with_ctx(|_| {
                        (
                            crate::ErrorKind::Filesystem,
                            format!("open {}", src_path.display()),
                        )
                    })?;
                    if let Some(ctr) = ctr {
                        tokio::io::copy(&mut CountingReader::new(rdr, ctr), &mut dst_file).await
                    } else {
                        tokio::io::copy(&mut rdr, &mut dst_file).await
                    }
                    .with_ctx(|_| {
                        (
                            crate::ErrorKind::Filesystem,
                            format!("cp {} -> {}", src_path.display(), dst_path.display()),
                        )
                    })?;
                    dst_file.flush().await?;
                    dst_file.shutdown().await?;
                    dst_file.sync_all().await?;
                    drop(dst_file);
                    let tmp_dst_path = dst_path.clone();
                    tokio::task::spawn_blocking(move || {
                        nix::unistd::chown(
                            &tmp_dst_path,
                            Some(Uid::from_raw(m.uid())),
                            Some(Gid::from_raw(m.gid())),
                        )
                    })
                    .await
                    .with_kind(crate::ErrorKind::Unknown)?
                    .with_ctx(|_| {
                        (
                            crate::ErrorKind::Filesystem,
                            format!("chown {}", dst_path.display()),
                        )
                    })?;
                } else if m.is_dir() {
                    dir_copy(src_path, dst_path, ctr).await?;
                } else if m.file_type().is_symlink() {
                    tokio::fs::symlink(
                        tokio::fs::read_link(&src_path).await.with_ctx(|_| {
                            (
                                crate::ErrorKind::Filesystem,
                                format!("readlink {}", src_path.display()),
                            )
                        })?,
                        &dst_path,
                    )
                    .await
                    .with_ctx(|_| {
                        (
                            crate::ErrorKind::Filesystem,
                            format!("cp -P {} -> {}", src_path.display(), dst_path.display()),
                        )
                    })?;
                    // Do not set permissions (see https://unix.stackexchange.com/questions/87200/change-permissions-for-a-symbolic-link)
                }
                Ok(())
            })
            .await?;
        Ok(())
    }
    .boxed()
}

#[pin_project::pin_project]
pub struct TimeoutStream<S: AsyncRead + AsyncWrite = TcpStream> {
    timeout: Duration,
    #[pin]
    sleep: Sleep,
    #[pin]
    stream: S,
}
impl<S: AsyncRead + AsyncWrite> TimeoutStream<S> {
    pub fn new(stream: S, timeout: Duration) -> Self {
        Self {
            timeout,
            sleep: tokio::time::sleep(timeout),
            stream,
        }
    }
}
impl<S: AsyncRead + AsyncWrite> AsyncRead for TimeoutStream<S> {
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut tokio::io::ReadBuf<'_>,
    ) -> std::task::Poll<std::io::Result<()>> {
        let mut this = self.project();
        if let std::task::Poll::Ready(_) = this.sleep.as_mut().poll(cx) {
            return std::task::Poll::Ready(Err(std::io::Error::new(
                std::io::ErrorKind::TimedOut,
                "timed out",
            )));
        }
        let res = this.stream.poll_read(cx, buf);
        if res.is_ready() {
            this.sleep.reset(Instant::now() + *this.timeout);
        }
        res
    }
}
impl<S: AsyncRead + AsyncWrite> AsyncWrite for TimeoutStream<S> {
    fn poll_write(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> std::task::Poll<Result<usize, std::io::Error>> {
        let this = self.project();
        let res = this.stream.poll_write(cx, buf);
        if res.is_ready() {
            this.sleep.reset(Instant::now() + *this.timeout);
        }
        res
    }
    fn poll_flush(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), std::io::Error>> {
        let this = self.project();
        let res = this.stream.poll_flush(cx);
        if res.is_ready() {
            this.sleep.reset(Instant::now() + *this.timeout);
        }
        res
    }
    fn poll_shutdown(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), std::io::Error>> {
        let this = self.project();
        let res = this.stream.poll_shutdown(cx);
        if res.is_ready() {
            this.sleep.reset(Instant::now() + *this.timeout);
        }
        res
    }
}
