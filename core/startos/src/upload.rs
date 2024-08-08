use std::io::SeekFrom;
use std::pin::Pin;
use std::sync::Arc;
use std::task::Poll;
use std::time::Duration;

use axum::body::Body;
use axum::extract::Request;
use axum::response::Response;
use bytes::Bytes;
use futures::{ready, FutureExt, Stream, StreamExt};
use http::header::CONTENT_LENGTH;
use http::{HeaderMap, StatusCode};
use imbl_value::InternedString;
use tokio::fs::File;
use tokio::io::{AsyncRead, AsyncSeek, AsyncSeekExt, AsyncWrite, AsyncWriteExt};
use tokio::sync::watch;

use crate::context::RpcContext;
use crate::prelude::*;
use crate::rpc_continuations::{Guid, RpcContinuation};
use crate::s9pk::merkle_archive::source::multi_cursor_file::{FileCursor, MultiCursorFile};
use crate::s9pk::merkle_archive::source::ArchiveSource;
use crate::util::io::{create_file, TmpDir};

pub async fn upload(
    ctx: &RpcContext,
    session: InternedString,
) -> Result<(Guid, UploadingFile), Error> {
    let guid = Guid::new();
    let (mut handle, file) = UploadingFile::new().await?;
    ctx.rpc_continuations
        .add(
            guid.clone(),
            RpcContinuation::rest_authed(
                ctx,
                session,
                |request| async move {
                    handle.upload(request).await;

                    Response::builder()
                        .status(StatusCode::NO_CONTENT)
                        .body(Body::empty())
                        .with_kind(ErrorKind::Network)
                },
                Duration::from_secs(30),
            ),
        )
        .await;
    Ok((guid, file))
}

#[derive(Default)]
struct Progress {
    expected_size: Option<u64>,
    written: u64,
    error: Option<Error>,
}
impl Progress {
    fn handle_error(&mut self, e: &std::io::Error) -> bool {
        if self.error.is_none() {
            self.error = Some(Error::new(eyre!("{e}"), ErrorKind::Network));
            true
        } else {
            false
        }
    }
    fn handle_write(&mut self, res: &std::io::Result<usize>) -> bool {
        match res {
            Ok(a) => {
                self.written += *a as u64;
                true
            }
            Err(e) => self.handle_error(e),
        }
    }
    async fn expected_size(watch: &mut watch::Receiver<Self>) -> Option<u64> {
        watch
            .wait_for(|progress| progress.error.is_some() || progress.expected_size.is_some())
            .await
            .ok()
            .and_then(|a| a.expected_size)
    }
    async fn ready_for(watch: &mut watch::Receiver<Self>, size: u64) -> Result<(), Error> {
        match &*watch
            .wait_for(|progress| {
                progress.error.is_some()
                    || progress.written >= size
                    || progress.expected_size.map_or(false, |e| e < size)
            })
            .await
            .map_err(|_| {
                Error::new(
                    eyre!("failed to determine upload progress"),
                    ErrorKind::Network,
                )
            })? {
            Progress { error: Some(e), .. } => Err(e.clone_output()),
            Progress {
                expected_size: Some(e),
                ..
            } if *e < size => Err(Error::new(
                eyre!("file size is less than requested"),
                ErrorKind::Network,
            )),
            _ => Ok(()),
        }
    }
    async fn ready(watch: &mut watch::Receiver<Self>) -> Result<(), Error> {
        match &*watch
            .wait_for(|progress| {
                progress.error.is_some() || Some(progress.written) == progress.expected_size
            })
            .await
            .map_err(|_| {
                Error::new(
                    eyre!("failed to determine upload progress"),
                    ErrorKind::Network,
                )
            })? {
            Progress { error: Some(e), .. } => Err(e.clone_output()),
            _ => Ok(()),
        }
    }
    fn complete(&mut self) -> bool {
        match self {
            Self {
                expected_size: Some(size),
                written,
                ..
            } if *written == *size => false,
            Self {
                expected_size: Some(size),
                written,
                error,
            } if *written > *size && error.is_none() => {
                *error = Some(Error::new(
                    eyre!("Too many bytes received"),
                    ErrorKind::Network,
                ));
                true
            }
            Self {
                error,
                expected_size: Some(_),
                ..
            } if error.is_none() => {
                *error = Some(Error::new(
                    eyre!("Connection closed or timed out before full file received"),
                    ErrorKind::Network,
                ));
                true
            }
            Self {
                expected_size,
                written,
                ..
            } if expected_size.is_none() => {
                *expected_size = Some(*written);
                true
            }
            _ => false,
        }
    }
}

#[derive(Clone)]
pub struct UploadingFile {
    tmp_dir: Arc<TmpDir>,
    file: MultiCursorFile,
    progress: watch::Receiver<Progress>,
}
impl UploadingFile {
    pub async fn new() -> Result<(UploadHandle, Self), Error> {
        let progress = watch::channel(Progress::default());
        let tmp_dir = Arc::new(TmpDir::new().await?);
        let file = create_file(tmp_dir.join("upload.tmp")).await?;
        let uploading = Self {
            tmp_dir: tmp_dir.clone(),
            file: MultiCursorFile::open(&file).await?,
            progress: progress.1,
        };
        Ok((
            UploadHandle {
                tmp_dir,
                file,
                progress: progress.0,
            },
            uploading,
        ))
    }
    pub async fn delete(self) -> Result<(), Error> {
        if let Ok(tmp_dir) = Arc::try_unwrap(self.tmp_dir) {
            tmp_dir.delete().await?;
        }
        Ok(())
    }
}
impl ArchiveSource for UploadingFile {
    type FetchReader = <MultiCursorFile as ArchiveSource>::FetchReader;
    type FetchAllReader = UploadingFileReader;
    async fn size(&self) -> Option<u64> {
        Progress::expected_size(&mut self.progress.clone()).await
    }
    async fn fetch_all(&self) -> Result<Self::FetchAllReader, Error> {
        let mut file = self.file.cursor().await?;
        file.seek(SeekFrom::Start(0)).await?;
        Ok(UploadingFileReader {
            tmp_dir: self.tmp_dir.clone(),
            file,
            position: 0,
            to_seek: None,
            progress: self.progress.clone(),
        })
    }
    async fn fetch(&self, position: u64, size: u64) -> Result<Self::FetchReader, Error> {
        Progress::ready_for(&mut self.progress.clone(), position + size).await?;
        self.file.fetch(position, size).await
    }
}

#[pin_project::pin_project(project = UploadingFileReaderProjection)]
pub struct UploadingFileReader {
    tmp_dir: Arc<TmpDir>,
    position: u64,
    to_seek: Option<SeekFrom>,
    #[pin]
    file: FileCursor,
    progress: watch::Receiver<Progress>,
}
impl<'a> UploadingFileReaderProjection<'a> {
    fn poll_ready(&mut self, cx: &mut std::task::Context<'_>) -> Result<bool, std::io::Error> {
        let ready = Progress::ready(&mut *self.progress);
        tokio::pin!(ready);
        Ok(ready
            .poll_unpin(cx)
            .map_err(|e| std::io::Error::other(e.source))?
            .is_ready())
    }
    fn poll_ready_for(
        &mut self,
        cx: &mut std::task::Context<'_>,
        size: u64,
    ) -> Result<bool, std::io::Error> {
        let ready = Progress::ready_for(&mut *self.progress, size);
        tokio::pin!(ready);
        Ok(ready
            .poll_unpin(cx)
            .map_err(|e| std::io::Error::other(e.source))?
            .is_ready())
    }
}
impl AsyncRead for UploadingFileReader {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut tokio::io::ReadBuf<'_>,
    ) -> Poll<std::io::Result<()>> {
        let mut this = self.project();

        let position = *this.position;
        if this.poll_ready(cx)? || this.poll_ready_for(cx, position + buf.remaining() as u64)? {
            let start = buf.filled().len();
            let res = this.file.poll_read(cx, buf);
            *this.position += (buf.filled().len() - start) as u64;
            res
        } else {
            Poll::Pending
        }
    }
}
impl AsyncSeek for UploadingFileReader {
    fn start_seek(self: Pin<&mut Self>, position: SeekFrom) -> std::io::Result<()> {
        let this = self.project();
        *this.to_seek = Some(position);
        Ok(())
    }
    fn poll_complete(
        self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<std::io::Result<u64>> {
        let mut this = self.project();
        if let Some(to_seek) = *this.to_seek {
            let size = match to_seek {
                SeekFrom::Current(n) => (*this.position as i64 + n) as u64,
                SeekFrom::Start(n) => n,
                SeekFrom::End(n) => {
                    let expected_size = this.progress.borrow().expected_size;
                    match expected_size {
                        Some(end) => (end as i64 + n) as u64,
                        None => {
                            if !this.poll_ready(cx)? {
                                return Poll::Pending;
                            }
                            (this.progress.borrow().expected_size.ok_or_else(|| {
                                std::io::Error::new(
                                    std::io::ErrorKind::Other,
                                    eyre!("upload maked complete without expected size"),
                                )
                            })? as i64
                                + n) as u64
                        }
                    }
                }
            };
            if !this.poll_ready_for(cx, size)? {
                return Poll::Pending;
            }
        }
        if let Some(seek) = this.to_seek.take() {
            this.file.as_mut().start_seek(seek)?;
        }
        *this.position = ready!(this.file.as_mut().poll_complete(cx)?);
        Poll::Ready(Ok(*this.position))
    }
}

#[pin_project::pin_project(PinnedDrop)]
pub struct UploadHandle {
    tmp_dir: Arc<TmpDir>,
    #[pin]
    file: File,
    progress: watch::Sender<Progress>,
}
impl UploadHandle {
    pub async fn upload(&mut self, request: Request) {
        self.process_headers(request.headers());
        self.process_body(request.into_body().into_data_stream())
            .await;
    }
    pub async fn download(&mut self, response: reqwest::Response) {
        self.process_headers(response.headers());
        self.process_body(response.bytes_stream()).await;
    }
    fn process_headers(&mut self, headers: &HeaderMap) {
        if let Some(content_length) = headers
            .get(CONTENT_LENGTH)
            .and_then(|a| a.to_str().log_err())
            .and_then(|a| a.parse::<u64>().log_err())
        {
            self.progress
                .send_modify(|p| p.expected_size = Some(content_length));
        }
    }
    async fn process_body<E: Into<Box<(dyn std::error::Error + Send + Sync + 'static)>>>(
        &mut self,
        mut body: impl Stream<Item = Result<Bytes, E>> + Unpin,
    ) {
        while let Some(next) = body.next().await {
            if let Err(e) = async {
                self.write_all(
                    &next.map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?,
                )
                .await?;
                Ok(())
            }
            .await
            {
                self.progress.send_if_modified(|p| p.handle_error(&e));
                break;
            }
        }
    }
}
#[pin_project::pinned_drop]
impl PinnedDrop for UploadHandle {
    fn drop(self: Pin<&mut Self>) {
        let this = self.project();
        this.progress.send_if_modified(|p| p.complete());
    }
}
impl AsyncWrite for UploadHandle {
    fn poll_write(
        self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> Poll<Result<usize, std::io::Error>> {
        let this = self.project();
        match this.file.poll_write(cx, buf) {
            Poll::Ready(res) => {
                this.progress
                    .send_if_modified(|progress| progress.handle_write(&res));
                Poll::Ready(res)
            }
            Poll::Pending => Poll::Pending,
        }
    }
    fn poll_flush(
        self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Result<(), std::io::Error>> {
        let this = self.project();
        match this.file.poll_flush(cx) {
            Poll::Ready(Err(e)) => {
                this.progress
                    .send_if_modified(|progress| progress.handle_error(&e));
                Poll::Ready(Err(e))
            }
            a => a,
        }
    }
    fn poll_shutdown(
        self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Result<(), std::io::Error>> {
        let this = self.project();
        match this.file.poll_shutdown(cx) {
            Poll::Ready(Err(e)) => {
                this.progress
                    .send_if_modified(|progress| progress.handle_error(&e));
                Poll::Ready(Err(e))
            }
            a => a,
        }
    }
}
