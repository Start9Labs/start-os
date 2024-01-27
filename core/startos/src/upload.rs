use std::path::PathBuf;
use std::pin::Pin;
use std::sync::Arc;
use std::task::Poll;
use std::time::Duration;

use axum::body::Body;
use axum::response::Response;
use clap::Parser;
use futures::{FutureExt, StreamExt};
use http::header::CONTENT_LENGTH;
use http::StatusCode;
use tokio::fs::File;
use tokio::io::{AsyncRead, AsyncWrite, AsyncWriteExt};
use tokio::sync::{watch, OwnedMutexGuard};

use crate::context::RpcContext;
use crate::core::rpc_continuations::{RequestGuid, RpcContinuation};
use crate::prelude::*;
use crate::s9pk::merkle_archive::source::multi_cursor_file::{FileSectionReader, MultiCursorFile};
use crate::s9pk::merkle_archive::source::ArchiveSource;
use crate::util::io::TmpDir;

pub async fn upload(ctx: &RpcContext) -> Result<(RequestGuid, UploadingFile), Error> {
    let guid = RequestGuid::new();
    let (mut handle, file) = UploadingFile::new().await?;
    ctx.add_continuation(
        guid.clone(),
        RpcContinuation::rest(
            Box::new(|request| {
                async move {
                    let headers = request.headers();
                    let content_length = match headers.get(CONTENT_LENGTH).map(|a| a.to_str()) {
                        None => {
                            return Response::builder()
                                .status(StatusCode::BAD_REQUEST)
                                .body(Body::from("Content-Length is required"))
                                .with_kind(ErrorKind::Network)
                        }
                        Some(Err(_)) => {
                            return Response::builder()
                                .status(StatusCode::BAD_REQUEST)
                                .body(Body::from("Invalid Content-Length"))
                                .with_kind(ErrorKind::Network)
                        }
                        Some(Ok(a)) => match a.parse::<u64>() {
                            Err(_) => {
                                return Response::builder()
                                    .status(StatusCode::BAD_REQUEST)
                                    .body(Body::from("Invalid Content-Length"))
                                    .with_kind(ErrorKind::Network)
                            }
                            Ok(a) => a,
                        },
                    };

                    handle
                        .progress
                        .send_modify(|p| p.expected_size = Some(content_length));

                    let mut body = request.into_body().into_data_stream();
                    while let Some(next) = body.next().await {
                        if let Err(e) = async {
                            handle
                                .write_all(&next.map_err(|e| {
                                    std::io::Error::new(std::io::ErrorKind::Other, e)
                                })?)
                                .await?;
                            Ok(())
                        }
                        .await
                        {
                            handle.progress.send_if_modified(|p| p.handle_error(&e));
                            break;
                        }
                    }

                    Response::builder()
                        .status(StatusCode::NO_CONTENT)
                        .body(Body::empty())
                        .with_kind(ErrorKind::Network)
                }
                .boxed()
            }),
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
        if let Some(e) = watch
            .wait_for(|progress| progress.error.is_some() || progress.written >= size)
            .await
            .map_err(|_| {
                Error::new(
                    eyre!("failed to determine upload progress"),
                    ErrorKind::Network,
                )
            })?
            .error
            .as_ref()
            .map(|e| e.clone_output())
        {
            Err(e)
        } else {
            Ok(())
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
            Self { error, .. } if error.is_none() => {
                *error = Some(Error::new(
                    eyre!("Connection closed or timed out before full file received"),
                    ErrorKind::Network,
                ));
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
        let file = File::create(tmp_dir.join("upload.tmp")).await?;
        let uploading = Self {
            tmp_dir,
            file: MultiCursorFile::open(&file).await?,
            progress: progress.1,
        };
        Ok((
            UploadHandle {
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
#[async_trait::async_trait]
impl ArchiveSource for UploadingFile {
    type Reader = <MultiCursorFile as ArchiveSource>::Reader;
    async fn size(&self) -> Option<u64> {
        Progress::expected_size(&mut self.progress.clone()).await
    }
    async fn fetch(&self, position: u64, size: u64) -> Result<Self::Reader, Error> {
        Progress::ready_for(&mut self.progress.clone(), position + size).await?;
        self.file.fetch(position, size).await
    }
}

#[pin_project::pin_project(PinnedDrop)]
pub struct UploadHandle {
    #[pin]
    file: File,
    progress: watch::Sender<Progress>,
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

// #[derive(Deserialize, Serialize, Parser)]
// pub struct CliUploadParams {
//     path: PathBuf,
// }

// /// BLOCKING
// pub fn cli_upload(ctx: CliContext, CliUploadParams { path }: CliUploadParams) -> Result<(), Error> {
//     todo!()
// }

// pub fn rpc_upload(ctx: RpcContext) -> Result<RequestGuid, Error> {
//     let guid = RequestGuid::new();
//     let uploads: Uploads = todo!();
//     uploads.0.insert(key, value)
// }
