use std::collections::HashMap;
use std::path::Path;
use std::pin::Pin;
use std::sync::Arc;
use std::task::{Context, Poll};

use chrono::{DateTime, Utc};
use futures::StreamExt;
use reqwest::header::RANGE;
use reqwest::{Client, Response, StatusCode};
use serde::{Deserialize, Serialize};
use tokio::io::AsyncWrite;
use tokio::io::{AsyncRead, AsyncSeekExt, AsyncWriteExt, ReadBuf};
use ts_rs::TS;
use url::Url;

use crate::prelude::*;
use crate::progress::PhaseProgressTrackerHandle;
use crate::registry::signer::AcceptSigners;
use crate::s9pk::S9pk;
use crate::s9pk::merkle_archive::source::http::HttpSource;
use crate::s9pk::merkle_archive::source::multi_cursor_file::MultiCursorFile;
use crate::s9pk::merkle_archive::source::{ArchiveSource, Section};
use crate::sign::commitment::merkle_archive::MerkleArchiveCommitment;
use crate::sign::commitment::{Commitment, Digestable};
use crate::sign::{AnySignature, AnyVerifyingKey};
use crate::upload::UploadingFile;
use crate::util::future::NonDetachingJoinHandle;
use crate::util::io::{TmpDir, create_file, open_file};

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct RegistryAsset<Commitment> {
    #[ts(type = "string")]
    pub published_at: DateTime<Utc>,
    #[ts(type = "string[]")]
    pub urls: Vec<Url>,
    pub commitment: Commitment,
    pub signatures: HashMap<AnyVerifyingKey, AnySignature>,
}
impl<Commitment> RegistryAsset<Commitment> {
    pub fn all_signers(&self) -> AcceptSigners {
        AcceptSigners::All(
            self.signatures
                .keys()
                .cloned()
                .map(AcceptSigners::Signer)
                .collect(),
        )
    }
    pub async fn load_http_source(&self, client: Client) -> Result<HttpSource, Error> {
        for url in &self.urls {
            if let Ok(source) = HttpSource::new(client.clone(), url.clone()).await {
                return Ok(source);
            }
        }
        Err(Error::new(
            eyre!("{}", t!("registry.asset.failed-to-load-http-url")),
            ErrorKind::Network,
        ))
    }
    pub async fn load_buffered_http_source(
        &self,
        client: Client,
        progress: PhaseProgressTrackerHandle,
    ) -> Result<BufferedHttpSource, Error> {
        BufferedHttpSource::from_urls(&self.urls, client, progress).await
    }
    pub async fn load_buffered_http_source_with_path(
        &self,
        path: impl AsRef<Path>,
        client: Client,
        progress: PhaseProgressTrackerHandle,
    ) -> Result<BufferedHttpSource, Error> {
        BufferedHttpSource::from_urls_with_path(path, &self.urls, client, progress).await
    }
}
impl<Commitment: Digestable> RegistryAsset<Commitment> {
    pub fn validate(&self, context: &str, mut accept: AcceptSigners) -> Result<&Commitment, Error> {
        for (signer, signature) in &self.signatures {
            accept.process_signature(signer, &self.commitment, context, signature)?;
        }
        accept.try_accept()?;
        Ok(&self.commitment)
    }
}
impl<C: for<'a> Commitment<&'a HttpSource>> RegistryAsset<C> {
    pub async fn download(
        &self,
        client: Client,
        dst: &mut (impl AsyncWrite + Unpin + Send + ?Sized),
    ) -> Result<(), Error> {
        self.commitment
            .copy_to(&self.load_http_source(client).await?, dst)
            .await
    }
}
impl RegistryAsset<MerkleArchiveCommitment> {
    pub async fn deserialize_s9pk(
        &self,
        client: Client,
    ) -> Result<S9pk<Section<Arc<HttpSource>>>, Error> {
        S9pk::deserialize(
            &Arc::new(self.load_http_source(client).await?),
            Some(&self.commitment),
        )
        .await
    }
    pub async fn deserialize_s9pk_buffered(
        &self,
        client: Client,
        progress: PhaseProgressTrackerHandle,
    ) -> Result<S9pk<Section<Arc<BufferedHttpSource>>>, Error> {
        S9pk::deserialize(
            &Arc::new(self.load_buffered_http_source(client, progress).await?),
            Some(&self.commitment),
        )
        .await
    }
    pub async fn download_to(
        &self,
        path: impl AsRef<Path>,
        client: Client,
        progress: PhaseProgressTrackerHandle,
    ) -> Result<
        (
            S9pk<Section<Arc<BufferedHttpSource>>>,
            Arc<BufferedHttpSource>,
        ),
        Error,
    > {
        let source = Arc::new(
            self.load_buffered_http_source_with_path(path, client, progress)
                .await?,
        );
        Ok((
            S9pk::deserialize(&source, Some(&self.commitment)).await?,
            source,
        ))
    }
}

pub struct BufferedHttpSource {
    _download: NonDetachingJoinHandle<()>,
    file: BufferedHttpFile,
}
enum BufferedHttpFile {
    Uploading(UploadingFile),
    Complete {
        _tmp_dir: Option<Arc<TmpDir>>,
        file: MultiCursorFile,
    },
}

struct DownloadFailure {
    error: Error,
    written: u64,
    expected: Option<u64>,
}

#[pin_project::pin_project(project = BufferedFetchReaderProj)]
pub enum BufferedFetchReader {
    Uploading(#[pin] <UploadingFile as ArchiveSource>::FetchReader),
    Complete(#[pin] <MultiCursorFile as ArchiveSource>::FetchReader),
}
impl AsyncRead for BufferedFetchReader {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> Poll<std::io::Result<()>> {
        match self.project() {
            BufferedFetchReaderProj::Uploading(reader) => reader.poll_read(cx, buf),
            BufferedFetchReaderProj::Complete(reader) => reader.poll_read(cx, buf),
        }
    }
}

#[pin_project::pin_project(project = BufferedFetchAllReaderProj)]
pub enum BufferedFetchAllReader {
    Uploading(#[pin] <UploadingFile as ArchiveSource>::FetchAllReader),
    Complete(#[pin] <MultiCursorFile as ArchiveSource>::FetchAllReader),
}
impl AsyncRead for BufferedFetchAllReader {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> Poll<std::io::Result<()>> {
        match self.project() {
            BufferedFetchAllReaderProj::Uploading(reader) => reader.poll_read(cx, buf),
            BufferedFetchAllReaderProj::Complete(reader) => reader.poll_read(cx, buf),
        }
    }
}

impl BufferedHttpSource {
    pub async fn new(
        client: Client,
        url: Url,
        progress: PhaseProgressTrackerHandle,
    ) -> Result<Self, Error> {
        let response = client.get(url).send().await?;
        Self::from_response(response, progress).await
    }
    pub async fn from_response(
        response: Response,
        progress: PhaseProgressTrackerHandle,
    ) -> Result<Self, Error> {
        let (mut handle, file) = UploadingFile::new(progress).await?;
        Ok(Self {
            _download: tokio::spawn(async move { handle.download(response).await }).into(),
            file: BufferedHttpFile::Uploading(file),
        })
    }
    pub async fn from_response_with_path(
        path: impl AsRef<Path>,
        response: Response,
        progress: PhaseProgressTrackerHandle,
    ) -> Result<Self, Error> {
        let (mut handle, file) = UploadingFile::with_path(path, progress).await?;
        Ok(Self {
            _download: tokio::spawn(async move { handle.download(response).await }).into(),
            file: BufferedHttpFile::Uploading(file),
        })
    }
    async fn from_urls(
        urls: &[Url],
        client: Client,
        progress: PhaseProgressTrackerHandle,
    ) -> Result<Self, Error> {
        let tmp_dir = Arc::new(TmpDir::new().await?);
        let path = tmp_dir.join("download.s9pk");
        Self::from_urls_inner(Some(tmp_dir), path, urls, client, progress).await
    }
    async fn from_urls_with_path(
        path: impl AsRef<Path>,
        urls: &[Url],
        client: Client,
        progress: PhaseProgressTrackerHandle,
    ) -> Result<Self, Error> {
        Self::from_urls_inner(None, path.as_ref().to_owned(), urls, client, progress).await
    }
    async fn from_urls_inner(
        tmp_dir: Option<Arc<TmpDir>>,
        path: impl AsRef<Path>,
        urls: &[Url],
        client: Client,
        mut progress: PhaseProgressTrackerHandle,
    ) -> Result<Self, Error> {
        let mut errors = Vec::new();
        for url in urls {
            progress.set_done(0);
            match Self::download_url_with_retry(url, path.as_ref(), client.clone(), &mut progress)
                .await
            {
                Ok(()) => {
                    let file = MultiCursorFile::from(open_file(path.as_ref()).await?);
                    return Ok(Self {
                        _download: tokio::spawn(async {}).into(),
                        file: BufferedHttpFile::Complete {
                            _tmp_dir: tmp_dir,
                            file,
                        },
                    });
                }
                Err(e) => {
                    tracing::warn!("Failed to download {url}; trying next mirror: {e}");
                    errors.push(format!("{url}: {e}"));
                }
            }
        }
        Err(Error::new(
            eyre!(
                "failed to download package from any mirror: {}",
                errors.join("; ")
            ),
            ErrorKind::Network,
        ))
    }
    async fn download_url_with_retry(
        url: &Url,
        path: &Path,
        client: Client,
        progress: &mut PhaseProgressTrackerHandle,
    ) -> Result<(), Error> {
        match Self::download_full_response(path, client.get(url.clone()).send().await, progress)
            .await
        {
            Ok(_) => Ok(()),
            Err(first) => {
                tracing::warn!(
                    "Failed to download {url}; retrying same mirror: {}",
                    first.error
                );
                if let Some(total) = first.expected {
                    if first.written > 0 && first.written < total {
                        match Self::download_range_response(
                            path,
                            client
                                .get(url.clone())
                                .header(RANGE, format!("bytes={}-", first.written))
                                .send()
                                .await,
                            first.written,
                            total,
                            progress,
                        )
                        .await
                        {
                            Ok(()) => return Ok(()),
                            Err(range) => {
                                tracing::warn!(
                                    "Failed to resume {url}; retrying full download: {}",
                                    range.error
                                );
                            }
                        }
                    }
                }
                progress.set_done(0);
                Self::download_full_response(path, client.get(url.clone()).send().await, progress)
                    .await
                    .map(|_| ())
                    .map_err(|retry| retry.error)
            }
        }
    }
    async fn download_full_response(
        path: &Path,
        response: Result<Response, reqwest::Error>,
        progress: &mut PhaseProgressTrackerHandle,
    ) -> Result<(), DownloadFailure> {
        let response = response.map_err(|e| DownloadFailure {
            error: Error::new(e, ErrorKind::Network),
            written: 0,
            expected: None,
        })?;
        let response = response.error_for_status().map_err(|e| DownloadFailure {
            error: Error::new(e, ErrorKind::Network),
            written: 0,
            expected: None,
        })?;
        let expected = response.content_length();
        if let Some(total) = expected {
            progress.set_total(total);
        }
        let file = create_file(path).await.map_err(|error| DownloadFailure {
            error,
            written: 0,
            expected,
        })?;
        Self::write_response(file, response, 0, expected, progress).await
    }
    async fn download_range_response(
        path: &Path,
        response: Result<Response, reqwest::Error>,
        start: u64,
        expected: u64,
        progress: &mut PhaseProgressTrackerHandle,
    ) -> Result<(), DownloadFailure> {
        let response = response.map_err(|e| DownloadFailure {
            error: Error::new(e, ErrorKind::Network),
            written: start,
            expected: Some(expected),
        })?;
        match response.status() {
            StatusCode::PARTIAL_CONTENT => {
                let mut file = tokio::fs::OpenOptions::new()
                    .append(true)
                    .open(path)
                    .await
                    .map_err(|error| DownloadFailure {
                        error: Error::new(error, ErrorKind::Filesystem),
                        written: start,
                        expected: Some(expected),
                    })?;
                file.seek(std::io::SeekFrom::End(0))
                    .await
                    .map_err(|error| DownloadFailure {
                        error: Error::new(error, ErrorKind::Filesystem),
                        written: start,
                        expected: Some(expected),
                    })?;
                progress.set_total(expected);
                progress.set_done(start);
                Self::write_response(file, response, start, Some(expected), progress)
                    .await
                    .map(|_| ())
            }
            StatusCode::OK => {
                progress.set_done(0);
                Self::download_full_response(path, Ok(response), progress)
                    .await
                    .map(|_| ())
            }
            _ => {
                let status = response.status();
                Err(DownloadFailure {
                    error: Error::new(
                        eyre!("range request failed with {status}"),
                        ErrorKind::Network,
                    ),
                    written: start,
                    expected: Some(expected),
                })
            }
        }
    }
    async fn write_response(
        mut file: tokio::fs::File,
        response: Response,
        start: u64,
        expected: Option<u64>,
        progress: &mut PhaseProgressTrackerHandle,
    ) -> Result<(), DownloadFailure> {
        let mut written = start;
        let mut stream = response.bytes_stream();
        while let Some(chunk) = stream.next().await {
            let chunk = chunk.map_err(|e| DownloadFailure {
                error: Error::new(e, ErrorKind::Network),
                written,
                expected,
            })?;
            file.write_all(&chunk)
                .await
                .map_err(|error| DownloadFailure {
                    error: Error::new(error, ErrorKind::Filesystem),
                    written,
                    expected,
                })?;
            written += chunk.len() as u64;
            *progress += chunk.len() as u64;
        }
        file.sync_all().await.map_err(|error| DownloadFailure {
            error: Error::new(error, ErrorKind::Filesystem),
            written,
            expected,
        })?;
        if let Some(expected) = expected {
            if written != expected {
                return Err(DownloadFailure {
                    error: Error::new(
                        eyre!("download ended after {written} bytes, expected {expected}"),
                        ErrorKind::Network,
                    ),
                    written,
                    expected: Some(expected),
                });
            }
        }
        progress.complete();
        Ok(())
    }
    pub async fn wait_for_buffered(&self) -> Result<(), Error> {
        match &self.file {
            BufferedHttpFile::Uploading(file) => file.wait_for_complete().await,
            BufferedHttpFile::Complete { .. } => Ok(()),
        }
    }
}
impl ArchiveSource for BufferedHttpSource {
    type FetchReader = BufferedFetchReader;
    type FetchAllReader = BufferedFetchAllReader;
    async fn size(&self) -> Option<u64> {
        match &self.file {
            BufferedHttpFile::Uploading(file) => file.size().await,
            BufferedHttpFile::Complete { file, .. } => file.size().await,
        }
    }
    async fn fetch_all(&self) -> Result<Self::FetchAllReader, Error> {
        match &self.file {
            BufferedHttpFile::Uploading(file) => {
                Ok(BufferedFetchAllReader::Uploading(file.fetch_all().await?))
            }
            BufferedHttpFile::Complete { file, .. } => {
                Ok(BufferedFetchAllReader::Complete(file.fetch_all().await?))
            }
        }
    }
    async fn fetch(&self, position: u64, size: u64) -> Result<Self::FetchReader, Error> {
        match &self.file {
            BufferedHttpFile::Uploading(file) => Ok(BufferedFetchReader::Uploading(
                file.fetch(position, size).await?,
            )),
            BufferedHttpFile::Complete { file, .. } => Ok(BufferedFetchReader::Complete(
                file.fetch(position, size).await?,
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    use tokio::io::{AsyncReadExt, AsyncWriteExt};
    use tokio::net::TcpListener;

    use crate::progress::FullProgressTracker;
    use crate::s9pk::merkle_archive::source::ArchiveSource;

    struct TestServer {
        url: Url,
        complete_hits: Arc<AtomicUsize>,
    }

    async fn spawn_test_server() -> Result<TestServer, Error> {
        let listener = TcpListener::bind("127.0.0.1:0").await?;
        let addr = listener.local_addr()?;
        let complete_hits = Arc::new(AtomicUsize::new(0));
        let server_complete_hits = complete_hits.clone();
        tokio::spawn(async move {
            loop {
                let Ok((mut stream, _)) = listener.accept().await else {
                    break;
                };
                let complete_hits = server_complete_hits.clone();
                tokio::spawn(async move {
                    let mut buf = [0; 1024];
                    let Ok(n) = stream.read(&mut buf).await else {
                        return;
                    };
                    let request = String::from_utf8_lossy(&buf[..n]);
                    let request_lower = request.to_ascii_lowercase();
                    let path = request
                        .lines()
                        .next()
                        .and_then(|line| line.split_whitespace().nth(1))
                        .unwrap_or("/");
                    match path {
                        "/resume" if request_lower.contains("range: bytes=5-") => {
                            let _ = stream
                                .write_all(
                                    b"HTTP/1.1 206 Partial Content\r\nContent-Length: 6\r\nContent-Range: bytes 5-10/11\r\n\r\n world",
                                )
                                .await;
                        }
                        "/resume" => {
                            let _ = stream
                                .write_all(b"HTTP/1.1 200 OK\r\nContent-Length: 11\r\n\r\nhello")
                                .await;
                        }
                        "/range-ignored" if request_lower.contains("range: bytes=5-") => {
                            let _ = stream
                                .write_all(
                                    b"HTTP/1.1 200 OK\r\nContent-Length: 11\r\n\r\nhello world",
                                )
                                .await;
                        }
                        "/range-ignored" => {
                            let _ = stream
                                .write_all(b"HTTP/1.1 200 OK\r\nContent-Length: 11\r\n\r\nhello")
                                .await;
                        }
                        "/always-truncated" => {
                            let _ = stream
                                .write_all(b"HTTP/1.1 200 OK\r\nContent-Length: 11\r\n\r\nhello")
                                .await;
                        }
                        "/complete" => {
                            complete_hits.fetch_add(1, Ordering::SeqCst);
                            let _ = stream
                                .write_all(
                                    b"HTTP/1.1 200 OK\r\nContent-Length: 11\r\n\r\nhello world",
                                )
                                .await;
                        }
                        _ => {
                            let _ = stream
                                .write_all(b"HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\n\r\n")
                                .await;
                        }
                    }
                });
            }
        });
        Ok(TestServer {
            url: Url::parse(&format!("http://{addr}")).with_kind(ErrorKind::ParseUrl)?,
            complete_hits,
        })
    }

    async fn buffered_contents(asset: RegistryAsset<()>) -> Result<Vec<u8>, Error> {
        let progress = FullProgressTracker::new().add_phase("Downloading".into(), Some(100));
        let tmp_dir = TmpDir::new().await?;
        let source = asset
            .load_buffered_http_source_with_path(
                tmp_dir.join("package.s9pk"),
                Client::new(),
                progress,
            )
            .await?;
        let mut contents = Vec::new();
        source.fetch_all().await?.read_to_end(&mut contents).await?;
        Ok(contents)
    }

    #[tokio::test]
    async fn buffered_download_resumes_same_url_after_truncated_response() -> Result<(), Error> {
        let server = spawn_test_server().await?;
        let asset = RegistryAsset {
            published_at: Utc::now(),
            urls: vec![server.url.join("resume")?, server.url.join("complete")?],
            commitment: (),
            signatures: HashMap::new(),
        };

        assert_eq!(buffered_contents(asset).await?, b"hello world");
        assert_eq!(server.complete_hits.load(Ordering::SeqCst), 0);
        Ok(())
    }

    #[tokio::test]
    async fn buffered_download_retries_same_url_when_range_is_ignored() -> Result<(), Error> {
        let server = spawn_test_server().await?;
        let asset = RegistryAsset {
            published_at: Utc::now(),
            urls: vec![
                server.url.join("range-ignored")?,
                server.url.join("complete")?,
            ],
            commitment: (),
            signatures: HashMap::new(),
        };

        assert_eq!(buffered_contents(asset).await?, b"hello world");
        assert_eq!(server.complete_hits.load(Ordering::SeqCst), 0);
        Ok(())
    }

    #[tokio::test]
    async fn buffered_download_falls_back_to_next_mirror_after_retry_fails() -> Result<(), Error> {
        let server = spawn_test_server().await?;
        let asset = RegistryAsset {
            published_at: Utc::now(),
            urls: vec![
                server.url.join("always-truncated")?,
                server.url.join("complete")?,
            ],
            commitment: (),
            signatures: HashMap::new(),
        };

        assert_eq!(buffered_contents(asset).await?, b"hello world");
        assert_eq!(server.complete_hits.load(Ordering::SeqCst), 1);
        Ok(())
    }

    #[tokio::test]
    async fn buffered_download_fails_after_all_mirrors_fail() -> Result<(), Error> {
        let server = spawn_test_server().await?;
        let asset = RegistryAsset {
            published_at: Utc::now(),
            urls: vec![server.url.join("always-truncated")?],
            commitment: (),
            signatures: HashMap::new(),
        };

        assert!(buffered_contents(asset).await.is_err());
        Ok(())
    }
}
