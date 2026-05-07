use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;

use chrono::{DateTime, Utc};
use futures::StreamExt;
use reqwest::{Client, Response};
use serde::{Deserialize, Serialize};
use tokio::io::AsyncWriteExt;
use tokio::io::AsyncWrite;
use ts_rs::TS;
use url::Url;

use crate::prelude::*;
use crate::progress::PhaseProgressTrackerHandle;
use crate::registry::signer::AcceptSigners;
use crate::s9pk::S9pk;
use crate::s9pk::merkle_archive::source::http::HttpSource;
use crate::s9pk::merkle_archive::source::multi_cursor_file::MultiCursorFile;
use crate::s9pk::merkle_archive::source::TmpSource;
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
        for url in &self.urls {
            if let Ok(response) = client.get(url.clone()).send().await {
                return BufferedHttpSource::from_response(response, progress).await;
            }
        }
        Err(Error::new(
            eyre!("{}", t!("registry.asset.failed-to-load-http-url")),
            ErrorKind::Network,
        ))
    }
    pub async fn load_buffered_http_source_with_path(
        &self,
        path: impl AsRef<Path>,
        client: Client,
        progress: PhaseProgressTrackerHandle,
    ) -> Result<BufferedHttpSource, Error> {
        for url in &self.urls {
            if let Ok(response) = client.get(url.clone()).send().await {
                return BufferedHttpSource::from_response_with_path(path, response, progress).await;
            }
        }
        Err(Error::new(
            eyre!("{}", t!("registry.asset.failed-to-load-http-url")),
            ErrorKind::Network,
        ))
    }
    async fn download_to_path(
        &self,
        path: impl AsRef<Path>,
        client: Client,
        mut progress: PhaseProgressTrackerHandle,
    ) -> Result<MultiCursorFile, Error> {
        let mut errors = Vec::new();
        for url in &self.urls {
            progress.set_done(0);
            match Self::download_url_to_path(url, path.as_ref(), client.clone(), &mut progress).await
            {
                Ok(file) => return Ok(file),
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
    async fn download_url_to_path(
        url: &Url,
        path: &Path,
        client: Client,
        progress: &mut PhaseProgressTrackerHandle,
    ) -> Result<MultiCursorFile, Error> {
        let response = client
            .get(url.clone())
            .send()
            .await
            .with_kind(ErrorKind::Network)?
            .error_for_status()
            .with_kind(ErrorKind::Network)?;
        let expected_size = response.content_length();
        if let Some(size) = expected_size {
            progress.set_total(size);
        }

        let mut file = create_file(path).await?;
        let mut downloaded = 0;
        let mut stream = response.bytes_stream();
        while let Some(chunk) = stream.next().await {
            let chunk = chunk.with_kind(ErrorKind::Network)?;
            file.write_all(&chunk).await?;
            downloaded += chunk.len() as u64;
            *progress += chunk.len() as u64;
        }
        file.sync_all().await?;
        drop(file);

        if let Some(expected_size) = expected_size {
            ensure_code!(
                downloaded == expected_size,
                ErrorKind::Network,
                "download ended after {downloaded} bytes, expected {expected_size}"
            );
        }

        Ok(MultiCursorFile::from(open_file(path).await?))
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
    ) -> Result<S9pk<Section<TmpSource<MultiCursorFile>>>, Error> {
        let tmp_dir = Arc::new(TmpDir::new().await?);
        let source = TmpSource::new(
            tmp_dir.clone(),
            self.download_to_path(tmp_dir.join("download.s9pk"), client, progress)
                .await?,
        );
        S9pk::deserialize(&source, Some(&self.commitment)).await
    }
    pub async fn download_to(
        &self,
        path: impl AsRef<Path>,
        client: Client,
        progress: PhaseProgressTrackerHandle,
    ) -> Result<
        (
            S9pk<Section<MultiCursorFile>>,
            MultiCursorFile,
        ),
        Error,
    > {
        let source = self.download_to_path(path, client, progress).await?;
        Ok((S9pk::deserialize(&source, Some(&self.commitment)).await?, source))
    }
}

pub struct BufferedHttpSource {
    _download: NonDetachingJoinHandle<()>,
    file: UploadingFile,
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
            file,
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
            file,
        })
    }
    pub async fn wait_for_buffered(&self) -> Result<(), Error> {
        self.file.wait_for_complete().await
    }
}
impl ArchiveSource for BufferedHttpSource {
    type FetchReader = <UploadingFile as ArchiveSource>::FetchReader;
    type FetchAllReader = <UploadingFile as ArchiveSource>::FetchAllReader;
    async fn size(&self) -> Option<u64> {
        self.file.size().await
    }
    async fn fetch_all(&self) -> Result<Self::FetchAllReader, Error> {
        self.file.fetch_all().await
    }
    async fn fetch(&self, position: u64, size: u64) -> Result<Self::FetchReader, Error> {
        self.file.fetch(position, size).await
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tokio::io::{AsyncReadExt, AsyncWriteExt};
    use tokio::net::TcpListener;

    use crate::progress::FullProgressTracker;
    use crate::s9pk::merkle_archive::source::ArchiveSource;

    async fn spawn_test_server() -> Result<Url, Error> {
        let listener = TcpListener::bind("127.0.0.1:0").await?;
        let addr = listener.local_addr()?;
        tokio::spawn(async move {
            loop {
                let Ok((mut stream, _)) = listener.accept().await else {
                    break;
                };
                tokio::spawn(async move {
                    let mut buf = [0; 1024];
                    let Ok(n) = stream.read(&mut buf).await else {
                        return;
                    };
                    let request = String::from_utf8_lossy(&buf[..n]);
                    let path = request
                        .lines()
                        .next()
                        .and_then(|line| line.split_whitespace().nth(1))
                        .unwrap_or("/");
                    match path {
                        "/truncated" => {
                            let _ = stream
                                .write_all(b"HTTP/1.1 200 OK\r\nContent-Length: 11\r\n\r\nhello")
                                .await;
                        }
                        "/complete" => {
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
        Url::parse(&format!("http://{addr}")).with_kind(ErrorKind::ParseUrl)
    }

    #[tokio::test]
    async fn download_to_path_falls_back_after_truncated_mirror() -> Result<(), Error> {
        let base_url = spawn_test_server().await?;
        let asset = RegistryAsset {
            published_at: Utc::now(),
            urls: vec![base_url.join("truncated")?, base_url.join("complete")?],
            commitment: (),
            signatures: HashMap::new(),
        };
        let progress = FullProgressTracker::new().add_phase("Downloading".into(), Some(100));
        let tmp_dir = TmpDir::new().await?;

        let source = asset
            .download_to_path(tmp_dir.join("package.s9pk"), Client::new(), progress)
            .await?;
        let mut contents = Vec::new();
        source.fetch_all().await?.read_to_end(&mut contents).await?;

        assert_eq!(contents, b"hello world");
        Ok(())
    }
}
