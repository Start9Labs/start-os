use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;

use chrono::{DateTime, Utc};
use reqwest::Client;
use serde::{Deserialize, Serialize};
use tokio::io::AsyncWrite;
use ts_rs::TS;
use url::Url;

use crate::prelude::*;
use crate::progress::PhaseProgressTrackerHandle;
use crate::registry::signer::AcceptSigners;
use crate::s9pk::S9pk;
use crate::s9pk::merkle_archive::source::http::HttpSource;
use crate::s9pk::merkle_archive::source::{ArchiveSource, Section};
use crate::sign::commitment::merkle_archive::MerkleArchiveCommitment;
use crate::sign::commitment::{Commitment, Digestable};
use crate::sign::{AnySignature, AnyVerifyingKey};
use crate::upload::UploadingFile;
use crate::util::future::NonDetachingJoinHandle;

#[derive(Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct RegistryAsset<Commitment> {
    #[ts(type = "string")]
    pub published_at: DateTime<Utc>,
    #[ts(type = "string")]
    pub url: Url,
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
            .copy_to(&HttpSource::new(client, self.url.clone()).await?, dst)
            .await
    }
}
impl RegistryAsset<MerkleArchiveCommitment> {
    pub async fn deserialize_s9pk(
        &self,
        client: Client,
    ) -> Result<S9pk<Section<Arc<HttpSource>>>, Error> {
        S9pk::deserialize(
            &Arc::new(HttpSource::new(client, self.url.clone()).await?),
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
            &Arc::new(BufferedHttpSource::new(client, self.url.clone(), progress).await?),
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
            BufferedHttpSource::with_path(path, client, self.url.clone(), progress).await?,
        );
        Ok((
            S9pk::deserialize(&source, Some(&self.commitment)).await?,
            source,
        ))
    }
}

pub struct BufferedHttpSource {
    _download: NonDetachingJoinHandle<()>,
    file: UploadingFile,
}
impl BufferedHttpSource {
    pub async fn with_path(
        path: impl AsRef<Path>,
        client: Client,
        url: Url,
        progress: PhaseProgressTrackerHandle,
    ) -> Result<Self, Error> {
        let (mut handle, file) = UploadingFile::with_path(path, progress).await?;
        let response = client.get(url).send().await?;
        Ok(Self {
            _download: tokio::spawn(async move { handle.download(response).await }).into(),
            file,
        })
    }
    pub async fn new(
        client: Client,
        url: Url,
        progress: PhaseProgressTrackerHandle,
    ) -> Result<Self, Error> {
        let (mut handle, file) = UploadingFile::new(progress).await?;
        let response = client.get(url).send().await?;
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
