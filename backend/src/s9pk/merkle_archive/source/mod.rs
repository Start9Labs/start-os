use std::path::PathBuf;

use tokio::fs::File;
use tokio::io::{AsyncRead, AsyncWrite};

use crate::prelude::*;

pub mod http;
pub mod multi_cursor_file;

#[async_trait::async_trait]
pub trait FileSource: Send + Sync + Sized + 'static {
    const TRUSTED: bool;
    type Reader: AsyncRead + Unpin + Send;
    async fn size(&self) -> Result<u64, Error>;
    async fn reader(&self) -> Result<Self::Reader, Error>;
    async fn copy_to<W: AsyncWrite + Unpin + Send>(&self, w: &mut W) -> Result<(), Error> {
        tokio::io::copy(&mut self.reader().await?, w).await?;
        Ok(())
    }
}

#[async_trait::async_trait]
impl FileSource for PathBuf {
    const TRUSTED: bool = true;
    type Reader = File;
    async fn size(&self) -> Result<u64, Error> {
        Ok(tokio::fs::metadata(self).await?.len())
    }
    async fn reader(&self) -> Result<Self::Reader, Error> {
        Ok(File::open(self).await?)
    }
}

#[async_trait::async_trait]
pub trait ArchiveSource: Clone + Send + Sync + Sized + 'static {
    const TRUSTED: bool;
    type Reader: AsyncRead + Unpin + Send;
    async fn fetch(&self, position: u64, size: u64) -> Result<Self::Reader, Error>;
    async fn copy_to<W: AsyncWrite + Unpin + Send>(
        &self,
        position: u64,
        size: u64,
        w: &mut W,
    ) -> Result<(), Error> {
        tokio::io::copy(&mut self.fetch(position, size).await?, w).await?;
        Ok(())
    }
    fn section(&self, position: u64, size: u64) -> Section<Self> {
        Section {
            source: self.clone(),
            position,
            size,
        }
    }
}

pub struct Section<S> {
    source: S,
    position: u64,
    size: u64,
}
#[async_trait::async_trait]
impl<S: ArchiveSource> FileSource for Section<S> {
    const TRUSTED: bool = S::TRUSTED;
    type Reader = S::Reader;
    async fn size(&self) -> Result<u64, Error> {
        Ok(self.size)
    }
    async fn reader(&self) -> Result<Self::Reader, Error> {
        self.source.fetch(self.position, self.size).await
    }
    async fn copy_to<W: AsyncWrite + Unpin + Send>(&self, w: &mut W) -> Result<(), Error> {
        self.source.copy_to(self.position, self.size, w).await
    }
}
