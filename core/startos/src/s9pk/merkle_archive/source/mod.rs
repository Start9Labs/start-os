use std::path::PathBuf;
use std::sync::Arc;

use blake3::Hash;
use tokio::fs::File;
use tokio::io::{AsyncRead, AsyncWrite};

use crate::prelude::*;
use crate::s9pk::merkle_archive::hash::VerifyingWriter;

pub mod http;
pub mod multi_cursor_file;

#[async_trait::async_trait]
pub trait FileSource: Clone + Send + Sync + Sized + 'static {
    type Reader: AsyncRead + Unpin + Send;
    async fn size(&self) -> Result<u64, Error>;
    async fn reader(&self) -> Result<Self::Reader, Error>;
    async fn copy<W: AsyncWrite + Unpin + Send + ?Sized>(&self, w: &mut W) -> Result<(), Error> {
        tokio::io::copy(&mut self.reader().await?, w).await?;
        Ok(())
    }
    async fn copy_verify<W: AsyncWrite + Unpin + Send + ?Sized>(
        &self,
        w: &mut W,
        verify: Option<Hash>,
    ) -> Result<(), Error> {
        let mut w = VerifyingWriter::new(w, verify);
        tokio::io::copy(&mut self.reader().await?, &mut w).await?;
        w.verify()?;
        Ok(())
    }
    async fn to_vec(&self, verify: Option<Hash>) -> Result<Vec<u8>, Error> {
        let mut vec = Vec::with_capacity(self.size().await? as usize);
        self.copy_verify(&mut vec, verify).await?;
        Ok(vec)
    }
}

#[derive(Clone)]
pub struct DynFileSource(Arc<dyn DynableFileSource>);
impl DynFileSource {
    pub fn new<T: FileSource>(source: T) -> Self {
        Self(Arc::new(source))
    }
}
#[async_trait::async_trait]
impl FileSource for DynFileSource {
    type Reader = Box<dyn AsyncRead + Unpin + Send>;
    async fn size(&self) -> Result<u64, Error> {
        self.0.size().await
    }
    async fn reader(&self) -> Result<Self::Reader, Error> {
        self.0.reader().await
    }
    async fn copy<W: AsyncWrite + Unpin + Send + ?Sized>(
        &self,
        mut w: &mut W,
    ) -> Result<(), Error> {
        self.0.copy(&mut w).await
    }
    async fn copy_verify<W: AsyncWrite + Unpin + Send + ?Sized>(
        &self,
        mut w: &mut W,
        verify: Option<Hash>,
    ) -> Result<(), Error> {
        self.0.copy_verify(&mut w, verify).await
    }
    async fn to_vec(&self, verify: Option<Hash>) -> Result<Vec<u8>, Error> {
        self.0.to_vec(verify).await
    }
}

#[async_trait::async_trait]
trait DynableFileSource: Send + Sync + 'static {
    async fn size(&self) -> Result<u64, Error>;
    async fn reader(&self) -> Result<Box<dyn AsyncRead + Unpin + Send>, Error>;
    async fn copy(&self, w: &mut (dyn AsyncWrite + Unpin + Send)) -> Result<(), Error>;
    async fn copy_verify(
        &self,
        w: &mut (dyn AsyncWrite + Unpin + Send),
        verify: Option<Hash>,
    ) -> Result<(), Error>;
    async fn to_vec(&self, verify: Option<Hash>) -> Result<Vec<u8>, Error>;
}
#[async_trait::async_trait]
impl<T: FileSource> DynableFileSource for T {
    async fn size(&self) -> Result<u64, Error> {
        FileSource::size(self).await
    }
    async fn reader(&self) -> Result<Box<dyn AsyncRead + Unpin + Send>, Error> {
        Ok(Box::new(FileSource::reader(self).await?))
    }
    async fn copy(&self, w: &mut (dyn AsyncWrite + Unpin + Send)) -> Result<(), Error> {
        FileSource::copy(self, w).await
    }
    async fn copy_verify(
        &self,
        w: &mut (dyn AsyncWrite + Unpin + Send),
        verify: Option<Hash>,
    ) -> Result<(), Error> {
        FileSource::copy_verify(self, w, verify).await
    }
    async fn to_vec(&self, verify: Option<Hash>) -> Result<Vec<u8>, Error> {
        FileSource::to_vec(self, verify).await
    }
}

#[async_trait::async_trait]
impl FileSource for PathBuf {
    type Reader = File;
    async fn size(&self) -> Result<u64, Error> {
        Ok(tokio::fs::metadata(self).await?.len())
    }
    async fn reader(&self) -> Result<Self::Reader, Error> {
        Ok(File::open(self).await?)
    }
}

#[async_trait::async_trait]
impl FileSource for Arc<[u8]> {
    type Reader = std::io::Cursor<Self>;
    async fn size(&self) -> Result<u64, Error> {
        Ok(self.len() as u64)
    }
    async fn reader(&self) -> Result<Self::Reader, Error> {
        Ok(std::io::Cursor::new(self.clone()))
    }
    async fn copy<W: AsyncWrite + Unpin + Send + ?Sized>(&self, w: &mut W) -> Result<(), Error> {
        use tokio::io::AsyncWriteExt;

        w.write_all(&*self).await?;
        Ok(())
    }
}

#[async_trait::async_trait]
pub trait ArchiveSource: Clone + Send + Sync + Sized + 'static {
    type Reader: AsyncRead + Unpin + Send;
    async fn size(&self) -> Option<u64> {
        None
    }
    async fn fetch(&self, position: u64, size: u64) -> Result<Self::Reader, Error>;
    async fn copy_to<W: AsyncWrite + Unpin + Send + ?Sized>(
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

#[async_trait::async_trait]
impl ArchiveSource for Arc<[u8]> {
    type Reader = tokio::io::Take<std::io::Cursor<Self>>;
    async fn fetch(&self, position: u64, size: u64) -> Result<Self::Reader, Error> {
        use tokio::io::AsyncReadExt;

        let mut cur = std::io::Cursor::new(self.clone());
        cur.set_position(position);
        Ok(cur.take(size))
    }
}

#[derive(Debug, Clone)]
pub struct Section<S> {
    source: S,
    position: u64,
    size: u64,
}
#[async_trait::async_trait]
impl<S: ArchiveSource> FileSource for Section<S> {
    type Reader = S::Reader;
    async fn size(&self) -> Result<u64, Error> {
        Ok(self.size)
    }
    async fn reader(&self) -> Result<Self::Reader, Error> {
        self.source.fetch(self.position, self.size).await
    }
    async fn copy<W: AsyncWrite + Unpin + Send + ?Sized>(&self, w: &mut W) -> Result<(), Error> {
        self.source.copy_to(self.position, self.size, w).await
    }
}
