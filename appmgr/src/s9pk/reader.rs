use std::io::SeekFrom;
use std::path::Path;
use std::pin::Pin;
use std::task::{Context, Poll};

use digest::Output;
use sha2::{Digest, Sha512};
use tokio::fs::File;
use tokio::io::{AsyncRead, AsyncReadExt, AsyncSeek, AsyncSeekExt, ReadBuf, Take};

use super::header::{FileSection, Header, TableOfContents};
use super::manifest::Manifest;
use super::SIG_CONTEXT;
use crate::install::progress::InstallProgressTracker;
use crate::{Error, ResultExt};

#[pin_project::pin_project]
pub struct ReadHandle<'a, R: AsyncRead + AsyncSeek + Unpin = File> {
    pos: &'a mut u64,
    #[pin]
    rdr: Take<&'a mut R>,
}
impl<'a, R: AsyncRead + AsyncSeek + Unpin> ReadHandle<'a, R> {
    pub async fn to_vec(mut self) -> std::io::Result<Vec<u8>> {
        let mut buf = vec![0; self.rdr.limit() as usize];
        self.rdr.read_exact(&mut buf).await?;
        Ok(buf)
    }
}
impl<'a, R: AsyncRead + AsyncSeek + Unpin> AsyncRead for ReadHandle<'a, R> {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> Poll<std::io::Result<()>> {
        let start = buf.filled().len();
        let this = self.project();
        let pos = this.pos;
        AsyncRead::poll_read(this.rdr, cx, buf).map(|res| {
            **pos += (buf.filled().len() - start) as u64;
            res
        })
    }
}

pub struct S9pkReader<R: AsyncRead + AsyncSeek + Unpin = File> {
    hash: Output<Sha512>,
    hash_string: String,
    toc: TableOfContents,
    pos: u64,
    rdr: R,
}
impl S9pkReader {
    pub async fn open<P: AsRef<Path>>(path: P) -> Result<Self, Error> {
        let p = path.as_ref();
        let rdr = File::open(p)
            .await
            .with_ctx(|_| (crate::error::ErrorKind::Filesystem, p.display().to_string()))?;

        Self::from_reader(rdr).await
    }
}
impl<R: AsyncRead + AsyncSeek + Unpin> S9pkReader<InstallProgressTracker<R>> {
    pub fn validated(&mut self) {
        self.rdr.validated()
    }
}
impl<R: AsyncRead + AsyncSeek + Unpin> S9pkReader<R> {
    pub async fn validate(&mut self) -> Result<(), Error> {
        todo!()
    }
    pub async fn from_reader(mut rdr: R) -> Result<Self, Error> {
        let header = Header::deserialize(&mut rdr).await?;
        let pos = rdr.stream_position().await?;

        let mut hasher = Sha512::new();
        let mut buf = [0; 1024];
        let mut read;
        while {
            read = rdr.read(&mut buf).await?;
            read != 0
        } {
            hasher.update(&buf[0..read]);
        }
        let hash = hasher.clone().finalize();
        header
            .pubkey
            .verify_prehashed(hasher, Some(SIG_CONTEXT), &header.signature)?;

        Ok(S9pkReader {
            hash_string: base32::encode(
                base32::Alphabet::RFC4648 { padding: false },
                hash.as_slice(),
            ),
            hash,
            toc: header.table_of_contents,
            pos,
            rdr,
        })
    }

    pub fn hash(&self) -> &Output<Sha512> {
        &self.hash
    }

    pub fn hash_str(&self) -> &str {
        self.hash_string.as_str()
    }

    async fn read_handle<'a>(
        &'a mut self,
        section: FileSection,
    ) -> Result<ReadHandle<'a, R>, Error> {
        if self.pos != section.position {
            self.rdr.seek(SeekFrom::Start(section.position)).await?;
            self.pos = section.position;
        }
        Ok(ReadHandle {
            pos: &mut self.pos,
            rdr: (&mut self.rdr).take(section.length),
        })
    }

    pub async fn manifest(&mut self) -> Result<Manifest, Error> {
        serde_cbor::from_slice(&self.read_handle(self.toc.manifest).await?.to_vec().await?)
            .with_ctx(|_| (crate::ErrorKind::ParseS9pk, "Deserializing Manifest (CBOR)"))
    }

    pub async fn license<'a>(&'a mut self) -> Result<ReadHandle<'a, R>, Error> {
        Ok(self.read_handle(self.toc.license).await?)
    }

    pub async fn instructions<'a>(&'a mut self) -> Result<ReadHandle<'a, R>, Error> {
        Ok(self.read_handle(self.toc.instructions).await?)
    }

    pub async fn icon<'a>(&'a mut self) -> Result<ReadHandle<'a, R>, Error> {
        Ok(self.read_handle(self.toc.icon).await?)
    }

    pub async fn docker_images<'a>(&'a mut self) -> Result<ReadHandle<'a, R>, Error> {
        Ok(self.read_handle(self.toc.docker_images).await?)
    }
}
