use std::io::SeekFrom;
use std::path::Path;
use std::pin::Pin;
use std::task::{Context, Poll};

use digest::Output;
use sha2::{Digest, Sha512};
use tokio::fs::File;
use tokio::io::{AsyncRead, AsyncReadExt, AsyncSeek, AsyncSeekExt, ReadBuf, Take};
use tracing::instrument;

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
    hash: Option<Output<Sha512>>,
    hash_string: Option<String>,
    toc: TableOfContents,
    pos: u64,
    rdr: R,
}
impl S9pkReader {
    pub async fn open<P: AsRef<Path>>(path: P, check_sig: bool) -> Result<Self, Error> {
        let p = path.as_ref();
        let rdr = File::open(p)
            .await
            .with_ctx(|_| (crate::error::ErrorKind::Filesystem, p.display().to_string()))?;

        Self::from_reader(rdr, check_sig).await
    }
}
impl<R: AsyncRead + AsyncSeek + Unpin> S9pkReader<InstallProgressTracker<R>> {
    pub fn validated(&mut self) {
        self.rdr.validated()
    }
}
impl<R: AsyncRead + AsyncSeek + Unpin> S9pkReader<R> {
    #[instrument(skip(self))]
    pub async fn validate(&mut self) -> Result<(), Error> {
        Ok(())
    }
    #[instrument(skip(rdr))]
    pub async fn from_reader(mut rdr: R, check_sig: bool) -> Result<Self, Error> {
        let header = Header::deserialize(&mut rdr).await?;

        let (hash, hash_string) = if check_sig {
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
            (
                Some(hash),
                Some(base32::encode(
                    base32::Alphabet::RFC4648 { padding: false },
                    hash.as_slice(),
                )),
            )
        } else {
            (None, None)
        };

        let pos = rdr.stream_position().await?;

        Ok(S9pkReader {
            hash_string,
            hash,
            toc: header.table_of_contents,
            pos,
            rdr,
        })
    }

    pub fn hash(&self) -> Option<&Output<Sha512>> {
        self.hash.as_ref()
    }

    pub fn hash_str(&self) -> Option<&str> {
        self.hash_string.as_ref().map(|s| s.as_str())
    }

    pub async fn reset(&mut self) -> Result<(), Error> {
        self.rdr.seek(SeekFrom::Start(0)).await?;
        Ok(())
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

    pub async fn manifest_raw<'a>(&'a mut self) -> Result<ReadHandle<'a, R>, Error> {
        self.read_handle(self.toc.manifest).await
    }

    pub async fn manifest(&mut self) -> Result<Manifest, Error> {
        let slice = self.manifest_raw().await?.to_vec().await?;
        serde_cbor::de::from_reader(slice.as_slice())
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

    pub async fn assets<'a>(&'a mut self) -> Result<ReadHandle<'a, R>, Error> {
        Ok(self.read_handle(self.toc.assets).await?)
    }
}
