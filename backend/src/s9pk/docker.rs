use std::borrow::Cow;
use std::collections::BTreeSet;
use std::io::SeekFrom;
use std::path::Path;

use color_eyre::eyre::eyre;
use futures::{FutureExt, TryStreamExt};
use serde::{Deserialize, Serialize};
use tokio::io::{AsyncRead, AsyncSeek, AsyncSeekExt};
use tokio_tar::{Archive, Entry};

use crate::util::io::from_cbor_async_reader;
use crate::{Error, ErrorKind, ARCH};

#[derive(Default, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct DockerMultiArch {
    pub default: String,
    pub available: BTreeSet<String>,
}

#[pin_project::pin_project(project = DockerReaderProject)]
#[derive(Debug)]
pub enum DockerReader<R: AsyncRead + Unpin> {
    SingleArch(#[pin] R),
    MultiArch(#[pin] Entry<Archive<R>>),
}
impl<R: AsyncRead + AsyncSeek + Unpin + Send + Sync> DockerReader<R> {
    pub async fn new(mut rdr: R) -> Result<Self, Error> {
        let arch = if let Some(multiarch) = tokio_tar::Archive::new(&mut rdr)
            .entries()?
            .try_filter_map(|e| {
                async move {
                    Ok(if &*e.path()? == Path::new("multiarch.cbor") {
                        Some(e)
                    } else {
                        None
                    })
                }
                .boxed()
            })
            .try_next()
            .await?
        {
            let multiarch: DockerMultiArch = from_cbor_async_reader(multiarch).await?;
            Some(if multiarch.available.contains(&**ARCH) {
                Cow::Borrowed(&**ARCH)
            } else {
                Cow::Owned(multiarch.default)
            })
        } else {
            None
        };
        rdr.seek(SeekFrom::Start(0)).await?;
        if let Some(arch) = arch {
            if let Some(image) = tokio_tar::Archive::new(rdr)
                .entries()?
                .try_filter_map(|e| {
                    let arch = arch.clone();
                    async move {
                        Ok(if &*e.path()? == Path::new(&format!("{}.tar", arch)) {
                            Some(e)
                        } else {
                            None
                        })
                    }
                    .boxed()
                })
                .try_next()
                .await?
            {
                Ok(Self::MultiArch(image))
            } else {
                Err(Error::new(
                    eyre!("Docker image section does not contain tarball for architecture"),
                    ErrorKind::ParseS9pk,
                ))
            }
        } else {
            Ok(Self::SingleArch(rdr))
        }
    }
}
impl<R: AsyncRead + Unpin + Send + Sync> AsyncRead for DockerReader<R> {
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut tokio::io::ReadBuf<'_>,
    ) -> std::task::Poll<std::io::Result<()>> {
        match self.project() {
            DockerReaderProject::SingleArch(r) => r.poll_read(cx, buf),
            DockerReaderProject::MultiArch(r) => r.poll_read(cx, buf),
        }
    }
}
