use std::collections::BTreeSet;
use std::io::SeekFrom;
use std::ops::Range;
use std::path::Path;
use std::pin::Pin;
use std::str::FromStr;
use std::task::{Context, Poll};

use color_eyre::eyre::eyre;
use digest_old::Output;
use ed25519_dalek::PublicKey;
use futures::TryStreamExt;
use sha2_old::{Digest, Sha512};
use tokio::fs::File;
use tokio::io::{AsyncRead, AsyncReadExt, AsyncSeek, AsyncSeekExt, ReadBuf};
use tracing::instrument;

use super::header::{FileSection, Header, TableOfContents};
use super::manifest::{Manifest, PackageId};
use super::SIG_CONTEXT;
use crate::id::ImageId;
use crate::install::progress::InstallProgressTracker;
use crate::s9pk::docker::DockerReader;
use crate::util::Version;
use crate::{Error, ResultExt};

#[pin_project::pin_project]
#[derive(Debug)]
pub struct ReadHandle<'a, R = File> {
    pos: &'a mut u64,
    range: Range<u64>,
    #[pin]
    rdr: &'a mut R,
}
impl<'a, R: AsyncRead + Unpin> ReadHandle<'a, R> {
    pub async fn to_vec(mut self) -> std::io::Result<Vec<u8>> {
        let mut buf = vec![0; (self.range.end - self.range.start) as usize];
        self.read_exact(&mut buf).await?;
        Ok(buf)
    }
}
impl<'a, R: AsyncRead + Unpin> AsyncRead for ReadHandle<'a, R> {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> Poll<std::io::Result<()>> {
        let this = self.project();
        let start = buf.filled().len();
        let mut take_buf = buf.take(this.range.end.saturating_sub(**this.pos) as usize);
        let res = AsyncRead::poll_read(this.rdr, cx, &mut take_buf);
        let n = take_buf.filled().len();
        unsafe { buf.assume_init(start + n) };
        buf.advance(n);
        **this.pos += n as u64;
        res
    }
}
impl<'a, R: AsyncSeek + Unpin> AsyncSeek for ReadHandle<'a, R> {
    fn start_seek(self: Pin<&mut Self>, position: SeekFrom) -> std::io::Result<()> {
        let this = self.project();
        AsyncSeek::start_seek(
            this.rdr,
            match position {
                SeekFrom::Current(n) => SeekFrom::Current(n),
                SeekFrom::End(n) => SeekFrom::Start((this.range.end as i64 + n) as u64),
                SeekFrom::Start(n) => SeekFrom::Start(this.range.start + n),
            },
        )
    }
    fn poll_complete(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<std::io::Result<u64>> {
        let this = self.project();
        match AsyncSeek::poll_complete(this.rdr, cx) {
            Poll::Ready(Ok(n)) => {
                let res = n.saturating_sub(this.range.start);
                **this.pos = this.range.start + res;
                Poll::Ready(Ok(res))
            }
            a => a,
        }
    }
}

#[derive(Debug)]
pub struct ImageTag {
    pub package_id: PackageId,
    pub image_id: ImageId,
    pub version: Version,
}
impl ImageTag {
    #[instrument]
    pub fn validate(&self, id: &PackageId, version: &Version) -> Result<(), Error> {
        if id != &self.package_id {
            return Err(Error::new(
                eyre!(
                    "Contains image for incorrect package: id {}",
                    self.package_id,
                ),
                crate::ErrorKind::ValidateS9pk,
            ));
        }
        if version != &self.version {
            return Err(Error::new(
                eyre!(
                    "Contains image with incorrect version: expected {} received {}",
                    version,
                    self.version,
                ),
                crate::ErrorKind::ValidateS9pk,
            ));
        }
        Ok(())
    }
}
impl FromStr for ImageTag {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let rest = s.strip_prefix("start9/").ok_or_else(|| {
            Error::new(
                eyre!("Invalid image tag prefix: expected start9/"),
                crate::ErrorKind::ValidateS9pk,
            )
        })?;
        let (package, rest) = rest.split_once("/").ok_or_else(|| {
            Error::new(
                eyre!("Image tag missing image id"),
                crate::ErrorKind::ValidateS9pk,
            )
        })?;
        let (image, version) = rest.split_once(":").ok_or_else(|| {
            Error::new(
                eyre!("Image tag missing version"),
                crate::ErrorKind::ValidateS9pk,
            )
        })?;
        Ok(ImageTag {
            package_id: package.parse()?,
            image_id: image.parse()?,
            version: version.parse()?,
        })
    }
}

pub struct S9pkReader<R: AsyncRead + AsyncSeek + Unpin + Send + Sync = File> {
    hash: Option<Output<Sha512>>,
    hash_string: Option<String>,
    developer_key: PublicKey,
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
impl<R: AsyncRead + AsyncSeek + Unpin + Send + Sync> S9pkReader<InstallProgressTracker<R>> {
    pub fn validated(&mut self) {
        self.rdr.validated()
    }
}
impl<R: AsyncRead + AsyncSeek + Unpin + Send + Sync> S9pkReader<R> {
    #[instrument(skip(self))]
    pub async fn validate(&mut self) -> Result<(), Error> {
        if self.toc.icon.length > 102_400 {
            // 100 KiB
            return Err(Error::new(
                eyre!("icon must be less than 100KiB"),
                crate::ErrorKind::ValidateS9pk,
            ));
        }
        let image_tags = self.image_tags().await?;
        let man = self.manifest().await?;
        let containers = &man.containers;
        let validated_image_ids = image_tags
            .into_iter()
            .map(|i| i.validate(&man.id, &man.version).map(|_| i.image_id))
            .collect::<Result<BTreeSet<ImageId>, _>>()?;
        man.description.validate()?;
        man.actions
            .0
            .iter()
            .map(|(_, action)| {
                action.validate(
                    containers,
                    &man.eos_version,
                    &man.volumes,
                    &validated_image_ids,
                )
            })
            .collect::<Result<(), Error>>()?;
        man.backup.validate(
            containers,
            &man.eos_version,
            &man.volumes,
            &validated_image_ids,
        )?;
        if let Some(cfg) = &man.config {
            cfg.validate(
                containers,
                &man.eos_version,
                &man.volumes,
                &validated_image_ids,
            )?;
        }
        man.health_checks.validate(
            containers,
            &man.eos_version,
            &man.volumes,
            &validated_image_ids,
        )?;
        man.interfaces.validate()?;
        man.main
            .validate(
                containers,
                &man.eos_version,
                &man.volumes,
                &validated_image_ids,
                false,
            )
            .with_ctx(|_| (crate::ErrorKind::ValidateS9pk, "Main"))?;
        man.migrations.validate(
            containers,
            &man.eos_version,
            &man.volumes,
            &validated_image_ids,
        )?;

        #[cfg(feature = "js_engine")]
        if man.containers.is_some()
            || matches!(man.main, crate::procedure::PackageProcedure::Script(_))
        {
            return Err(Error::new(
                eyre!("Right now we don't support the containers and the long running main"),
                crate::ErrorKind::ValidateS9pk,
            ));
        }

        if man.containers.is_some()
            && matches!(man.main, crate::procedure::PackageProcedure::Docker(_))
        {
            return Err(Error::new(
                eyre!("Cannot have a main docker and a main in containers"),
                crate::ErrorKind::ValidateS9pk,
            ));
        }
        if let Some(props) = &man.properties {
            props
                .validate(
                    containers,
                    &man.eos_version,
                    &man.volumes,
                    &validated_image_ids,
                    true,
                )
                .with_ctx(|_| (crate::ErrorKind::ValidateS9pk, "Properties"))?;
        }
        man.volumes.validate(&man.interfaces)?;

        Ok(())
    }
    #[instrument(skip(self))]
    pub async fn image_tags(&mut self) -> Result<Vec<ImageTag>, Error> {
        let mut tar = tokio_tar::Archive::new(self.docker_images().await?);
        let mut entries = tar.entries()?;
        while let Some(mut entry) = entries.try_next().await? {
            if &*entry.path()? != Path::new("manifest.json") {
                continue;
            }
            let mut buf = Vec::with_capacity(entry.header().size()? as usize);
            entry.read_to_end(&mut buf).await?;
            #[derive(serde::Deserialize)]
            struct ManEntry {
                #[serde(rename = "RepoTags")]
                tags: Vec<String>,
            }
            let man_entries = serde_json::from_slice::<Vec<ManEntry>>(&buf)
                .with_ctx(|_| (crate::ErrorKind::Deserialization, "manifest.json"))?;
            return man_entries
                .iter()
                .flat_map(|e| &e.tags)
                .map(|t| t.parse())
                .collect();
        }
        Err(Error::new(
            eyre!("image.tar missing manifest.json"),
            crate::ErrorKind::ParseS9pk,
        ))
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
            developer_key: header.pubkey,
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

    pub fn developer_key(&self) -> &PublicKey {
        &self.developer_key
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
            range: self.pos..(self.pos + section.length),
            pos: &mut self.pos,
            rdr: &mut self.rdr,
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

    pub async fn docker_images<'a>(&'a mut self) -> Result<DockerReader<ReadHandle<'a, R>>, Error> {
        DockerReader::new(self.read_handle(self.toc.docker_images).await?).await
    }

    pub async fn assets<'a>(&'a mut self) -> Result<ReadHandle<'a, R>, Error> {
        Ok(self.read_handle(self.toc.assets).await?)
    }

    pub async fn scripts<'a>(&'a mut self) -> Result<Option<ReadHandle<'a, R>>, Error> {
        Ok(match self.toc.scripts {
            None => None,
            Some(a) => Some(self.read_handle(a).await?),
        })
    }
}
