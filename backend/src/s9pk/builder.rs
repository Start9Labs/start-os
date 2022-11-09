use nom::combinator::success;
use sha2_old::{Digest, Sha512};
use tokio::io::{AsyncReadExt, AsyncSeekExt, AsyncWriteExt, SeekFrom};
use tracing::instrument;
use typed_builder::TypedBuilder;

use super::header::{FileSection, Header};
use super::manifest::Manifest;
use super::SIG_CONTEXT;
use crate::util::io::to_cbor_async_writer;
use crate::util::HashWriter;
use crate::{Error, ResultExt};

#[derive(TypedBuilder)]
pub struct S9pkPacker<
    'a,
    W: AsyncWriteExt + AsyncSeekExt,
    RLicense: AsyncReadExt + Unpin,
    RInstructions: AsyncReadExt + Unpin,
    RIcon: AsyncReadExt + Unpin,
    RDockerImages: AsyncReadExt + Unpin,
    RAssets: AsyncReadExt + Unpin,
    RScripts: AsyncReadExt + Unpin,
> {
    writer: W,
    manifest: &'a Manifest,
    license: RLicense,
    instructions: RInstructions,
    icon: RIcon,
    docker_images: RDockerImages,
    assets: RAssets,
    scripts: Option<RScripts>,
}
impl<
        'a,
        W: AsyncWriteExt + AsyncSeekExt + Unpin,
        RLicense: AsyncReadExt + Unpin,
        RInstructions: AsyncReadExt + Unpin,
        RIcon: AsyncReadExt + Unpin,
        RDockerImages: AsyncReadExt + Unpin,
        RAssets: AsyncReadExt + Unpin,
        RScripts: AsyncReadExt + Unpin,
    > S9pkPacker<'a, W, RLicense, RInstructions, RIcon, RDockerImages, RAssets, RScripts>
{
    /// BLOCKING
    #[instrument(skip(self))]
    pub async fn pack(mut self, key: &ed25519_dalek::Keypair) -> Result<(), Error> {
        let header_pos = self.writer.stream_position().await?;
        if header_pos != 0 {
            tracing::warn!("Appending to non-empty file.");
        }
        let mut header = Header::placeholder();
        header.serialize(&mut self.writer).await.with_ctx(|_| {
            (
                crate::ErrorKind::Serialization,
                "Writing Placeholder Header",
            )
        })?;
        let mut position = self.writer.stream_position().await?;

        let mut writer = HashWriter::new(Sha512::new(), &mut self.writer);
        // manifest
        to_cbor_async_writer(&mut writer, self.manifest).await?;
        let new_pos = writer.inner_mut().stream_position().await?;
        header.table_of_contents.manifest = FileSection {
            position,
            length: new_pos - position,
        };
        position = new_pos;
        // license
        tokio::io::copy(&mut self.license, &mut writer)
            .await
            .with_ctx(|_| (crate::ErrorKind::Filesystem, "Copying License"))?;
        let new_pos = writer.inner_mut().stream_position().await?;
        header.table_of_contents.license = FileSection {
            position,
            length: new_pos - position,
        };
        position = new_pos;
        // instructions
        tokio::io::copy(&mut self.instructions, &mut writer)
            .await
            .with_ctx(|_| (crate::ErrorKind::Filesystem, "Copying Instructions"))?;
        let new_pos = writer.inner_mut().stream_position().await?;
        header.table_of_contents.instructions = FileSection {
            position,
            length: new_pos - position,
        };
        position = new_pos;
        // icon
        tokio::io::copy(&mut self.icon, &mut writer)
            .await
            .with_ctx(|_| (crate::ErrorKind::Filesystem, "Copying Icon"))?;
        let new_pos = writer.inner_mut().stream_position().await?;
        header.table_of_contents.icon = FileSection {
            position,
            length: new_pos - position,
        };
        position = new_pos;
        // docker_images
        tokio::io::copy(&mut self.docker_images, &mut writer)
            .await
            .with_ctx(|_| (crate::ErrorKind::Filesystem, "Copying Docker Images"))?;
        let new_pos = writer.inner_mut().stream_position().await?;
        header.table_of_contents.docker_images = FileSection {
            position,
            length: new_pos - position,
        };
        position = new_pos;
        // assets
        tokio::io::copy(&mut self.assets, &mut writer)
            .await
            .with_ctx(|_| (crate::ErrorKind::Filesystem, "Copying Assets"))?;
        let new_pos = writer.inner_mut().stream_position().await?;
        header.table_of_contents.assets = FileSection {
            position,
            length: new_pos - position,
        };
        position = new_pos;
        // scripts
        if let Some(mut scripts) = self.scripts {
            tokio::io::copy(&mut scripts, &mut writer)
                .await
                .with_ctx(|_| (crate::ErrorKind::Filesystem, "Copying Scripts"))?;
            let new_pos = writer.inner_mut().stream_position().await?;
            header.table_of_contents.scripts = Some(FileSection {
                position,
                length: new_pos - position,
            });
            position = new_pos;
        }

        // header
        let (hash, _) = writer.finish();
        self.writer.seek(SeekFrom::Start(header_pos)).await?;
        header.pubkey = key.public.clone();
        header.signature = key.sign_prehashed(hash, Some(SIG_CONTEXT))?;
        header
            .serialize(&mut self.writer)
            .await
            .with_ctx(|_| (crate::ErrorKind::Serialization, "Writing Header"))?;
        self.writer.seek(SeekFrom::Start(position)).await?;

        Ok(())
    }
}
