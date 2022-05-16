use std::io::{Read, Seek, SeekFrom, Write};

use digest::Digest;
use sha2::Sha512;
use tracing::instrument;
use typed_builder::TypedBuilder;

use super::header::{FileSection, Header};
use super::manifest::Manifest;
use super::SIG_CONTEXT;
use crate::util::HashWriter;
use crate::{Error, ResultExt};

#[derive(TypedBuilder)]
pub struct S9pkPacker<
    'a,
    W: Write + Seek,
    RLicense: Read,
    RInstructions: Read,
    RIcon: Read,
    RDockerImages: Read,
    RAssets: Read,
    RScripts: Read,
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
        W: Write + Seek,
        RLicense: Read,
        RInstructions: Read,
        RIcon: Read,
        RDockerImages: Read,
        RAssets: Read,
        RScripts: Read,
    > S9pkPacker<'a, W, RLicense, RInstructions, RIcon, RDockerImages, RAssets, RScripts>
{
    /// BLOCKING
    #[instrument(skip(self))]
    pub fn pack(mut self, key: &ed25519_dalek::Keypair) -> Result<(), Error> {
        let header_pos = self.writer.stream_position()?;
        if header_pos != 0 {
            tracing::warn!("Appending to non-empty file.");
        }
        let mut header = Header::placeholder();
        header.serialize(&mut self.writer).with_ctx(|_| {
            (
                crate::ErrorKind::Serialization,
                "Writing Placeholder Header",
            )
        })?;
        let mut position = self.writer.stream_position()?;

        let mut writer = HashWriter::new(Sha512::new(), &mut self.writer);
        // manifest
        serde_cbor::ser::into_writer(self.manifest, &mut writer).with_ctx(|_| {
            (
                crate::ErrorKind::Serialization,
                "Serializing Manifest (CBOR)",
            )
        })?;
        let new_pos = writer.inner_mut().stream_position()?;
        header.table_of_contents.manifest = FileSection {
            position,
            length: new_pos - position,
        };
        position = new_pos;
        // license
        std::io::copy(&mut self.license, &mut writer)
            .with_ctx(|_| (crate::ErrorKind::Filesystem, "Copying License"))?;
        let new_pos = writer.inner_mut().stream_position()?;
        header.table_of_contents.license = FileSection {
            position,
            length: new_pos - position,
        };
        position = new_pos;
        // instructions
        std::io::copy(&mut self.instructions, &mut writer)
            .with_ctx(|_| (crate::ErrorKind::Filesystem, "Copying Instructions"))?;
        let new_pos = writer.inner_mut().stream_position()?;
        header.table_of_contents.instructions = FileSection {
            position,
            length: new_pos - position,
        };
        position = new_pos;
        // icon
        std::io::copy(&mut self.icon, &mut writer)
            .with_ctx(|_| (crate::ErrorKind::Filesystem, "Copying Icon"))?;
        let new_pos = writer.inner_mut().stream_position()?;
        header.table_of_contents.icon = FileSection {
            position,
            length: new_pos - position,
        };
        position = new_pos;
        // docker_images
        std::io::copy(&mut self.docker_images, &mut writer)
            .with_ctx(|_| (crate::ErrorKind::Filesystem, "Copying Docker Images"))?;
        let new_pos = writer.inner_mut().stream_position()?;
        header.table_of_contents.docker_images = FileSection {
            position,
            length: new_pos - position,
        };
        position = new_pos;
        // assets
        std::io::copy(&mut self.assets, &mut writer)
            .with_ctx(|_| (crate::ErrorKind::Filesystem, "Copying Assets"))?;
        let new_pos = writer.inner_mut().stream_position()?;
        header.table_of_contents.assets = FileSection {
            position,
            length: new_pos - position,
        };
        position = new_pos;
        // scripts
        if let Some(mut scripts) = self.scripts {
            std::io::copy(&mut scripts, &mut writer)
                .with_ctx(|_| (crate::ErrorKind::Filesystem, "Copying Scripts"))?;
            let new_pos = writer.inner_mut().stream_position()?;
            header.table_of_contents.scripts = Some(FileSection {
                position,
                length: new_pos - position,
            });
            position = new_pos;
        }

        // header
        let (hash, _) = writer.finish();
        self.writer.seek(SeekFrom::Start(header_pos))?;
        header.pubkey = key.public.clone();
        header.signature = key.sign_prehashed(hash, Some(SIG_CONTEXT))?;
        header
            .serialize(&mut self.writer)
            .with_ctx(|_| (crate::ErrorKind::Serialization, "Writing Header"))?;
        self.writer.seek(SeekFrom::Start(position))?;

        Ok(())
    }
}
