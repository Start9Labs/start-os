use std::io::{Read, Seek, SeekFrom, Write};

use typed_builder::TypedBuilder;

use super::header::{FileSection, Header};
use super::manifest::Manifest;
use crate::{Error, ResultExt};

#[derive(TypedBuilder)]
pub struct S9pkPacker<
    'a,
    W: Write + Seek,
    RLicense: Read,
    RInstructions: Read,
    RIcon: Read,
    RDockerImages: Read,
> {
    writer: W,
    manifest: &'a Manifest,
    license: RLicense,
    instructions: RInstructions,
    icon: RIcon,
    docker_images: RDockerImages,
}
impl<
        'a,
        W: Write + Seek,
        RLicense: Read,
        RInstructions: Read,
        RIcon: Read,
        RDockerImages: Read,
    > S9pkPacker<'a, W, RLicense, RInstructions, RIcon, RDockerImages>
{
    /// BLOCKING
    pub fn pack(mut self) -> Result<(), Error> {
        let header_pos = self.writer.stream_position()?;
        if header_pos != 0 {
            log::warn!("Appending to non-empty file.");
        }
        let mut header = Header::placeholder();
        header.serialize(&mut self.writer).with_ctx(|_| {
            (
                crate::ErrorKind::Serialization,
                "Writing Placeholder Header",
            )
        })?;
        let mut position = self.writer.stream_position()?;
        // manifest
        serde_cbor::to_writer(&mut self.writer, self.manifest).with_ctx(|_| {
            (
                crate::ErrorKind::Serialization,
                "Serializing Manifest (CBOR)",
            )
        })?;
        let new_pos = self.writer.stream_position()?;
        header.table_of_contents.manifest = FileSection {
            position,
            length: new_pos - position,
        };
        position = new_pos;
        // license
        std::io::copy(&mut self.license, &mut self.writer)
            .with_ctx(|_| (crate::ErrorKind::Filesystem, "Copying License"))?;
        let new_pos = self.writer.stream_position()?;
        header.table_of_contents.license = FileSection {
            position,
            length: new_pos - position,
        };
        position = new_pos;
        // instructions
        std::io::copy(&mut self.instructions, &mut self.writer)
            .with_ctx(|_| (crate::ErrorKind::Filesystem, "Copying Instructions"))?;
        let new_pos = self.writer.stream_position()?;
        header.table_of_contents.instructions = FileSection {
            position,
            length: new_pos - position,
        };
        position = new_pos;
        // icon
        std::io::copy(&mut self.icon, &mut self.writer)
            .with_ctx(|_| (crate::ErrorKind::Filesystem, "Copying Icon"))?;
        let new_pos = self.writer.stream_position()?;
        header.table_of_contents.icon = FileSection {
            position,
            length: new_pos - position,
        };
        position = new_pos;
        // docker_images
        std::io::copy(&mut self.docker_images, &mut self.writer)
            .with_ctx(|_| (crate::ErrorKind::Filesystem, "Copying App Image"))?;
        let new_pos = self.writer.stream_position()?;
        header.table_of_contents.docker_images = FileSection {
            position,
            length: new_pos - position,
        };
        position = new_pos;
        // header
        self.writer.seek(SeekFrom::Start(header_pos))?;
        header
            .serialize(&mut self.writer)
            .with_ctx(|_| (crate::ErrorKind::Serialization, "Writing Header"))?;
        self.writer.seek(SeekFrom::Start(position))?;

        Ok(())
    }
}
