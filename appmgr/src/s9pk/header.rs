use std::collections::HashMap;
use std::io::Write;

use anyhow::anyhow;
use ed25519_dalek::{PublicKey, Signature};
use tokio::io::{AsyncRead, AsyncReadExt};

use crate::Error;

pub const MAGIC: [u8; 2] = [59, 59];
pub const VERSION: u8 = 1;

pub struct Header {
    pub pubkey: PublicKey,
    pub signature: Signature,
    pub table_of_contents: TableOfContents,
}
impl Header {
    pub fn placeholder() -> Self {
        Header {
            pubkey: PublicKey::default(),
            signature: Signature::new([0; 64]),
            table_of_contents: Default::default(),
        }
    }
    // MUST BE SAME SIZE REGARDLESS OF DATA
    pub fn serialize<W: Write>(&self, mut writer: W) -> std::io::Result<()> {
        writer.write_all(&MAGIC)?;
        writer.write_all(&[VERSION])?;
        writer.write_all(self.pubkey.as_bytes())?;
        writer.write_all(self.signature.as_ref())?;
        self.table_of_contents.serialize(writer)?;
        Ok(())
    }
    pub async fn deserialize<R: AsyncRead + Unpin>(mut reader: R) -> Result<Self, Error> {
        let mut magic = [0; 2];
        reader.read_exact(&mut magic).await?;
        if magic != MAGIC {
            return Err(Error::new(
                anyhow!("Incorrect Magic"),
                crate::ErrorKind::ParseS9pk,
            ));
        }
        let mut version = [0];
        reader.read_exact(&mut version).await?;
        if version[0] != VERSION {
            return Err(Error::new(
                anyhow!("Unknown Version"),
                crate::ErrorKind::ParseS9pk,
            ));
        }
        let mut pubkey_bytes = [0; 32];
        reader.read_exact(&mut pubkey_bytes).await?;
        let pubkey = PublicKey::from_bytes(&pubkey_bytes)
            .map_err(|e| Error::new(e, crate::ErrorKind::ParseS9pk))?;
        let mut sig_bytes = [0; 64];
        reader.read_exact(&mut sig_bytes).await?;
        let signature = Signature::new(sig_bytes);
        let table_of_contents = TableOfContents::deserialize(reader).await?;

        Ok(Header {
            pubkey,
            signature,
            table_of_contents,
        })
    }
}

#[derive(Debug, Default)]
pub struct TableOfContents {
    pub manifest: FileSection,
    pub license: FileSection,
    pub instructions: FileSection,
    pub icon: FileSection,
    pub docker_images: FileSection,
}
impl TableOfContents {
    pub fn serialize<W: Write>(&self, mut writer: W) -> std::io::Result<()> {
        let len: u32 = 16 // size of FileSection
            * (
                1 + // manifest
                1 + // license
                1 + // instructions
                1 + // icon
                1 // docker_images
            );
        writer.write_all(&u32::to_be_bytes(len))?;
        self.manifest.serialize_entry("manifest", &mut writer)?;
        self.license.serialize_entry("license", &mut writer)?;
        self.instructions
            .serialize_entry("instructions", &mut writer)?;
        self.icon.serialize_entry("icon", &mut writer)?;
        self.docker_images
            .serialize_entry("docker_images", &mut writer)?;
        Ok(())
    }
    pub async fn deserialize<R: AsyncRead + Unpin>(mut reader: R) -> std::io::Result<Self> {
        let mut toc_len = [0; 4];
        reader.read_exact(&mut toc_len).await?;
        let toc_len = u32::from_be_bytes(toc_len);
        let mut reader = reader.take(toc_len as u64);
        let mut table = HashMap::new();
        while let Some((label, section)) = FileSection::deserialize_entry(&mut reader).await? {
            table.insert(label, section);
        }
        fn from_table(
            table: &HashMap<Vec<u8>, FileSection>,
            label: &str,
        ) -> std::io::Result<FileSection> {
            table.get(label.as_bytes()).copied().ok_or_else(|| {
                std::io::Error::new(
                    std::io::ErrorKind::UnexpectedEof,
                    format!("Missing Required Label: {}", label),
                )
            })
        }
        fn as_opt(fs: FileSection) -> Option<FileSection> {
            if fs.position | fs.length == 0 {
                // 0/0 is not a valid file section
                None
            } else {
                Some(fs)
            }
        }
        Ok(TableOfContents {
            manifest: from_table(&table, "manifest")?,
            license: from_table(&table, "license")?,
            instructions: from_table(&table, "instructions")?,
            icon: from_table(&table, "icon")?,
            docker_images: from_table(&table, "docker_images")?,
        })
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct FileSection {
    pub position: u64,
    pub length: u64,
}
impl FileSection {
    pub fn serialize_entry<W: Write>(self, label: &str, mut writer: W) -> std::io::Result<()> {
        writer.write_all(&[label.len() as u8])?;
        writer.write_all(label.as_bytes())?;
        writer.write_all(&u64::to_be_bytes(self.position))?;
        writer.write_all(&u64::to_be_bytes(self.length))?;
        Ok(())
    }
    pub async fn deserialize_entry<R: AsyncRead + Unpin>(
        mut reader: R,
    ) -> std::io::Result<Option<(Vec<u8>, Self)>> {
        let mut label_len = [0];
        let read = reader.read(&mut label_len).await?;
        if read == 0 {
            return Ok(None);
        }
        let label = vec![0; label_len[0] as usize];
        let mut pos = [0; 8];
        reader.read_exact(&mut pos).await?;
        let mut len = [0; 8];
        reader.read_exact(&mut len).await?;
        Ok(Some((
            label,
            FileSection {
                position: u64::from_be_bytes(pos),
                length: u64::from_be_bytes(len),
            },
        )))
    }
}
