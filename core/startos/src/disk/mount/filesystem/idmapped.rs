use std::ffi::OsStr;
use std::fmt::Display;
use std::os::unix::fs::MetadataExt;
use std::path::Path;
use std::str::FromStr;

use clap::Parser;
use clap::builder::ValueParserFactory;
use digest::generic_array::GenericArray;
use digest::{Digest, OutputSizeUser};
use models::FromStrParser;
use serde::{Deserialize, Serialize};
use sha2::Sha256;
use tokio::process::Command;
use ts_rs::TS;

use super::FileSystem;
use crate::prelude::*;
use crate::util::Invoke;

#[derive(Clone, Copy, Debug, Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
pub struct IdMap {
    pub from_id: u32,
    pub to_id: u32,
    pub range: u32,
}
impl IdMap {
    pub fn stack(a: Vec<IdMap>, b: Vec<IdMap>) -> Vec<IdMap> {
        let mut res = Vec::with_capacity(a.len() + b.len());
        res.extend_from_slice(&a);

        for mut b in b {
            for a in &a {
                if a.from_id <= b.from_id && a.from_id + a.range > b.from_id {
                    b.from_id += a.to_id;
                }
                if a.from_id <= b.to_id && a.from_id + a.range > b.to_id {
                    b.to_id += a.to_id;
                }
            }
            res.push(b);
        }

        res
    }
}
impl FromStr for IdMap {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let split = s.splitn(3, ":").collect::<Vec<_>>();
        if let Some([u, k, r]) = split.get(0..3) {
            Ok(Self {
                from_id: u.parse()?,
                to_id: k.parse()?,
                range: r.parse()?,
            })
        } else if let Some([u, k]) = split.get(0..2) {
            Ok(Self {
                from_id: u.parse()?,
                to_id: k.parse()?,
                range: 1,
            })
        } else {
            Err(Error::new(
                eyre!("{s} is not a valid idmap"),
                ErrorKind::ParseNumber,
            ))
        }
    }
}
impl ValueParserFactory for IdMap {
    type Parser = FromStrParser<IdMap>;
    fn value_parser() -> Self::Parser {
        <Self::Parser>::new()
    }
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct IdMapped<Fs: FileSystem> {
    filesystem: Fs,
    idmap: Vec<IdMap>,
}
impl<Fs: FileSystem> IdMapped<Fs> {
    pub fn new(filesystem: Fs, idmap: Vec<IdMap>) -> Self {
        Self { filesystem, idmap }
    }
}
impl<Fs: FileSystem> FileSystem for IdMapped<Fs> {
    fn mount_type(&self) -> Option<impl AsRef<str>> {
        self.filesystem.mount_type()
    }
    fn extra_args(&self) -> impl IntoIterator<Item = impl AsRef<OsStr>> {
        self.filesystem.extra_args()
    }
    fn mount_options(&self) -> impl IntoIterator<Item = impl Display> {
        self.filesystem
            .mount_options()
            .into_iter()
            .map(|a| Box::new(a) as Box<dyn Display>)
            .chain(if self.idmap.is_empty() {
                None
            } else {
                use std::fmt::Write;

                let mut option = "X-mount.idmap=".to_owned();
                for i in &self.idmap {
                    write!(&mut option, "b:{}:{}:{} ", i.from_id, i.to_id, i.range).unwrap();
                }
                Some(Box::new(option) as Box<dyn Display>)
            })
    }
    async fn source(&self) -> Result<Option<impl AsRef<Path>>, Error> {
        self.filesystem.source().await
    }
    async fn pre_mount(&self, mountpoint: &Path) -> Result<(), Error> {
        self.filesystem.pre_mount(mountpoint).await?;
        let info = tokio::fs::metadata(mountpoint).await?;
        for i in &self.idmap {
            let uid_in_range = i.from_id <= info.uid() && i.from_id + i.range > info.uid();
            let gid_in_range = i.from_id <= info.gid() && i.from_id + i.range > info.gid();
            if uid_in_range || gid_in_range {
                Command::new("chown")
                    .arg(format!(
                        "{uid}:{gid}",
                        uid = if uid_in_range {
                            i.to_id + info.uid() - i.from_id
                        } else {
                            info.uid()
                        },
                        gid = if gid_in_range {
                            i.to_id + info.gid() - i.from_id
                        } else {
                            info.gid()
                        },
                    ))
                    .arg(&mountpoint)
                    .invoke(crate::ErrorKind::Filesystem)
                    .await?;
            }
        }
        Ok(())
    }
    async fn source_hash(
        &self,
    ) -> Result<GenericArray<u8, <Sha256 as OutputSizeUser>::OutputSize>, Error> {
        let mut sha = Sha256::new();
        sha.update("IdMapped");
        sha.update(self.filesystem.source_hash().await?);
        sha.update(usize::to_be_bytes(self.idmap.len()));
        for i in &self.idmap {
            sha.update(u32::to_be_bytes(i.from_id));
            sha.update(u32::to_be_bytes(i.to_id));
            sha.update(u32::to_be_bytes(i.range));
        }
        Ok(sha.finalize())
    }
}
