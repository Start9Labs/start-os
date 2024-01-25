use std::ffi::OsStr;
use std::fmt::Debug;
use std::path::Path;
use std::sync::Arc;

use futures::future::BoxFuture;
use futures::FutureExt;
use imbl::OrdMap;
use imbl_value::InternedString;
use itertools::Itertools;
use tokio::io::AsyncRead;

use crate::prelude::*;
use crate::s9pk::merkle_archive::hash::{Hash, HashWriter};
use crate::s9pk::merkle_archive::sink::{Sink, TrackingWriter};
use crate::s9pk::merkle_archive::source::{ArchiveSource, FileSource, Section};
use crate::s9pk::merkle_archive::write_queue::WriteQueue;
use crate::s9pk::merkle_archive::{varint, Entry, EntryContents};

#[derive(Clone)]
pub struct DirectoryContents<S> {
    contents: OrdMap<InternedString, Entry<S>>,
    /// used to optimize files to have earliest needed information up front
    sort_by: Option<Arc<dyn Fn(&str, &str) -> std::cmp::Ordering + Send + Sync>>,
}
impl<S: Debug> Debug for DirectoryContents<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DirectoryContents")
            .field("contents", &self.contents)
            .finish_non_exhaustive()
    }
}
impl<S> DirectoryContents<S> {
    pub fn new() -> Self {
        Self {
            contents: OrdMap::new(),
            sort_by: None,
        }
    }

    pub fn sort_by(
        &mut self,
        sort_by: impl Fn(&str, &str) -> std::cmp::Ordering + Send + Sync + 'static,
    ) {
        self.sort_by = Some(Arc::new(sort_by))
    }

    #[instrument(skip_all)]
    pub fn get_path(&self, path: impl AsRef<Path>) -> Option<&Entry<S>> {
        let mut dir = Some(self);
        let mut res = None;
        for segment in path.as_ref().into_iter() {
            let segment = segment.to_str()?;
            if segment == "/" {
                continue;
            }
            res = dir?.get(segment);
            if let Some(EntryContents::Directory(d)) = res.as_ref().map(|e| e.as_contents()) {
                dir = Some(d);
            } else {
                dir = None
            }
        }
        res
    }

    pub const fn header_size() -> u64 {
        8 // position: u64 BE
        + 8 // size: u64 BE
    }

    #[instrument(skip_all)]
    pub async fn serialize_header<W: Sink>(&self, position: u64, w: &mut W) -> Result<u64, Error> {
        use tokio::io::AsyncWriteExt;

        let size = self.toc_size();

        w.write_all(&position.to_be_bytes()).await?;
        w.write_all(&size.to_be_bytes()).await?;

        Ok(position)
    }

    pub fn toc_size(&self) -> u64 {
        self.iter().fold(
            varint::serialized_varint_size(self.len() as u64),
            |acc, (name, entry)| {
                acc + varint::serialized_varstring_size(&**name) + entry.header_size()
            },
        )
    }
}
impl<S: Clone> DirectoryContents<S> {
    pub fn with_stem(&self, stem: &str) -> impl Iterator<Item = (InternedString, Entry<S>)> {
        let prefix = InternedString::intern(stem);
        let (_, center, right) = self.split_lookup(&*stem);
        center.map(|e| (prefix.clone(), e)).into_iter().chain(
            right.into_iter().take_while(move |(k, _)| {
                Path::new(&**k).file_stem() == Some(OsStr::new(&*prefix))
            }),
        )
    }
    pub fn insert_path(&mut self, path: impl AsRef<Path>, entry: Entry<S>) -> Result<(), Error> {
        let path = path.as_ref();
        let (parent, Some(file)) = (path.parent(), path.file_name().and_then(|f| f.to_str()))
        else {
            return Err(Error::new(
                eyre!("cannot create file at root"),
                ErrorKind::Pack,
            ));
        };
        let mut dir = self;
        for segment in parent.into_iter().flatten() {
            let segment = segment
                .to_str()
                .ok_or_else(|| Error::new(eyre!("non-utf8 path segment"), ErrorKind::Utf8))?;
            if segment == "/" {
                continue;
            }
            if !dir.contains_key(segment) {
                dir.insert(
                    segment.into(),
                    Entry::new(EntryContents::Directory(DirectoryContents::new())),
                );
            }
            if let Some(EntryContents::Directory(d)) =
                dir.get_mut(segment).map(|e| e.as_contents_mut())
            {
                dir = d;
            } else {
                return Err(Error::new(eyre!("failed to insert entry at path {path:?}: ancestor exists and is not a directory"), ErrorKind::Pack));
            }
        }
        dir.insert(file.into(), entry);
        Ok(())
    }
}
impl<S: ArchiveSource> DirectoryContents<Section<S>> {
    #[instrument(skip_all)]
    pub fn deserialize<'a>(
        source: &'a S,
        header: &'a mut (impl AsyncRead + Unpin + Send),
        sighash: Hash,
    ) -> BoxFuture<'a, Result<Self, Error>> {
        async move {
            use tokio::io::AsyncReadExt;

            let mut position = [0u8; 8];
            header.read_exact(&mut position).await?;
            let position = u64::from_be_bytes(position);

            let mut size = [0u8; 8];
            header.read_exact(&mut size).await?;
            let size = u64::from_be_bytes(size);

            let mut toc_reader = source.fetch(position, size).await?;

            let len = varint::deserialize_varint(&mut toc_reader).await?;
            let mut entries = OrdMap::new();
            for _ in 0..len {
                entries.insert(
                    varint::deserialize_varstring(&mut toc_reader).await?.into(),
                    Entry::deserialize(source, &mut toc_reader).await?,
                );
            }

            let res = Self {
                contents: entries,
                sort_by: None,
            };

            if res.sighash().await? == sighash {
                Ok(res)
            } else {
                Err(Error::new(
                    eyre!("hash sum does not match"),
                    ErrorKind::InvalidSignature,
                ))
            }
        }
        .boxed()
    }
}
impl<S: FileSource> DirectoryContents<S> {
    pub fn filter(&mut self, filter: impl Fn(&Path) -> bool) -> Result<(), Error> {
        for k in self.keys().cloned().collect::<Vec<_>>() {
            let path = Path::new(&*k);
            if let Some(v) = self.get_mut(&k) {
                if !filter(path) {
                    if v.hash.is_none() {
                        return Err(Error::new(
                            eyre!("cannot filter out unhashed file, run `update_hashes` first"),
                            ErrorKind::InvalidRequest,
                        ));
                    }
                    v.contents = EntryContents::Missing;
                } else {
                    let filter: Box<dyn Fn(&Path) -> bool> = Box::new(|p| filter(&path.join(p)));
                    v.filter(filter)?;
                }
            }
        }
        Ok(())
    }
    #[instrument(skip_all)]
    pub fn update_hashes<'a>(&'a mut self, only_missing: bool) -> BoxFuture<'a, Result<(), Error>> {
        async move {
            for key in self.keys().cloned().collect::<Vec<_>>() {
                if let Some(entry) = self.get_mut(&key) {
                    entry.update_hash(only_missing).await?;
                }
            }
            Ok(())
        }
        .boxed()
    }

    #[instrument(skip_all)]
    pub fn sighash<'a>(&'a self) -> BoxFuture<'a, Result<Hash, Error>> {
        async move {
            let mut hasher = TrackingWriter::new(0, HashWriter::new());
            let mut sig_contents = OrdMap::new();
            for (name, entry) in &**self {
                sig_contents.insert(name.clone(), entry.to_missing().await?);
            }
            Self {
                contents: sig_contents,
                sort_by: None,
            }
            .serialize_toc(&mut WriteQueue::new(0), &mut hasher)
            .await?;
            Ok(hasher.into_inner().finalize())
        }
        .boxed()
    }

    #[instrument(skip_all)]
    pub async fn serialize_toc<'a, W: Sink>(
        &'a self,
        queue: &mut WriteQueue<'a, S>,
        w: &mut W,
    ) -> Result<(), Error> {
        varint::serialize_varint(self.len() as u64, w).await?;
        for (name, entry) in self.iter().sorted_by(|a, b| match (a, b, &self.sort_by) {
            ((_, a), (_, b), _) if a.as_contents().is_dir() && !b.as_contents().is_dir() => {
                std::cmp::Ordering::Less
            }
            ((_, a), (_, b), _) if !a.as_contents().is_dir() && b.as_contents().is_dir() => {
                std::cmp::Ordering::Greater
            }
            ((a, _), (b, _), Some(sort_by)) => sort_by(&***a, &***b),
            _ => std::cmp::Ordering::Equal,
        }) {
            varint::serialize_varstring(&**name, w).await?;
            entry.serialize_header(queue.add(entry).await?, w).await?;
        }

        Ok(())
    }
}
impl<S> std::ops::Deref for DirectoryContents<S> {
    type Target = OrdMap<InternedString, Entry<S>>;
    fn deref(&self) -> &Self::Target {
        &self.contents
    }
}
impl<S> std::ops::DerefMut for DirectoryContents<S> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.contents
    }
}
