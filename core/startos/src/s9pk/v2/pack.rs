use std::collections::BTreeMap;
use std::fs::Metadata;
use std::io::Cursor;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::Arc;
use std::time::SystemTime;

use clap::Parser;
use futures::future::BoxFuture;
use futures::{FutureExt, TryStreamExt};
use imbl_value::InternedString;
use models::{ImageId, PackageId};
use serde::{Deserialize, Serialize};
use tokio::fs::File;
use tokio::io::AsyncRead;
use tokio::process::Command;
use tokio::sync::OnceCell;
use tokio_stream::wrappers::ReadDirStream;
use ts_rs::TS;

use crate::context::CliContext;
use crate::prelude::*;
use crate::rpc_continuations::Guid;
use crate::s9pk::merkle_archive::directory_contents::DirectoryContents;
use crate::s9pk::merkle_archive::source::multi_cursor_file::MultiCursorFile;
use crate::s9pk::merkle_archive::source::{into_dyn_read, ArchiveSource, FileSource};
use crate::s9pk::merkle_archive::{Entry, MerkleArchive};
use crate::s9pk::v2::compat::CONTAINER_TOOL;
use crate::s9pk::v2::SIG_CONTEXT;
use crate::s9pk::S9pk;
use crate::util::serde::IoFormat;
use crate::util::Invoke;

fn modified<'a>(
    path: &'a Path,
    metadata: &'a Metadata,
) -> BoxFuture<'a, Result<SystemTime, Error>> {
    async move {
        let mut top = metadata.modified()?;
        if metadata.is_dir() {
            let mut dir = tokio::fs::read_dir(path).await?;
            while let Some(file) = dir.next_entry().await? {
                let metadata = file.metadata().await?;
                let mtime = modified(&file.path(), &metadata).await?;
                if mtime > top {
                    top = mtime;
                }
            }
        }
        Ok(top)
    }
    .boxed()
}

struct SqfsDir {
    path: PathBuf,
    sqfs: OnceCell<MultiCursorFile>,
}
impl SqfsDir {
    async fn file(&self) -> Result<&MultiCursorFile, Error> {
        self.sqfs
            .get_or_try_init(|| async move {
                let guid = Guid::new();
                let path = Path::new("/var/tmp/startos")
                    .join(guid.as_ref())
                    .with_extension("squashfs");
                let mut cmd = Command::new("mksquashfs");
                if self.path.extension().and_then(|s| s.to_str()) == Some("tar") {
                    cmd.arg("-tar");
                }
                cmd.arg(&self.path)
                    .arg(&path)
                    .invoke(ErrorKind::Filesystem)
                    .await?;
                Ok(MultiCursorFile::from(File::open(&path).await?))
            })
            .await
    }
}
impl From<PathBuf> for SqfsDir {
    fn from(value: PathBuf) -> Self {
        Self {
            path: value,
            sqfs: OnceCell::new(),
        }
    }
}

#[derive(Clone)]
enum PackSource {
    Buffered(Arc<[u8]>),
    File(PathBuf),
    Squashfs(Arc<SqfsDir>),
}
impl FileSource for PackSource {
    type Reader = Box<dyn AsyncRead + Unpin + Send + Sync + 'static>;
    async fn size(&self) -> Result<u64, Error> {
        match self {
            Self::Buffered(a) => Ok(a.len() as u64),
            Self::File(f) => Ok(tokio::fs::metadata(f).await?.len()),
            Self::Squashfs(dir) => dir.file().await?.size().await.or_not_found("file metadata"),
        }
    }
    async fn reader(&self) -> Result<Self::Reader, Error> {
        match self {
            Self::Buffered(a) => Ok(into_dyn_read(Cursor::new(a.clone()))),
            Self::File(f) => Ok(into_dyn_read(File::open(f).await?)),
            Self::Squashfs(dir) => dir.file().await?.fetch_all().await.map(into_dyn_read),
        }
    }
}

#[derive(Deserialize, Serialize, Parser)]
pub struct PackParams {
    pub path: Option<PathBuf>,
    #[arg(short = 'o', long = "output")]
    pub output: Option<PathBuf>,
    #[arg(long = "javascript")]
    pub javascript: Option<PathBuf>,
    #[arg(long = "icon")]
    pub icon: Option<PathBuf>,
    #[arg(long = "license")]
    pub license: Option<PathBuf>,
    #[arg(long = "instructions")]
    pub instructions: Option<PathBuf>,
    #[arg(long = "assets")]
    pub assets: Option<PathBuf>,
}
impl PackParams {
    fn path(&self) -> &Path {
        self.path.as_deref().unwrap_or(Path::new("."))
    }
    fn output(&self, id: &PackageId) -> PathBuf {
        self.output
            .as_ref()
            .cloned()
            .unwrap_or_else(|| self.path().join(id).with_extension("s9pk"))
    }
    fn javascript(&self) -> PathBuf {
        self.javascript
            .as_ref()
            .cloned()
            .unwrap_or_else(|| self.path().join("javascript"))
    }
    async fn icon(&self) -> Result<PathBuf, Error> {
        if let Some(icon) = &self.icon {
            Ok(icon.clone())
        } else {
            ReadDirStream::new(tokio::fs::read_dir(self.path()).await?).map_err(Error::from).try_fold(Err(Error::new(eyre!("icon not found"), ErrorKind::NotFound)), |acc, x| async move { match acc {
                Ok(_) => Err(Error::new(eyre!("multiple icons found in working directory, please specify which to use with `--icon`"), ErrorKind::InvalidRequest)),
                Err(e) => Ok({
                    let path = x.path();
                    if path.file_stem().and_then(|s| s.to_str()) == Some("icon") {
                    Ok(path)
                    } else {
                        Err(e)
                    }
                })
            }}).await?
        }
    }
    fn license(&self) -> PathBuf {
        self.license
            .as_ref()
            .cloned()
            .unwrap_or_else(|| self.path().join("LICENSE.md"))
    }
    fn instructions(&self) -> PathBuf {
        self.instructions
            .as_ref()
            .cloned()
            .unwrap_or_else(|| self.path().join("instructions.md"))
    }
    fn assets(&self) -> PathBuf {
        self.assets
            .as_ref()
            .cloned()
            .unwrap_or_else(|| self.path().join("assets"))
    }
}

#[derive(Deserialize, Serialize, TS)]
#[ts(export)]
pub struct ImageSources(pub BTreeMap<ImageId, ImageSource>);

#[derive(Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub enum ImageSource {
    Docker(String), // image tag
}
impl ImageSource {
    pub async fn load(&self, into: &mut DirectoryContents<PackSource>) -> Result<(), Error> {
        todo!()
    }
}

pub async fn pack(ctx: CliContext, params: PackParams) -> Result<(), Error> {
    let mut files = DirectoryContents::<PackSource>::new();
    let js_dir = params.javascript();
    let manifest: Arc<[u8]> = Command::new("node")
        .arg("-e")
        .arg(format!(
            "console.log(JSON.stringify(require('{}/index.js').manifest))",
            js_dir.display()
        ))
        .invoke(ErrorKind::Javascript)
        .await?
        .into();
    let images: ImageSources = serde_json::from_slice(
        &Command::new("node")
            .arg("-e")
            .arg(format!(
                "console.log(JSON.stringify(require('{}/index.js').images))",
                js_dir.display()
            ))
            .invoke(ErrorKind::Javascript)
            .await?,
    )
    .with_kind(ErrorKind::Deserialization)?;
    files.insert(
        "manifest.json".into(),
        Entry::file(PackSource::Buffered(manifest.clone())),
    );
    let icon = params.icon().await?;
    let icon_ext = icon
        .extension()
        .or_not_found("icon file extension")?
        .to_string_lossy();
    files.insert(
        InternedString::from_display(&lazy_format!("icon.{}", icon_ext)),
        Entry::file(PackSource::File(icon)),
    );
    files.insert(
        "LICENSE.md".into(),
        Entry::file(PackSource::File(params.license())),
    );
    files.insert(
        "instructions.md".into(),
        Entry::file(PackSource::File(params.instructions())),
    );
    files.insert(
        "javascript.squashfs".into(),
        Entry::file(PackSource::Squashfs(Arc::new(js_dir.into()))),
    );
    let mut assets_dir = DirectoryContents::<PackSource>::new();
    let mut assets = tokio::fs::read_dir(params.assets()).await?;
    while let Some(assets) = assets.next_entry().await? {
        assets_dir.insert(
            InternedString::from_display(
                &AsRef::<Path>::as_ref(&assets.file_name())
                    .with_extension("squashfs")
                    .display(),
            ),
            Entry::file(PackSource::Squashfs(Arc::new(assets.path().into()))),
        );
    }

    let mut s9pk = S9pk::new(
        MerkleArchive::new(files, ctx.developer_key()?.clone(), SIG_CONTEXT),
        None,
    )
    .await?;

    s9pk.serialize(
        &mut File::create(params.output(&s9pk.as_manifest().id)).await?,
        false,
    )
    .await?;

    Ok(())
}
