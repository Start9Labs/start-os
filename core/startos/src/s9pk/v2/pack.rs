use std::collections::BTreeSet;
use std::ffi::OsStr;
use std::io::Cursor;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use clap::Parser;
use futures::future::{ready, BoxFuture};
use futures::{FutureExt, TryStreamExt};
use imbl_value::InternedString;
use models::{ImageId, PackageId, VersionString};
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
use crate::s9pk::merkle_archive::source::{
    into_dyn_read, ArchiveSource, DynFileSource, FileSource,
};
use crate::s9pk::merkle_archive::{Entry, MerkleArchive};
use crate::s9pk::v2::SIG_CONTEXT;
use crate::s9pk::S9pk;
use crate::util::io::TmpDir;
use crate::util::Invoke;

#[cfg(not(feature = "docker"))]
pub const CONTAINER_TOOL: &str = "podman";

#[cfg(feature = "docker")]
pub const CONTAINER_TOOL: &str = "docker";

pub struct SqfsDir {
    path: PathBuf,
    tmpdir: Arc<TmpDir>,
    sqfs: OnceCell<MultiCursorFile>,
}
impl SqfsDir {
    pub fn new(path: PathBuf, tmpdir: Arc<TmpDir>) -> Self {
        Self {
            path,
            tmpdir,
            sqfs: OnceCell::new(),
        }
    }
    async fn file(&self) -> Result<&MultiCursorFile, Error> {
        self.sqfs
            .get_or_try_init(|| async move {
                let guid = Guid::new();
                let path = self.tmpdir.join(guid.as_ref()).with_extension("squashfs");
                let mut cmd = Command::new("mksquashfs");
                if self.path.extension().and_then(|s| s.to_str()) == Some("tar") {
                    cmd.arg("-tar");
                }
                cmd.arg(&self.path)
                    .arg(&path)
                    .invoke(ErrorKind::Filesystem)
                    .await?;
                Ok(MultiCursorFile::from(
                    File::open(&path)
                        .await
                        .with_ctx(|_| (ErrorKind::Filesystem, path.display()))?,
                ))
            })
            .await
    }
}

#[derive(Clone)]
pub enum PackSource {
    Buffered(Arc<[u8]>),
    File(PathBuf),
    Squashfs(Arc<SqfsDir>),
}
impl FileSource for PackSource {
    type Reader = Box<dyn AsyncRead + Unpin + Send + Sync + 'static>;
    async fn size(&self) -> Result<u64, Error> {
        match self {
            Self::Buffered(a) => Ok(a.len() as u64),
            Self::File(f) => Ok(tokio::fs::metadata(f)
                .await
                .with_ctx(|_| (ErrorKind::Filesystem, f.display()))?
                .len()),
            Self::Squashfs(dir) => dir
                .file()
                .await
                .with_ctx(|_| (ErrorKind::Filesystem, dir.path.display()))?
                .size()
                .await
                .or_not_found("file metadata"),
        }
    }
    async fn reader(&self) -> Result<Self::Reader, Error> {
        match self {
            Self::Buffered(a) => Ok(into_dyn_read(Cursor::new(a.clone()))),
            Self::File(f) => Ok(into_dyn_read(
                File::open(f)
                    .await
                    .with_ctx(|_| (ErrorKind::Filesystem, f.display()))?,
            )),
            Self::Squashfs(dir) => dir.file().await?.fetch_all().await.map(into_dyn_read),
        }
    }
}
impl From<PackSource> for DynFileSource {
    fn from(value: PackSource) -> Self {
        DynFileSource::new(value)
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
            ReadDirStream::new(tokio::fs::read_dir(self.path()).await?).try_filter(|x| ready(x.path().file_stem() == Some(OsStr::new("icon")))).map_err(Error::from).try_fold(Err(Error::new(eyre!("icon not found"), ErrorKind::NotFound)), |acc, x| async move { match acc {
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

#[derive(Debug, Clone, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct ImageConfig {
    pub source: ImageSource,
    #[ts(type = "string[]")]
    pub arch: BTreeSet<InternedString>,
    #[ts(type = "string | null")]
    pub emulate_missing_as: Option<InternedString>,
}
impl Default for ImageConfig {
    fn default() -> Self {
        Self {
            source: ImageSource::Packed,
            arch: BTreeSet::new(),
            emulate_missing_as: None,
        }
    }
}

#[derive(Parser)]
struct CliImageConfig {
    #[arg(long, conflicts_with("docker-tag"))]
    docker_build: bool,
    #[arg(long, requires("docker-build"))]
    dockerfile: Option<PathBuf>,
    #[arg(long, requires("docker-build"))]
    workdir: Option<PathBuf>,
    #[arg(long, conflicts_with_all(["dockerfile", "workdir"]))]
    docker_tag: Option<String>,
    #[arg(long)]
    arch: Vec<InternedString>,
    #[arg(long)]
    emulate_missing_as: Option<InternedString>,
}
impl TryFrom<CliImageConfig> for ImageConfig {
    type Error = clap::Error;
    fn try_from(value: CliImageConfig) -> Result<Self, Self::Error> {
        let res = Self {
            source: if value.docker_build {
                ImageSource::DockerBuild {
                    dockerfile: value.dockerfile,
                    workdir: value.workdir,
                }
            } else if let Some(tag) = value.docker_tag {
                ImageSource::DockerTag(tag)
            } else {
                ImageSource::Packed
            },
            arch: value.arch.into_iter().collect(),
            emulate_missing_as: value.emulate_missing_as,
        };
        res.emulate_missing_as
            .as_ref()
            .map(|a| {
                if !res.arch.contains(a) {
                    Err(clap::Error::raw(
                        clap::error::ErrorKind::InvalidValue,
                        "`emulate-missing-as` must match one of the provided `arch`es",
                    ))
                } else {
                    Ok(())
                }
            })
            .transpose()?;
        Ok(res)
    }
}
impl clap::Args for ImageConfig {
    fn augment_args(cmd: clap::Command) -> clap::Command {
        CliImageConfig::augment_args(cmd)
    }
    fn augment_args_for_update(cmd: clap::Command) -> clap::Command {
        CliImageConfig::augment_args_for_update(cmd)
    }
}
impl clap::FromArgMatches for ImageConfig {
    fn from_arg_matches(matches: &clap::ArgMatches) -> Result<Self, clap::Error> {
        Self::try_from(CliImageConfig::from_arg_matches(matches)?)
    }
    fn update_from_arg_matches(&mut self, matches: &clap::ArgMatches) -> Result<(), clap::Error> {
        *self = Self::try_from(CliImageConfig::from_arg_matches(matches)?)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub enum ImageSource {
    Packed,
    #[serde(rename_all = "camelCase")]
    DockerBuild {
        workdir: Option<PathBuf>,
        dockerfile: Option<PathBuf>,
    },
    DockerTag(String),
}
impl ImageSource {
    #[instrument(skip_all)]
    pub fn load<'a, S: From<PackSource> + FileSource + Clone>(
        &'a self,
        tmpdir: &'a TmpDir,
        id: &'a PackageId,
        version: &'a VersionString,
        image_id: &'a ImageId,
        arch: &'a str,
        into: &'a mut DirectoryContents<S>,
    ) -> BoxFuture<'a, Result<(), Error>> {
        #[derive(Deserialize)]
        #[serde(rename_all = "PascalCase")]
        struct DockerImageConfig {
            env: Vec<String>,
            #[serde(default)]
            working_dir: PathBuf,
            #[serde(default)]
            user: String,
        }
        async move {
            match self {
                ImageSource::Packed => Ok(()),
                ImageSource::DockerBuild {
                    workdir,
                    dockerfile,
                } => {
                    let workdir = workdir.as_deref().unwrap_or(Path::new("."));
                    let dockerfile = dockerfile
                        .clone()
                        .unwrap_or_else(|| workdir.join("Dockerfile"));
                    let docker_platform = if arch == "x86_64" {
                        "--platform=linux/amd64".to_owned()
                    } else if arch == "aarch64" {
                        "--platform=linux/arm64".to_owned()
                    } else {
                        format!("--platform=linux/{arch}")
                    };
                    // docker buildx build ${path} -o type=image,name=start9/${id}
                    let tag = format!("start9/{id}/{image_id}:{version}");
                    Command::new(CONTAINER_TOOL)
                        .arg("build")
                        .arg(workdir)
                        .arg("-f")
                        .arg(dockerfile)
                        .arg("-t")
                        .arg(&tag)
                        .arg(&docker_platform)
                        .arg("-o")
                        .arg("type=image")
                        .capture(false)
                        .invoke(ErrorKind::Docker)
                        .await?;
                    ImageSource::DockerTag(tag.clone())
                        .load(tmpdir, id, version, image_id, arch, into)
                        .await?;
                    Command::new(CONTAINER_TOOL)
                        .arg("rmi")
                        .arg(&tag)
                        .invoke(ErrorKind::Docker)
                        .await?;
                    Ok(())
                }
                ImageSource::DockerTag(tag) => {
                    let docker_platform = if arch == "x86_64" {
                        "--platform=linux/amd64".to_owned()
                    } else if arch == "aarch64" {
                        "--platform=linux/arm64".to_owned()
                    } else {
                        format!("--platform=linux/{arch}")
                    };
                    let mut inspect_cmd = Command::new(CONTAINER_TOOL);
                    inspect_cmd
                        .arg("image")
                        .arg("inspect")
                        .arg("--format")
                        .arg("{{json .Config}}")
                        .arg(&tag);
                    let inspect_res = match inspect_cmd.invoke(ErrorKind::Docker).await {
                        Ok(a) => a,
                        Err(e)
                            if {
                                let msg = e.source.to_string();
                                #[cfg(feature = "docker")]
                                let matches = msg.contains("No such image:");
                                #[cfg(not(feature = "docker"))]
                                let matches = msg.contains(": image not known");
                                matches
                            } =>
                        {
                            Command::new(CONTAINER_TOOL)
                                .arg("pull")
                                .arg(&docker_platform)
                                .arg(tag)
                                .capture(false)
                                .invoke(ErrorKind::Docker)
                                .await?;
                            inspect_cmd.invoke(ErrorKind::Docker).await?
                        }
                        Err(e) => return Err(e),
                    };
                    let config = serde_json::from_slice::<DockerImageConfig>(&inspect_res)
                        .with_kind(ErrorKind::Deserialization)?;
                    let base_path = Path::new("images").join(arch).join(image_id);
                    into.insert_path(
                        base_path.with_extension("json"),
                        Entry::file(
                            PackSource::Buffered(
                                serde_json::to_vec(&ImageMetadata {
                                    workdir: if config.working_dir == Path::new("") {
                                        "/".into()
                                    } else {
                                        config.working_dir
                                    },
                                    user: if config.user.is_empty() {
                                        "root".into()
                                    } else {
                                        config.user.into()
                                    },
                                })
                                .with_kind(ErrorKind::Serialization)?
                                .into(),
                            )
                            .into(),
                        ),
                    )?;
                    into.insert_path(
                        base_path.with_extension("env"),
                        Entry::file(
                            PackSource::Buffered(config.env.join("\n").into_bytes().into()).into(),
                        ),
                    )?;
                    let dest = tmpdir.join(Guid::new().as_ref()).with_extension("squashfs");
                    let container = String::from_utf8(
                        Command::new(CONTAINER_TOOL)
                            .arg("create")
                            .arg(&docker_platform)
                            .arg(&tag)
                            .invoke(ErrorKind::Docker)
                            .await?,
                    )?;
                    Command::new(CONTAINER_TOOL)
                        .arg("export")
                        .arg(container.trim())
                        .pipe(Command::new("mksquashfs").arg("-").arg(&dest).arg("-tar"))
                        .capture(false)
                        .invoke(ErrorKind::Docker)
                        .await?;
                    Command::new(CONTAINER_TOOL)
                        .arg("rm")
                        .arg(container.trim())
                        .invoke(ErrorKind::Docker)
                        .await?;
                    into.insert_path(
                        base_path.with_extension("squashfs"),
                        Entry::file(PackSource::File(dest).into()),
                    )?;

                    Ok(())
                }
            }
        }
        .boxed()
    }
}

#[derive(Debug, Clone, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct ImageMetadata {
    pub workdir: PathBuf,
    #[ts(type = "string")]
    pub user: InternedString,
}

#[instrument(skip_all)]
pub async fn pack(ctx: CliContext, params: PackParams) -> Result<(), Error> {
    let tmpdir = Arc::new(TmpDir::new().await?);
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
        Entry::file(PackSource::Squashfs(Arc::new(SqfsDir::new(
            js_dir,
            tmpdir.clone(),
        )))),
    );

    let mut s9pk = S9pk::new(
        MerkleArchive::new(files, ctx.developer_key()?.clone(), SIG_CONTEXT),
        None,
    )
    .await?;

    let assets_dir = params.assets();
    for assets in s9pk.as_manifest().assets.clone() {
        s9pk.as_archive_mut().contents_mut().insert_path(
            Path::new("assets").join(&assets).with_extension("squashfs"),
            Entry::file(PackSource::Squashfs(Arc::new(SqfsDir::new(
                assets_dir.join(&assets),
                tmpdir.clone(),
            )))),
        )?;
    }

    s9pk.load_images(&*tmpdir).await?;

    s9pk.validate_and_filter(None)?;

    s9pk.serialize(
        &mut File::create(params.output(&s9pk.as_manifest().id)).await?,
        false,
    )
    .await?;

    drop(s9pk);

    tmpdir.gc().await?;

    Ok(())
}
