extern crate self as uciedit;

use chrono::DateTime;
use chrono::Utc;
use serde::Serializer;
use std::collections::BTreeMap;
use std::fmt;
use std::fs;
use std::io;
use std::io::Seek as _;
use std::ops;
use std::path::Path;
use std::path::PathBuf;
use std::str::Utf8Error;

pub mod openwrt;

mod syntax;
pub use syntax::*;

pub trait TypedSection<'a>: Sized {
    fn is_type(ty: &str) -> bool;
    fn read(section: &Section<'a>) -> Result<Self, Error>;
    fn write(&self, section: &mut Section<'a>) -> Result<(), Error>;
    fn append(&self, arena: &'a Arena, name: Option<&'a str>) -> Result<Section<'a>, Error>;
}
pub use uciedit_macros::TypedSection;

#[derive(Debug, serde::Serialize)]
pub enum Source {
    Unknown,
    UnknownLine(usize),
    Line(PathBuf, usize),
    Path(PathBuf),
}

impl From<usize> for Source {
    fn from(value: usize) -> Self {
        Source::UnknownLine(value)
    }
}

impl fmt::Display for Source {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Source::Unknown => write!(f, "???"),
            Source::UnknownLine(line) => write!(f, "???:{}", line),
            Source::Line(path, line) => write!(f, "{}:{}", path.display(), line),
            Source::Path(path) => write!(f, "{}", path.display()),
        }
    }
}

fn ser_std_err<S: Serializer>(err: &impl std::error::Error, ser: S) -> Result<S::Ok, S::Error> {
    serde::Serialize::serialize(&err.to_string(), ser)
}

#[derive(serde::Serialize, thiserror::Error, Debug)]
#[serde(tag = "what")]
pub enum Error {
    #[error("{cause} at {src}")]
    Io {
        #[serde(serialize_with = "ser_std_err")]
        cause: std::io::Error,
        src: Source,
    },
    #[error("{cause} at {src}")]
    Utf8 {
        #[serde(serialize_with = "ser_std_err")]
        cause: Utf8Error,
        src: Source,
    },
    #[error("config {src} was modified more recently")]
    Conflict { src: Source },
    #[error("{cause} at {src}")]
    FdLock {
        #[serde(serialize_with = "ser_std_err")]
        cause: fd_lock_rs::Error,
        src: Source,
    },
    #[error("bad section type at {src}: {desc}")]
    BadSection { src: Source, desc: String },
    #[error("bad option at {src}: {desc}")]
    BadOption { src: Source, desc: String },
    #[error("bad list at {src}: {desc}")]
    BadList { src: Source, desc: String },
    #[error("unknown uci keyword {found:?} at {src}")]
    UnknownKeyword { src: Source, found: String },
    #[error("expected uci section at {src}")]
    ExpectedSection { src: Source },
    #[error("expected {found:?} section at {src} to be {expected:?}")]
    ExpectedSectionType {
        src: Source,
        expected: String,
        found: String,
    },
    #[error("error parsing a value at {src}: {desc:?}")]
    ValueMsg { src: Source, desc: String },
    #[error("error parsing a value at {src}: {found:?} should be a boolean")]
    ValueBoolean { src: Source, found: String },
    #[error("missing option {missing:?} at {src}")]
    MissingOption { src: Source, missing: String },
}

impl Error {
    pub fn map_src(self, with: impl FnOnce(Source) -> Source) -> Self {
        use Error::*;
        match self {
            Io { cause, src } => Io {
                cause,
                src: with(src),
            },
            Utf8 { cause, src } => Utf8 {
                cause,
                src: with(src),
            },
            Conflict { src } => Conflict { src: with(src) },
            FdLock { cause, src } => FdLock {
                cause,
                src: with(src),
            },
            BadSection { src, desc } => BadSection {
                src: with(src),
                desc,
            },
            BadOption { src, desc } => BadOption {
                src: with(src),
                desc,
            },
            BadList { src, desc } => BadList {
                src: with(src),
                desc,
            },
            UnknownKeyword { src, found } => UnknownKeyword {
                src: with(src),
                found,
            },
            ExpectedSection { src } => ExpectedSection { src: with(src) },
            ExpectedSectionType {
                src,
                expected,
                found,
            } => ExpectedSectionType {
                src: with(src),
                expected,
                found,
            },
            ValueMsg { src, desc } => ValueMsg {
                src: with(src),
                desc,
            },
            ValueBoolean { src, found } => ValueBoolean {
                src: with(src),
                found,
            },
            MissingOption { src, missing } => MissingOption {
                src: with(src),
                missing,
            },
        }
    }
}

trait ResultExt {
    fn with_path(self, path: &Path) -> Self;
}

impl<T> ResultExt for Result<T, Error> {
    fn with_path(self, path: &Path) -> Self {
        use Source::*;
        match self {
            Ok(x) => Ok(x),
            Err(e) => Err(e.map_src(|src| match src {
                Unknown => Path(path.to_path_buf()),
                UnknownLine(n) => Line(path.to_path_buf(), n),
                _ => src,
            })),
        }
    }
}

impl Error {
    pub fn parse(
        error: impl std::error::Error + Sync + Send + 'static,
        line_number: usize,
    ) -> Self {
        Error::ValueMsg {
            src: Source::UnknownLine(line_number),
            desc: error.to_string(),
        }
    }
}

pub struct LockedConfigWriter {
    path: PathBuf,
    locked: fd_lock_rs::FdLock<fs::File>,
}

impl LockedConfigWriter {
    pub fn start(path: PathBuf) -> Result<Self, Error> {
        use fd_lock_rs::{FdLock, LockType};
        let file = std::fs::File::options()
            .create(true)
            .write(true)
            .truncate(false)
            .open(&path)
            .map_err(|cause| Error::Io {
                cause,
                src: Source::Path(path.clone()),
            })?;
        let locked =
            FdLock::lock(file, LockType::Exclusive, true).map_err(|cause| Error::FdLock {
                cause,
                src: Source::Path(path.clone()),
            })?;
        Ok(LockedConfigWriter { path, locked })
    }

    pub fn check_modified(&mut self, expected: DateTime<Utc>) -> Result<(), Error> {
        let found = self
            .locked
            .metadata()
            .and_then(|m| m.modified())
            .map(DateTime::<Utc>::from)
            .ok();
        if found.is_some_and(|found| found > expected) {
            Err(Error::Conflict {
                src: Source::Path(self.path.clone()),
            })
        } else {
            Ok(())
        }
    }

    pub fn finish(&mut self, config: &Config) -> Result<(), Error> {
        let locked = &mut *self.locked;
        locked.set_len(0).map_err(|cause| Error::Io {
            cause,
            src: Source::Path(self.path.clone()),
        })?;
        locked
            .seek(std::io::SeekFrom::Start(0))
            .map_err(|cause| Error::Io {
                cause,
                src: Source::Path(self.path.clone()),
            })?;
        config
            .dump_io(io::BufWriter::new(locked))
            .with_path(&self.path)
    }
}

#[derive(Clone, Default)]
pub struct Configs<'a> {
    map: BTreeMap<String, Config<'a>>,
}

impl<'a> ops::Index<&str> for Configs<'a> {
    type Output = Config<'a>;

    fn index(&self, index: &str) -> &Self::Output {
        match self.map.get(index) {
            Some(cfg) => cfg,
            None => panic!("did not parse config {index}"),
        }
    }
}

impl<'a> ops::IndexMut<&str> for Configs<'a> {
    fn index_mut(&mut self, index: &str) -> &mut Self::Output {
        match self.map.get_mut(index) {
            Some(cfg) => cfg,
            None => panic!("did not parse config {index}"),
        }
    }
}

pub fn parse_all<'a>(
    root: impl AsRef<Path>,
    arena: &'a Arena,
    names: &[&str],
) -> Result<Configs<'a>, Error> {
    const MAX_RETRIES: usize = 4;

    let mut configs = Configs {
        map: BTreeMap::new(),
    };
    'retry: for _ in 0..MAX_RETRIES {
        configs.map.clear();
        let expected: DateTime<Utc> = std::time::SystemTime::now().into();
        for &name in names {
            configs
                .map
                .insert(name.into(), Config::parse(arena, root.as_ref().join(name))?);
        }
        for config in configs.map.values() {
            if let Some(found) = config.modified {
                if found > expected {
                    // file was updated after we started reading, retry!
                    continue 'retry;
                }
            }
        }
        break 'retry; // looks valid!
    }
    Ok(configs)
}

pub fn dump_all(root: impl AsRef<Path>, configs: Configs) -> Result<(), Error> {
    // Lock all the files at once.
    // We do it in lexicographic order so that deadlocks are impossible.
    let mut files = configs
        .map
        .into_iter()
        .map(|(name, section)| {
            let path = root.as_ref().join(&name);
            Ok::<_, Error>((name, section, LockedConfigWriter::start(path)?))
        })
        .collect::<Result<Vec<_>, _>>()?;

    // Check all the modified times.
    for (_, tosave, saved) in &mut files {
        if let Some(expected) = tosave.modified {
            saved.check_modified(expected)?;
        }
    }

    // Actually write the sections.
    let mut result = Ok(());
    for (_, tosave, mut saved) in files {
        result = result.and(saved.finish(&tosave));
    }

    result
}

#[cfg(test)]
mod tests;
