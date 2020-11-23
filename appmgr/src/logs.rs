use std::borrow::Cow;
use std::ffi::{OsStr, OsString};
use std::path::Path;

use failure::ResultExt as _;
use futures::stream::StreamExt;
use futures::stream::TryStreamExt;
use itertools::Itertools;

use crate::util::PersistencePath;
use crate::Error;
use crate::ResultExt as _;

#[derive(Clone, Copy, Debug, serde::Serialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum Level {
    Error,
    Warn,
    Success,
    Info,
}
impl std::fmt::Display for Level {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Level::Error => write!(f, "ERROR"),
            Level::Warn => write!(f, "WARN"),
            Level::Success => write!(f, "SUCCESS"),
            Level::Info => write!(f, "INFO"),
        }
    }
}
impl std::str::FromStr for Level {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "ERROR" => Ok(Level::Error),
            "WARN" => Ok(Level::Warn),
            "SUCCESS" => Ok(Level::Success),
            "INFO" => Ok(Level::Info),
            _ => Err(Error::from(format_err!("Unknown Notification Level"))),
        }
    }
}

#[derive(Clone, Debug, serde::Serialize)]
pub struct Notification {
    pub time: i64,
    pub level: Level,
    pub code: usize,
    pub title: String,
    pub message: String,
}
impl std::fmt::Display for Notification {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{}:{}",
            self.level,
            self.code,
            self.title.replace(":", "\u{A789}"),
            self.message.replace("\n", "\u{2026}")
        )
    }
}
impl std::str::FromStr for Notification {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut split = s.split(":");
        Ok(Notification {
            time: split
                .next()
                .ok_or_else(|| format_err!("missing time"))?
                .parse::<f64>()
                .map(|a| a as i64)
                .no_code()?,
            level: split
                .next()
                .ok_or_else(|| format_err!("missing level"))?
                .parse()?,
            code: split
                .next()
                .ok_or_else(|| format_err!("missing code"))?
                .parse()
                .no_code()?,
            title: split
                .next()
                .ok_or_else(|| format_err!("missing title"))?
                .replace("\u{A789}", ":"),
            message: split
                .intersperse(":")
                .collect::<String>()
                .replace("\u{2026}", "\n"),
        })
    }
}

pub struct LogOptions<A: AsRef<str>, B: AsRef<str>> {
    pub details: bool,
    pub follow: bool,
    pub since: Option<A>,
    pub until: Option<B>,
    pub tail: Option<usize>,
    pub timestamps: bool,
}

pub async fn logs<A: AsRef<str>, B: AsRef<str>>(
    name: &str,
    options: LogOptions<A, B>,
) -> Result<(), Error> {
    let mut args = vec![Cow::Borrowed(OsStr::new("logs"))];
    if options.details {
        args.push(Cow::Borrowed(OsStr::new("--details")));
    }
    if options.follow {
        args.push(Cow::Borrowed(OsStr::new("-f")));
    }
    if let Some(since) = options.since.as_ref() {
        args.push(Cow::Borrowed(OsStr::new("--since")));
        args.push(Cow::Borrowed(OsStr::new(since.as_ref())));
    }
    if let Some(until) = options.until.as_ref() {
        args.push(Cow::Borrowed(OsStr::new("--until")));
        args.push(Cow::Borrowed(OsStr::new(until.as_ref())));
    }
    if let Some(tail) = options.tail {
        args.push(Cow::Borrowed(OsStr::new("--tail")));
        args.push(Cow::Owned(OsString::from(format!("{}", tail))));
    }
    if options.timestamps {
        args.push(Cow::Borrowed(OsStr::new("-t")));
    }
    args.push(Cow::Borrowed(OsStr::new(name)));
    crate::ensure_code!(
        std::process::Command::new("docker")
            .args(args.into_iter())
            .status()?
            .success(),
        crate::error::DOCKER_ERROR,
        "Failed to Collect Logs from Docker"
    );
    Ok(())
}

pub async fn notifications(id: &str) -> Result<Vec<Notification>, Error> {
    let p = PersistencePath::from_ref("notifications").join(id).tmp();
    if let Some(parent) = p.parent() {
        if !parent.exists() {
            tokio::fs::create_dir_all(parent).await?;
        }
    }
    match tokio::fs::rename(
        Path::new(crate::VOLUMES)
            .join(id)
            .join("start9")
            .join("notifications.log"),
        &p,
    )
    .await
    {
        Err(ref e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(Vec::new()),
        a => a,
    }?;
    let f = tokio::fs::File::open(&p)
        .await
        .with_context(|e| format!("{}: {}", p.display(), e))
        .with_code(crate::error::FILESYSTEM_ERROR)?;
    tokio::io::AsyncBufReadExt::lines(tokio::io::BufReader::new(f))
        .map(|a| a.map_err(From::from).and_then(|a| a.parse()))
        .try_collect()
        .await
}

pub async fn stats(id: &str) -> Result<serde_yaml::Value, Error> {
    let p = PersistencePath::from_ref("stats").join(id).tmp();
    if let Some(parent) = p.parent() {
        if !parent.exists() {
            tokio::fs::create_dir_all(parent).await?;
        }
    }
    match tokio::fs::copy(
        Path::new(crate::VOLUMES)
            .join(id)
            .join("start9")
            .join("stats.yaml"),
        &p,
    )
    .await
    {
        Err(ref e) if e.kind() == std::io::ErrorKind::NotFound => {
            return Ok(serde_yaml::Value::Null)
        }
        a => a,
    }?;
    let f = tokio::fs::File::open(&p)
        .await
        .with_context(|e| format!("{}: {}", p.display(), e))
        .with_code(crate::error::FILESYSTEM_ERROR)?;
    crate::util::from_yaml_async_reader(f).await.no_code()
}
