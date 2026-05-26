use std::collections::BTreeMap;
use std::fs::File;
use std::net::SocketAddr;
use std::path::{Path, PathBuf};

use clap::Parser;
use imbl_value::InternedString;
use reqwest::Url;
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};

use crate::MAIN_DATA;
use crate::prelude::*;
use crate::util::serde::IoFormat;
use crate::version::VersionT;

pub const DEVICE_CONFIG_PATH: &str = "/media/startos/config/config.yaml"; // "/media/startos/config/config.yaml";
pub const CONFIG_PATH: &str = "/etc/startos/config.yaml";
pub const CONFIG_PATH_LOCAL: &str = ".startos/config.yaml";

pub fn local_config_path() -> Option<PathBuf> {
    if let Ok(home) = std::env::var("HOME") {
        Some(Path::new(&home).join(CONFIG_PATH_LOCAL))
    } else {
        None
    }
}

pub trait ContextConfig: DeserializeOwned + Default {
    fn next(&mut self) -> Option<PathBuf>;
    fn merge_with(&mut self, other: Self);
    fn from_path(path: impl AsRef<Path>) -> Result<Self, Error> {
        let format: IoFormat = path
            .as_ref()
            .extension()
            .and_then(|s| s.to_str())
            .map(|f| f.parse())
            .transpose()?
            .unwrap_or_default();
        format.from_reader(
            File::open(path.as_ref())
                .with_ctx(|_| (ErrorKind::Filesystem, path.as_ref().display()))?,
        )
    }
    fn load_path_rec(&mut self, path: Option<impl AsRef<Path>>) -> Result<(), Error> {
        if let Some(path) = path.filter(|p| p.as_ref().exists()) {
            let mut other = Self::from_path(path)?;
            let path = other.next();
            self.merge_with(other);
            self.load_path_rec(path)?;
        }
        Ok(())
    }
}

#[derive(Debug, Default, Deserialize, Serialize, Parser)]
#[group(skip)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
#[command(version = crate::version::Current::default().semver().to_string())]
pub struct ClientConfig {
    #[arg(short = 'c', long, help = "help.arg.config-file-path")]
    pub config: Option<PathBuf>,
    /// A `host` profile name from the workspace `.startos/config.yaml`, or a URL.
    #[arg(short = 'H', long, help = "help.arg.host-url")]
    pub host: Option<String>,
    /// A `registry` profile name from the workspace `.startos/config.yaml`, or a URL.
    #[arg(short = 'r', long, help = "help.arg.registry-url")]
    pub registry: Option<String>,
    #[arg(long, help = "help.arg.registry-hostname")]
    pub registry_hostname: Option<Vec<InternedString>>,
    #[arg(skip)]
    pub registry_listen: Option<SocketAddr>,
    #[arg(long, help = "help.s9pk-s3base")]
    pub s9pk_s3base: Option<Url>,
    #[arg(long, help = "help.s9pk-s3bucket")]
    pub s9pk_s3bucket: Option<InternedString>,
    #[arg(short = 't', long, help = "help.arg.tunnel-address")]
    pub tunnel: Option<SocketAddr>,
    #[arg(skip)]
    pub tunnel_listen: Option<SocketAddr>,
    #[arg(short = 'p', long, help = "help.arg.proxy-url")]
    pub proxy: Option<Url>,
    #[arg(skip)]
    pub socks_listen: Option<SocketAddr>,
    #[arg(long, help = "help.arg.cookie-path")]
    pub cookie_path: Option<PathBuf>,
    #[arg(long, help = "help.arg.developer-key-path")]
    pub developer_key_path: Option<PathBuf>,
    /// PEM-encoded root CA certificate(s) to trust when talking to a
    /// StartOS server with a self-signed cert (e.g. immediately after
    /// `setup complete`, before the device's CA has been imported into
    /// the local trust store). Repeatable.
    #[arg(long = "root-ca", value_name = "PEM_PATH")]
    #[serde(default)]
    pub root_ca: Option<Vec<PathBuf>>,
    /// Skip TLS certificate verification entirely. Intended for
    /// unattended bring-up against a fresh StartOS server whose
    /// self-signed CA hasn't been pinned yet. **Do not use over the
    /// public internet.**
    #[arg(long)]
    #[serde(default)]
    pub insecure: bool,
}
impl ContextConfig for ClientConfig {
    fn next(&mut self) -> Option<PathBuf> {
        self.config.take()
    }
    fn merge_with(&mut self, other: Self) {
        self.host = self.host.take().or(other.host);
        self.registry = self.registry.take().or(other.registry);
        self.registry_hostname = self.registry_hostname.take().or(other.registry_hostname);
        self.registry_listen = self.registry_listen.take().or(other.registry_listen);
        self.s9pk_s3base = self.s9pk_s3base.take().or(other.s9pk_s3base);
        self.s9pk_s3bucket = self.s9pk_s3bucket.take().or(other.s9pk_s3bucket);
        self.tunnel = self.tunnel.take().or(other.tunnel);
        self.tunnel_listen = self.tunnel_listen.take().or(other.tunnel_listen);
        self.proxy = self.proxy.take().or(other.proxy);
        self.socks_listen = self.socks_listen.take().or(other.socks_listen);
        self.cookie_path = self.cookie_path.take().or(other.cookie_path);
        self.developer_key_path = self.developer_key_path.take().or(other.developer_key_path);
        self.root_ca = self.root_ca.take().or(other.root_ca);
        self.insecure = self.insecure || other.insecure;
    }
}
impl ClientConfig {
    pub fn load(mut self) -> Result<Self, Error> {
        let path = self.next();
        self.load_path_rec(path)?;
        self.load_path_rec(local_config_path())?;
        self.load_path_rec(Some(CONFIG_PATH))?;
        Ok(self)
    }
}

/// Per-workspace config (`.startos/config.yaml`), discovered by walking up from
/// cwd. Unlike the flat global config it carries named `host`/`registry` profiles.
/// The required `schema` field is what distinguishes it from the legacy flat
/// `~/.startos/config.yaml` (which has no `schema`, so it's skipped here and still
/// loaded the old way).
#[derive(Debug, Default, Deserialize)]
pub struct WorkspaceConfig {
    #[allow(dead_code)]
    pub schema: u64,
    #[serde(default)]
    pub host: BTreeMap<String, Url>,
    #[serde(default)]
    pub registry: BTreeMap<String, Url>,
}
impl WorkspaceConfig {
    /// Walk up from cwd for the nearest `.startos/config.yaml` that parses as a
    /// workspace config. A flat config (no `schema`) fails to parse and is skipped,
    /// so the walk continues — preserving the global fallback when there's none.
    /// An EACCES (or other non-NotFound) error on a candidate is surfaced rather
    /// than swallowed, so an unreadable workspace can't masquerade as absent.
    pub fn find() -> Result<Option<Self>, Error> {
        let mut dir = std::env::current_dir()?;
        loop {
            let candidate = dir.join(".startos").join("config.yaml");
            match File::open(&candidate) {
                Ok(file) => {
                    if let Ok(config) = IoFormat::Yaml.from_reader::<_, Self>(file) {
                        return Ok(Some(config));
                    }
                    // Present but not a workspace config (e.g. the legacy flat
                    // config) — keep walking up.
                }
                Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
                Err(e) => return Err(e.into()),
            }
            if !dir.pop() {
                return Ok(None);
            }
        }
    }
}

/// Resolve a `-H`/`-r` value against a profile map: a profile-name match first,
/// then a literal URL, else an error. A missing value falls back to the `default`
/// profile, or `None` when there's no workspace config (keeping the global fallback).
pub fn resolve_target(
    value: Option<&str>,
    profiles: Option<&BTreeMap<String, Url>>,
) -> Result<Option<Url>, Error> {
    match value {
        Some(value) => {
            if let Some(url) = profiles.and_then(|p| p.get(value)) {
                Ok(Some(url.clone()))
            } else if let Ok(url) = Url::parse(value) {
                Ok(Some(url))
            } else {
                Err(Error::new(
                    eyre!(
                        "{}",
                        t!("context.cli.unknown-profile-or-url", value = value)
                    ),
                    ErrorKind::InvalidRequest,
                ))
            }
        }
        None => Ok(profiles.and_then(|p| p.get("default")).cloned()),
    }
}

#[derive(Debug, Clone, Default, Deserialize, Serialize, Parser)]
#[group(skip)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct ServerConfig {
    #[arg(short, long, help = "help.arg.config-file-path")]
    pub config: Option<PathBuf>,
    #[arg(long, help = "help.arg.socks-listen-address")]
    pub socks_listen: Option<SocketAddr>,
    #[arg(long, help = "help.arg.revision-cache-size")]
    pub revision_cache_size: Option<usize>,
    #[arg(long, help = "help.arg.disable-encryption")]
    pub disable_encryption: Option<bool>,
    #[arg(long, help = "help.arg.multi-arch-s9pks")]
    pub multi_arch_s9pks: Option<bool>,
    #[arg(long, help = "help.arg.developer-key-path")]
    pub developer_key_path: Option<PathBuf>,
    #[arg(long, help = "help.arg.max-proxy-conns-per-target")]
    pub max_proxy_conns_per_target: Option<usize>,
}
impl ContextConfig for ServerConfig {
    fn next(&mut self) -> Option<PathBuf> {
        self.config.take()
    }
    fn merge_with(&mut self, other: Self) {
        self.socks_listen = self.socks_listen.take().or(other.socks_listen);
        self.revision_cache_size = self
            .revision_cache_size
            .take()
            .or(other.revision_cache_size);
        self.disable_encryption = self.disable_encryption.take().or(other.disable_encryption);
        self.multi_arch_s9pks = self.multi_arch_s9pks.take().or(other.multi_arch_s9pks);
        self.developer_key_path = self.developer_key_path.take().or(other.developer_key_path);
        self.max_proxy_conns_per_target = self
            .max_proxy_conns_per_target
            .take()
            .or(other.max_proxy_conns_per_target);
    }
}

impl ServerConfig {
    pub fn load(mut self) -> Result<Self, Error> {
        let path = self.next();
        self.load_path_rec(path)?;
        self.load_path_rec(Some(DEVICE_CONFIG_PATH))?;
        self.load_path_rec(Some(CONFIG_PATH))?;
        Ok(self)
    }
    pub async fn db(&self) -> Result<PatchDb, Error> {
        let db_path = Path::new(MAIN_DATA).join("embassy.db");
        let db = PatchDb::open(&db_path)
            .await
            .with_ctx(|_| (crate::ErrorKind::Filesystem, db_path.display().to_string()))?;

        Ok(db)
    }
}
