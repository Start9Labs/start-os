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
    #[arg(short = 'H', long, help = "help.arg.host-url")]
    pub host: Option<Url>,
    #[arg(short = 'r', long, help = "help.arg.registry-url")]
    pub registry: Option<Url>,
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
