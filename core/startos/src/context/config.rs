use std::fs::File;
use std::net::SocketAddr;
use std::path::{Path, PathBuf};

use clap::Parser;
use imbl_value::InternedString;
use reqwest::Url;
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};

use crate::MAIN_DATA;
use crate::disk::OsPartitionInfo;
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
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
#[command(version = crate::version::Current::default().semver().to_string())]
pub struct ClientConfig {
    #[arg(short = 'c', long)]
    pub config: Option<PathBuf>,
    #[arg(short = 'H', long)]
    pub host: Option<Url>,
    #[arg(short = 'r', long)]
    pub registry: Option<Url>,
    #[arg(long)]
    pub registry_hostname: Option<InternedString>,
    #[arg(skip)]
    pub registry_listen: Option<SocketAddr>,
    #[arg(short = 't', long)]
    pub tunnel: Option<SocketAddr>,
    #[arg(skip)]
    pub tunnel_listen: Option<SocketAddr>,
    #[arg(short = 'p', long)]
    pub proxy: Option<Url>,
    #[arg(skip)]
    pub socks_listen: Option<SocketAddr>,
    #[arg(long)]
    pub cookie_path: Option<PathBuf>,
    #[arg(long)]
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
        self.tunnel = self.tunnel.take().or(other.tunnel);
        self.proxy = self.proxy.take().or(other.proxy);
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
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct ServerConfig {
    #[arg(short, long)]
    pub config: Option<PathBuf>,
    #[arg(long)]
    pub ethernet_interface: Option<String>,
    #[arg(skip)]
    pub os_partitions: Option<OsPartitionInfo>,
    #[arg(long)]
    pub socks_listen: Option<SocketAddr>,
    #[arg(long)]
    pub revision_cache_size: Option<usize>,
    #[arg(long)]
    pub disable_encryption: Option<bool>,
    #[arg(long)]
    pub multi_arch_s9pks: Option<bool>,
    #[arg(long)]
    pub developer_key_path: Option<PathBuf>,
}
impl ContextConfig for ServerConfig {
    fn next(&mut self) -> Option<PathBuf> {
        self.config.take()
    }
    fn merge_with(&mut self, other: Self) {
        self.ethernet_interface = self.ethernet_interface.take().or(other.ethernet_interface);
        self.os_partitions = self.os_partitions.take().or(other.os_partitions);
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
