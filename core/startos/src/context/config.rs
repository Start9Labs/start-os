use std::fs::File;
use std::net::SocketAddr;
use std::path::{Path, PathBuf};

use clap::Parser;
use reqwest::Url;
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use sqlx::postgres::PgConnectOptions;
use sqlx::PgPool;

use crate::disk::OsPartitionInfo;
use crate::init::init_postgres;
use crate::prelude::*;
use crate::util::serde::IoFormat;

pub const DEVICE_CONFIG_PATH: &str = "/media/embassy/config/config.yaml"; // "/media/startos/config/config.yaml";
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
        format.from_reader(File::open(path)?)
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
pub struct ClientConfig {
    #[arg(short = 'c', long = "config")]
    pub config: Option<PathBuf>,
    #[arg(short = 'h', long = "host")]
    pub host: Option<Url>,
    #[arg(short = 'p', long = "proxy")]
    pub proxy: Option<Url>,
    #[arg(long = "cookie-path")]
    pub cookie_path: Option<PathBuf>,
    #[arg(long = "developer-key-path")]
    pub developer_key_path: Option<PathBuf>,
}
impl ContextConfig for ClientConfig {
    fn next(&mut self) -> Option<PathBuf> {
        self.config.take()
    }
    fn merge_with(&mut self, other: Self) {
        self.host = self.host.take().or(other.host);
        self.proxy = self.proxy.take().or(other.proxy);
        self.cookie_path = self.cookie_path.take().or(other.cookie_path);
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
    #[arg(short = 'c', long = "config")]
    pub config: Option<PathBuf>,
    #[arg(long = "wifi-interface")]
    pub wifi_interface: Option<String>,
    #[arg(long = "ethernet-interface")]
    pub ethernet_interface: Option<String>,
    #[arg(skip)]
    pub os_partitions: Option<OsPartitionInfo>,
    #[arg(long = "bind-rpc")]
    pub bind_rpc: Option<SocketAddr>,
    #[arg(long = "tor-control")]
    pub tor_control: Option<SocketAddr>,
    #[arg(long = "tor-socks")]
    pub tor_socks: Option<SocketAddr>,
    #[arg(long = "dns-bind")]
    pub dns_bind: Option<Vec<SocketAddr>>,
    #[arg(long = "revision-cache-size")]
    pub revision_cache_size: Option<usize>,
    #[arg(short = 'd', long = "datadir")]
    pub datadir: Option<PathBuf>,
    #[arg(long = "disable-encryption")]
    pub disable_encryption: Option<bool>,
}
impl ContextConfig for ServerConfig {
    fn next(&mut self) -> Option<PathBuf> {
        self.config.take()
    }
    fn merge_with(&mut self, other: Self) {
        self.wifi_interface = self.wifi_interface.take().or(other.wifi_interface);
        self.ethernet_interface = self.ethernet_interface.take().or(other.ethernet_interface);
        self.os_partitions = self.os_partitions.take().or(other.os_partitions);
        self.bind_rpc = self.bind_rpc.take().or(other.bind_rpc);
        self.tor_control = self.tor_control.take().or(other.tor_control);
        self.tor_socks = self.tor_socks.take().or(other.tor_socks);
        self.dns_bind = self.dns_bind.take().or(other.dns_bind);
        self.revision_cache_size = self
            .revision_cache_size
            .take()
            .or(other.revision_cache_size);
        self.datadir = self.datadir.take().or(other.datadir);
        self.disable_encryption = self.disable_encryption.take().or(other.disable_encryption);
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
    pub fn datadir(&self) -> &Path {
        self.datadir
            .as_deref()
            .unwrap_or_else(|| Path::new("/embassy-data"))
    }
    pub async fn db(&self) -> Result<PatchDb, Error> {
        let db_path = self.datadir().join("main").join("embassy.db");
        let db = PatchDb::open(&db_path)
            .await
            .with_ctx(|_| (crate::ErrorKind::Filesystem, db_path.display().to_string()))?;

        Ok(db)
    }
    #[instrument(skip_all)]
    pub async fn secret_store(&self) -> Result<PgPool, Error> {
        init_postgres(self.datadir()).await?;
        let secret_store =
            PgPool::connect_with(PgConnectOptions::new().database("secrets").username("root"))
                .await?;
        sqlx::migrate!()
            .run(&secret_store)
            .await
            .with_kind(crate::ErrorKind::Database)?;
        Ok(secret_store)
    }
}
