#[macro_use]
extern crate failure;
#[macro_use]
extern crate pest_derive;

pub const TOR_RC: &'static str = "/root/appmgr/tor/torrc";
pub const SERVICES_YAML: &'static str = "tor/services.yaml";
pub const VOLUMES: &'static str = "/root/volumes";
pub const PERSISTENCE_DIR: &'static str = "/root/appmgr";
pub const TMP_DIR: &'static str = "/root/tmp/appmgr";
pub const BACKUP_MOUNT_POINT: &'static str = "/mnt/backup_drive";
pub const BACKUP_DIR: &'static str = "Embassy Backups";
pub const BUFFER_SIZE: usize = 1024;
pub const HOST_IP: [u8; 4] = [172, 18, 0, 1];

lazy_static::lazy_static! {
    pub static ref REGISTRY_URL: String = std::env::var("REGISTRY_URL").unwrap_or_else(|_| "https://registry.start9labs.com".to_owned());
    pub static ref SYS_REGISTRY_URL: String = format!("{}/sys", *REGISTRY_URL);
    pub static ref APP_REGISTRY_URL: String = format!("{}/apps", *REGISTRY_URL);
    pub static ref QUIET: tokio::sync::RwLock<bool> = tokio::sync::RwLock::new(!std::env::var("APPMGR_QUIET").map(|a| a == "0").unwrap_or(true));
}

pub mod apps;
pub mod backup;
pub mod config;
pub mod control;
pub mod dependencies;
pub mod disks;
pub mod error;
pub mod index;
pub mod inspect;
pub mod install;
pub mod logs;
pub mod manifest;
pub mod pack;
pub mod registry;
pub mod remove;
pub mod tor;
pub mod update;
pub mod util;
pub mod version;

pub use config::{configure, Config};
pub use control::{restart_app, start_app, stop_app, stop_dependents};
pub use error::{Error, ResultExt};
pub use install::{install_name, install_path, install_url};
pub use logs::{logs, notifications, stats, LogOptions};
pub use pack::{pack, verify};
pub use remove::remove;
pub use update::update;
pub use version::{init, self_update};
