use std::time::Duration;

use patch_db::{DbHandle, LockReceipt, LockType};
use tokio::process::Command;

use crate::context::rpc::RpcContextConfig;
use crate::db::model::ServerStatus;
use crate::install::PKG_DOCKER_DIR;
use crate::sound::SHUTDOWN;
use crate::util::Invoke;
use crate::Error;

pub const SYSTEM_REBUILD_PATH: &str = "/embassy-os/system-rebuild";
pub const STANDBY_MODE_PATH: &str = "/embassy-os/standby";

pub async fn check_time_is_synchronized() -> Result<bool, Error> {
    Ok(String::from_utf8(
        Command::new("timedatectl")
            .arg("show")
            .arg("-p")
            .arg("NTPSynchronized")
            .invoke(crate::ErrorKind::Unknown)
            .await?,
    )?
    .trim()
        == "NTPSynchronized=yes")
}

pub struct InitReceipts {
    pub server_version: LockReceipt<crate::util::Version, ()>,
    pub version_range: LockReceipt<emver::VersionRange, ()>,
    pub last_wifi_region: LockReceipt<Option<isocountry::CountryCode>, ()>,
    pub status_info: LockReceipt<ServerStatus, ()>,
}
impl InitReceipts {
    pub async fn new(db: &mut impl DbHandle) -> Result<Self, Error> {
        let mut locks = Vec::new();

        let server_version = crate::db::DatabaseModel::new()
            .server_info()
            .version()
            .make_locker(LockType::Write)
            .add_to_keys(&mut locks);
        let version_range = crate::db::DatabaseModel::new()
            .server_info()
            .eos_version_compat()
            .make_locker(LockType::Write)
            .add_to_keys(&mut locks);
        let last_wifi_region = crate::db::DatabaseModel::new()
            .server_info()
            .last_wifi_region()
            .make_locker(LockType::Write)
            .add_to_keys(&mut locks);
        let status_info = crate::db::DatabaseModel::new()
            .server_info()
            .status_info()
            .into_model()
            .make_locker(LockType::Write)
            .add_to_keys(&mut locks);

        let skeleton_key = db.lock_all(locks).await?;
        Ok(Self {
            server_version: server_version.verify(&skeleton_key)?,
            version_range: version_range.verify(&skeleton_key)?,
            status_info: status_info.verify(&skeleton_key)?,
            last_wifi_region: last_wifi_region.verify(&skeleton_key)?,
        })
    }
}

pub async fn init(cfg: &RpcContextConfig, product_key: &str) -> Result<(), Error> {
    let should_rebuild = tokio::fs::metadata(SYSTEM_REBUILD_PATH).await.is_ok();
    let secret_store = cfg.secret_store().await?;
    let log_dir = cfg.datadir().join("main").join("logs");
    if tokio::fs::metadata(&log_dir).await.is_err() {
        tokio::fs::create_dir_all(&log_dir).await?;
    }
    crate::disk::mount::util::bind(&log_dir, "/var/log/journal", false).await?;
    Command::new("systemctl")
        .arg("restart")
        .arg("systemd-journald")
        .invoke(crate::ErrorKind::Journald)
        .await?;
    tracing::info!("Mounted Logs");
    let tmp_dir = cfg.datadir().join("package-data/tmp");
    if tokio::fs::metadata(&tmp_dir).await.is_err() {
        tokio::fs::create_dir_all(&tmp_dir).await?;
    }
    let tmp_docker = cfg.datadir().join("package-data/tmp/docker");
    let tmp_docker_exists = tokio::fs::metadata(&tmp_docker).await.is_ok();
    if should_rebuild || !tmp_docker_exists {
        if tmp_docker_exists {
            tokio::fs::remove_dir_all(&tmp_docker).await?;
        }
        Command::new("cp")
            .arg("-r")
            .arg("/var/lib/docker")
            .arg(&tmp_docker)
            .invoke(crate::ErrorKind::Filesystem)
            .await?;
    }
    Command::new("systemctl")
        .arg("stop")
        .arg("docker")
        .invoke(crate::ErrorKind::Docker)
        .await?;
    crate::disk::mount::util::bind(&tmp_docker, "/var/lib/docker", false).await?;
    Command::new("systemctl")
        .arg("reset-failed")
        .arg("docker")
        .invoke(crate::ErrorKind::Docker)
        .await?;
    Command::new("systemctl")
        .arg("start")
        .arg("docker")
        .invoke(crate::ErrorKind::Docker)
        .await?;
    tracing::info!("Mounted Docker Data");

    if should_rebuild || !tmp_docker_exists {
        tracing::info!("Loading System Docker Images");
        crate::install::load_images("/var/lib/embassy/system-images").await?;
        tracing::info!("Loaded System Docker Images");

        tracing::info!("Loading Package Docker Images");
        crate::install::load_images(cfg.datadir().join(PKG_DOCKER_DIR)).await?;
        tracing::info!("Loaded Package Docker Images");
    }

    crate::ssh::sync_keys_from_db(&secret_store, "/home/start9/.ssh/authorized_keys").await?;
    tracing::info!("Synced SSH Keys");
    let db = cfg.db(&secret_store, product_key).await?;

    let mut handle = db.handle();
    let receipts = InitReceipts::new(&mut handle).await?;

    crate::net::wifi::synchronize_wpa_supplicant_conf(
        &cfg.datadir().join("main"),
        &receipts.last_wifi_region.get(&mut handle).await?,
    )
    .await?;
    tracing::info!("Synchronized wpa_supplicant.conf");
    receipts
        .status_info
        .set(
            &mut handle,
            ServerStatus {
                updated: false,
                update_progress: None,
                backup_progress: None,
            },
        )
        .await?;

    let mut warn_time_not_synced = true;
    for _ in 0..60 {
        if check_time_is_synchronized().await? {
            warn_time_not_synced = false;
            break;
        }
        tokio::time::sleep(Duration::from_secs(1)).await;
    }
    if warn_time_not_synced {
        tracing::warn!("Timed out waiting for system time to synchronize");
    } else {
        tracing::info!("Syncronized system clock");
    }

    crate::version::init(&mut handle, &receipts).await?;

    if should_rebuild {
        tokio::fs::remove_file(SYSTEM_REBUILD_PATH).await?;
    }

    tracing::info!("System initialized.");

    Ok(())
}
