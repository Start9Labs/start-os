use std::time::Duration;

use tokio::process::Command;

use crate::context::rpc::RpcContextConfig;
use crate::db::model::ServerStatus;
use crate::install::PKG_DOCKER_DIR;
use crate::util::Invoke;
use crate::Error;

pub const SYSTEM_REBUILD_PATH: &str = "/embassy-os/system-rebuild";

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

    crate::ssh::sync_keys_from_db(&secret_store, "/root/.ssh/authorized_keys").await?;
    tracing::info!("Synced SSH Keys");
    let db = cfg.db(&secret_store, product_key).await?;

    let mut handle = db.handle();

    crate::net::wifi::synchronize_wpa_supplicant_conf(
        &cfg.datadir().join("main"),
        &*crate::db::DatabaseModel::new()
            .server_info()
            .last_wifi_region()
            .get(&mut handle, false)
            .await
            .map_err(|_e| {
                Error::new(
                    color_eyre::eyre::eyre!("Could not find the last wifi region"),
                    crate::ErrorKind::NotFound,
                )
            })?,
    )
    .await?;
    tracing::info!("Synchronized wpa_supplicant.conf");
    let mut info = crate::db::DatabaseModel::new()
        .server_info()
        .get_mut(&mut handle)
        .await?;
    info.status_info = ServerStatus {
        backing_up: false,
        updated: false,
        update_progress: None,
    };
    info.save(&mut handle).await?;

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
    }

    crate::version::init(&mut handle).await?;

    if should_rebuild {
        tokio::fs::remove_file(SYSTEM_REBUILD_PATH).await?;
    }

    Ok(())
}
