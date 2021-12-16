use tokio::process::Command;

use crate::context::rpc::RpcContextConfig;
use crate::db::model::ServerStatus;
use crate::install::PKG_DOCKER_DIR;
use crate::util::Invoke;
use crate::Error;

pub async fn init(cfg: &RpcContextConfig) -> Result<(), Error> {
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
    if tokio::fs::metadata(&tmp_docker).await.is_ok() {
        tokio::fs::remove_dir_all(&tmp_docker).await?;
    }
    Command::new("cp")
        .arg("-r")
        .arg("/var/lib/docker")
        .arg(&tmp_docker)
        .invoke(crate::ErrorKind::Filesystem)
        .await?;
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

    crate::install::load_images(cfg.datadir().join(PKG_DOCKER_DIR)).await?;
    tracing::info!("Loaded Package Docker Images");
    // Loading system images
    crate::install::load_images("/var/lib/embassy/system-images").await?;
    tracing::info!("Loaded System Docker Images");

    crate::ssh::sync_keys_from_db(&secret_store, "/root/.ssh/authorized_keys").await?;
    tracing::info!("Synced SSH Keys");

    crate::net::wifi::synchronize_wpa_supplicant_conf(&cfg.datadir().join("main")).await?;
    tracing::info!("Synchronized wpa_supplicant.conf");

    let db = cfg.db(&secret_store).await?;
    let mut handle = db.handle();
    let mut info = crate::db::DatabaseModel::new()
        .server_info()
        .get_mut(&mut handle)
        .await?;
    match info.status {
        ServerStatus::Running | ServerStatus::Updated | ServerStatus::BackingUp => {
            info.status = ServerStatus::Running;
        }
        ServerStatus::Updating => {
            info.update_progress = None;
            info.status = ServerStatus::Running;
        }
    }
    info.save(&mut handle).await?;

    crate::version::init(&mut handle).await?;

    Ok(())
}
