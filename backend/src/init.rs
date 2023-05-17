use std::collections::HashMap;
use std::fs::Permissions;
use std::os::unix::fs::PermissionsExt;
use std::path::Path;
use std::process::Stdio;
use std::time::Duration;

use color_eyre::eyre::eyre;
use helpers::NonDetachingJoinHandle;
use models::ResultExt;
use rand::random;
use sqlx::{Pool, Postgres};
use tokio::process::Command;

use crate::account::AccountInfo;
use crate::context::rpc::RpcContextConfig;
use crate::db::model::ServerStatus;
use crate::install::PKG_ARCHIVE_DIR;
use crate::middleware::auth::LOCAL_AUTH_COOKIE_PATH;
use crate::prelude::*;
use crate::sound::BEP;
use crate::system::time;
use crate::util::Invoke;
use crate::ARCH;

pub const SYSTEM_REBUILD_PATH: &str = "/media/embassy/config/system-rebuild";
pub const STANDBY_MODE_PATH: &str = "/media/embassy/config/standby";

pub async fn check_time_is_synchronized() -> Result<bool, Error> {
    Ok(String::from_utf8(
        Command::new("timedatectl")
            .arg("show")
            .arg("-p")
            .arg("NTPSynchronized")
            .invoke(ErrorKind::Unknown)
            .await?,
    )?
    .trim()
        == "NTPSynchronized=yes")
}

pub async fn pgloader(
    old_db_path: impl AsRef<Path>,
    batch_rows: usize,
    prefetch_rows: usize,
) -> Result<(), Error> {
    tokio::fs::write(
        "/etc/embassy/migrate.load",
        format!(
            include_str!("migrate.load"),
            sqlite_path = old_db_path.as_ref().display(),
            batch_rows = batch_rows,
            prefetch_rows = prefetch_rows
        ),
    )
    .await?;
    match tokio::fs::remove_dir_all("/tmp/pgloader").await {
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(()),
        a => a,
    }?;
    tracing::info!("Running pgloader");
    let out = Command::new("pgloader")
        .arg("-v")
        .arg("/etc/embassy/migrate.load")
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .await?;
    let stdout = String::from_utf8(out.stdout)?;
    for line in stdout.lines() {
        tracing::debug!("pgloader: {}", line);
    }
    let stderr = String::from_utf8(out.stderr)?;
    for line in stderr.lines() {
        tracing::debug!("pgloader err: {}", line);
    }
    tracing::debug!("pgloader exited with code {:?}", out.status);
    if let Some(err) = stdout.lines().chain(stderr.lines()).find_map(|l| {
        if l.split_ascii_whitespace()
            .any(|word| word == "ERROR" || word == "FATAL")
        {
            Some(l)
        } else {
            None
        }
    }) {
        return Err(Error::new(
            eyre!("pgloader error: {}", err),
            ErrorKind::Database,
        ));
    }
    tokio::fs::rename(
        old_db_path.as_ref(),
        old_db_path.as_ref().with_extension("bak"),
    )
    .await?;
    Ok(())
}

// must be idempotent
pub async fn init_postgres(datadir: impl AsRef<Path>) -> Result<(), Error> {
    let db_dir = datadir.as_ref().join("main/postgresql");
    let is_mountpoint = || async {
        Ok::<_, Error>(
            tokio::process::Command::new("mountpoint")
                .arg("/var/lib/postgresql")
                .stdout(std::process::Stdio::null())
                .stderr(std::process::Stdio::null())
                .status()
                .await?
                .success(),
        )
    };
    let exists = tokio::fs::metadata(&db_dir).await.is_ok();
    if !exists {
        Command::new("cp")
            .arg("-ra")
            .arg("/var/lib/postgresql")
            .arg(&db_dir)
            .invoke(ErrorKind::Filesystem)
            .await?;
    }
    if !is_mountpoint().await? {
        crate::disk::mount::util::bind(&db_dir, "/var/lib/postgresql", false).await?;
    }
    Command::new("chown")
        .arg("-R")
        .arg("postgres")
        .arg("/var/lib/postgresql")
        .invoke(ErrorKind::Database)
        .await?;
    Command::new("systemctl")
        .arg("start")
        .arg("postgresql")
        .invoke(ErrorKind::Database)
        .await?;
    if !exists {
        Command::new("sudo")
            .arg("-u")
            .arg("postgres")
            .arg("createuser")
            .arg("root")
            .invoke(ErrorKind::Database)
            .await?;
        Command::new("sudo")
            .arg("-u")
            .arg("postgres")
            .arg("createdb")
            .arg("secrets")
            .arg("-O")
            .arg("root")
            .invoke(ErrorKind::Database)
            .await?;
    }
    Ok(())
}

pub struct InitResult {
    pub secret_store: Pool<Postgres>,
    pub db: patch_db::PatchDb,
}

pub async fn init(cfg: &RpcContextConfig) -> Result<InitResult, Error> {
    tokio::fs::create_dir_all("/run/embassy")
        .await
        .with_ctx(|_| (ErrorKind::Filesystem, "mkdir -p /run/embassy"))?;
    if tokio::fs::metadata(LOCAL_AUTH_COOKIE_PATH).await.is_err() {
        tokio::fs::write(
            LOCAL_AUTH_COOKIE_PATH,
            base64::encode(random::<[u8; 32]>()).as_bytes(),
        )
        .await
        .with_ctx(|_| {
            (
                ErrorKind::Filesystem,
                format!("write {}", LOCAL_AUTH_COOKIE_PATH),
            )
        })?;
        tokio::fs::set_permissions(LOCAL_AUTH_COOKIE_PATH, Permissions::from_mode(0o046)).await?;
        Command::new("chown")
            .arg("root:embassy")
            .arg(LOCAL_AUTH_COOKIE_PATH)
            .invoke(ErrorKind::Filesystem)
            .await?;
    }

    Command::new("make-ssl-cert")
        .arg("generate-default-snakeoil")
        .arg("--force-overwrite")
        .invoke(ErrorKind::OpenSsl)
        .await?;

    let secret_store = cfg.secret_store().await?;
    tracing::info!("Opened Postgres");

    crate::ssh::sync_keys_from_db(&secret_store, "/home/start9/.ssh/authorized_keys").await?;
    tracing::info!("Synced SSH Keys");

    let account = AccountInfo::load(&secret_store).await?;
    let db = cfg.db(&account).await?;
    tracing::info!("Opened PatchDB");

    // write to ca cert store
    tokio::fs::write(
        "/usr/local/share/ca-certificates/startos-root-ca.crt",
        account.root_ca_cert.to_pem()?,
    )
    .await?;
    Command::new("update-ca-certificates")
        .invoke(ErrorKind::OpenSsl)
        .await?;

    let (version, last_wifi_region) = db
        .apply_fn(|mut v| {
            Ok((
                v.server_info().version().get()?,
                v.server_info().last_wifi_region().get()?,
            ))
        })
        .await?;

    if let Some(wifi_interface) = &cfg.wifi_interface {
        crate::net::wifi::synchronize_wpa_supplicant_conf(
            &cfg.datadir().join("main"),
            wifi_interface,
            &last_wifi_region,
        )
        .await?;
        tracing::info!("Synchronized WiFi");
    }

    let should_rebuild = tokio::fs::metadata(SYSTEM_REBUILD_PATH).await.is_ok()
        || &*version < &emver::Version::new(0, 3, 2, 0)
        || (*ARCH == "x86_64" && &*version < &emver::Version::new(0, 3, 4, 0));

    let song = if should_rebuild {
        Some(NonDetachingJoinHandle::from(tokio::spawn(async {
            loop {
                BEP.play().await.unwrap();
                BEP.play().await.unwrap();
                tokio::time::sleep(Duration::from_secs(60)).await;
            }
        })))
    } else {
        None
    };

    let log_dir = cfg.datadir().join("main/logs");
    if tokio::fs::metadata(&log_dir).await.is_err() {
        tokio::fs::create_dir_all(&log_dir).await?;
    }
    let current_machine_id = tokio::fs::read_to_string("/etc/machine-id").await?;
    let mut machine_ids = tokio::fs::read_dir(&log_dir).await?;
    while let Some(machine_id) = machine_ids.next_entry().await? {
        if machine_id.file_name().to_string_lossy().trim() != current_machine_id.trim() {
            tokio::fs::remove_dir_all(machine_id.path()).await?;
        }
    }
    crate::disk::mount::util::bind(&log_dir, "/var/log/journal", false).await?;
    Command::new("systemctl")
        .arg("restart")
        .arg("systemd-journald")
        .invoke(ErrorKind::Journald)
        .await?;
    tracing::info!("Mounted Logs");

    let tmp_dir = cfg.datadir().join("package-data/tmp");
    if tokio::fs::metadata(&tmp_dir).await.is_err() {
        tokio::fs::create_dir_all(&tmp_dir).await?;
    }
    let tmp_docker = cfg.datadir().join("package-data/tmp/docker");
    let tmp_docker_exists = tokio::fs::metadata(&tmp_docker).await.is_ok();
    if should_rebuild && tmp_docker_exists {
        tokio::fs::remove_dir_all(&tmp_docker).await?;
    }
    Command::new("systemctl")
        .arg("stop")
        .arg("docker")
        .invoke(ErrorKind::Docker)
        .await?;
    crate::disk::mount::util::bind(&tmp_docker, "/var/lib/docker", false).await?;
    Command::new("systemctl")
        .arg("reset-failed")
        .arg("docker")
        .invoke(ErrorKind::Docker)
        .await?;
    Command::new("systemctl")
        .arg("start")
        .arg("docker")
        .invoke(ErrorKind::Docker)
        .await?;
    tracing::info!("Mounted Docker Data");

    if should_rebuild || !tmp_docker_exists {
        tracing::info!("Creating Docker Network");
        let mut cmd = Command::new("docker");
        cmd.arg("network")
            .arg("create")
            .arg("--driver=bridge")
            .arg("--subnet")
            .arg("172.18.0.0/16")
            .arg("-o")
            .arg("com.docker.network.bridge.name=br-start9")
            .arg("start9");
        tracing::info!("Created Docker Network");

        tracing::info!("Loading System Docker Images");
        crate::install::load_images("/usr/lib/embassy/system-images").await?;
        tracing::info!("Loaded System Docker Images");

        tracing::info!("Loading Package Docker Images");
        crate::install::load_images(cfg.datadir().join(PKG_ARCHIVE_DIR)).await?;
        tracing::info!("Loaded Package Docker Images");
    }

    tracing::info!("Enabling Docker QEMU Emulation");
    Command::new("docker")
        .arg("run")
        .arg("--privileged")
        .arg("--rm")
        .arg("start9/x_system/binfmt")
        .arg("--install")
        .arg("all")
        .invoke(ErrorKind::Docker)
        .await?;
    tracing::info!("Enabled Docker QEMU Emulation");

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

    Command::new("systemctl")
        .arg("start")
        .arg("tor")
        .invoke(ErrorKind::Tor)
        .await?;

    let ip_info = crate::net::dhcp::init_ips().await?;
    let time = time().await?;
    db.apply_fn(|mut v| {
        let mut server_info = v.server_info();
        server_info.ip_info().set(&ip_info)?;
        server_info.status_info().set(&ServerStatus {
            updated: false,
            update_progress: None,
            backup_progress: None,
            shutting_down: false,
        })?;
        server_info.system_start_time().set(&time)?;
        Ok(())
    })
    .await?;

    crate::version::init(&secret_store, &db).await?;

    if should_rebuild {
        match tokio::fs::remove_file(SYSTEM_REBUILD_PATH).await {
            Ok(()) => Ok(()),
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(()),
            Err(e) => Err(e),
        }?;
    }

    drop(song);

    tracing::info!("System initialized.");

    Ok(InitResult { secret_store, db })
}
