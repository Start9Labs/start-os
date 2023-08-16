use std::collections::HashMap;
use std::fs::Permissions;
use std::os::unix::fs::PermissionsExt;
use std::path::Path;
use std::time::Duration;

use color_eyre::eyre::eyre;
use helpers::NonDetachingJoinHandle;
use models::ResultExt;
use patch_db::{DbHandle, LockReceipt, LockType};
use rand::random;
use sqlx::{Pool, Postgres};
use tokio::process::Command;

use crate::account::AccountInfo;
use crate::context::rpc::RpcContextConfig;
use crate::db::model::{ServerInfo, ServerStatus};
use crate::disk::mount::util::unmount;
use crate::install::PKG_ARCHIVE_DIR;
use crate::middleware::auth::LOCAL_AUTH_COOKIE_PATH;
use crate::sound::BEP;
use crate::system::time;
use crate::util::docker::{create_bridge_network, CONTAINER_DATADIR, CONTAINER_TOOL};
use crate::util::Invoke;
use crate::{Error, ARCH};

pub const SYSTEM_REBUILD_PATH: &str = "/media/embassy/config/system-rebuild";
pub const STANDBY_MODE_PATH: &str = "/media/embassy/config/standby";

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
    pub server_info: LockReceipt<ServerInfo, ()>,
    pub server_version: LockReceipt<crate::util::Version, ()>,
    pub version_range: LockReceipt<emver::VersionRange, ()>,
}
impl InitReceipts {
    pub async fn new(db: &mut impl DbHandle) -> Result<Self, Error> {
        let mut locks = Vec::new();

        let server_info = crate::db::DatabaseModel::new()
            .server_info()
            .make_locker(LockType::Write)
            .add_to_keys(&mut locks);
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

        let skeleton_key = db.lock_all(locks).await?;
        Ok(Self {
            server_info: server_info.verify(&skeleton_key)?,
            server_version: server_version.verify(&skeleton_key)?,
            version_range: version_range.verify(&skeleton_key)?,
        })
    }
}

// must be idempotent
pub async fn init_postgres(datadir: impl AsRef<Path>) -> Result<(), Error> {
    let db_dir = datadir.as_ref().join("main/postgresql");
    if tokio::process::Command::new("mountpoint")
        .arg("/var/lib/postgresql")
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .await?
        .success()
    {
        unmount("/var/lib/postgresql").await?;
    }
    let exists = tokio::fs::metadata(&db_dir).await.is_ok();
    if !exists {
        Command::new("cp")
            .arg("-ra")
            .arg("/var/lib/postgresql")
            .arg(&db_dir)
            .invoke(crate::ErrorKind::Filesystem)
            .await?;
    }
    Command::new("chown")
        .arg("-R")
        .arg("postgres:postgres")
        .arg(&db_dir)
        .invoke(crate::ErrorKind::Database)
        .await?;

    let mut pg_paths = tokio::fs::read_dir("/usr/lib/postgresql").await?;
    let mut pg_version = None;
    while let Some(pg_path) = pg_paths.next_entry().await? {
        let pg_path_version = pg_path
            .file_name()
            .to_str()
            .map(|v| v.parse())
            .transpose()?
            .unwrap_or(0);
        if pg_path_version > pg_version.unwrap_or(0) {
            pg_version = Some(pg_path_version)
        }
    }
    let pg_version = pg_version.ok_or_else(|| {
        Error::new(
            eyre!("could not determine postgresql version"),
            crate::ErrorKind::Database,
        )
    })?;

    crate::disk::mount::util::bind(&db_dir, "/var/lib/postgresql", false).await?;

    let pg_version_string = pg_version.to_string();
    let pg_version_path = db_dir.join(&pg_version_string);
    if tokio::fs::metadata(&pg_version_path).await.is_err() {
        let conf_dir = Path::new("/etc/postgresql").join(pg_version.to_string());
        let conf_dir_tmp = {
            let mut tmp = conf_dir.clone();
            tmp.set_extension("tmp");
            tmp
        };
        if tokio::fs::metadata(&conf_dir).await.is_ok() {
            tokio::fs::rename(&conf_dir, &conf_dir_tmp).await?;
        }
        let mut old_version = pg_version;
        while old_version > 13
        /* oldest pg version included in startos */
        {
            old_version -= 1;
            let old_datadir = db_dir.join(old_version.to_string());
            if tokio::fs::metadata(&old_datadir).await.is_ok() {
                Command::new("pg_upgradecluster")
                    .arg(old_version.to_string())
                    .arg("main")
                    .invoke(crate::ErrorKind::Database)
                    .await?;
                break;
            }
        }
        if tokio::fs::metadata(&conf_dir).await.is_ok() {
            if tokio::fs::metadata(&conf_dir).await.is_ok() {
                tokio::fs::remove_dir_all(&conf_dir).await?;
            }
            tokio::fs::rename(&conf_dir_tmp, &conf_dir).await?;
        }
    }

    Command::new("systemctl")
        .arg("start")
        .arg(format!("postgresql@{pg_version}-main.service"))
        .invoke(crate::ErrorKind::Database)
        .await?;
    if !exists {
        Command::new("sudo")
            .arg("-u")
            .arg("postgres")
            .arg("createuser")
            .arg("root")
            .invoke(crate::ErrorKind::Database)
            .await?;
        Command::new("sudo")
            .arg("-u")
            .arg("postgres")
            .arg("createdb")
            .arg("secrets")
            .arg("-O")
            .arg("root")
            .invoke(crate::ErrorKind::Database)
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
        .with_ctx(|_| (crate::ErrorKind::Filesystem, "mkdir -p /run/embassy"))?;
    if tokio::fs::metadata(LOCAL_AUTH_COOKIE_PATH).await.is_err() {
        tokio::fs::write(
            LOCAL_AUTH_COOKIE_PATH,
            base64::encode(random::<[u8; 32]>()).as_bytes(),
        )
        .await
        .with_ctx(|_| {
            (
                crate::ErrorKind::Filesystem,
                format!("write {}", LOCAL_AUTH_COOKIE_PATH),
            )
        })?;
        tokio::fs::set_permissions(LOCAL_AUTH_COOKIE_PATH, Permissions::from_mode(0o046)).await?;
        Command::new("chown")
            .arg("root:embassy")
            .arg(LOCAL_AUTH_COOKIE_PATH)
            .invoke(crate::ErrorKind::Filesystem)
            .await?;
    }

    let secret_store = cfg.secret_store().await?;
    tracing::info!("Opened Postgres");

    crate::ssh::sync_keys_from_db(&secret_store, "/home/start9/.ssh/authorized_keys").await?;
    tracing::info!("Synced SSH Keys");

    let account = AccountInfo::load(&secret_store).await?;
    let db = cfg.db(&account).await?;
    tracing::info!("Opened PatchDB");
    let mut handle = db.handle();
    let mut server_info = crate::db::DatabaseModel::new()
        .server_info()
        .get_mut(&mut handle)
        .await?;
    let receipts = InitReceipts::new(&mut handle).await?;

    // write to ca cert store
    tokio::fs::write(
        "/usr/local/share/ca-certificates/startos-root-ca.crt",
        account.root_ca_cert.to_pem()?,
    )
    .await?;
    Command::new("update-ca-certificates")
        .invoke(crate::ErrorKind::OpenSsl)
        .await?;

    if let Some(wifi_interface) = &cfg.wifi_interface {
        crate::net::wifi::synchronize_wpa_supplicant_conf(
            &cfg.datadir().join("main"),
            wifi_interface,
            &server_info.last_wifi_region,
        )
        .await?;
        tracing::info!("Synchronized WiFi");
    }

    let should_rebuild = tokio::fs::metadata(SYSTEM_REBUILD_PATH).await.is_ok()
        || &*server_info.version < &emver::Version::new(0, 3, 2, 0)
        || (*ARCH == "x86_64" && &*server_info.version < &emver::Version::new(0, 3, 4, 0));

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
        .invoke(crate::ErrorKind::Journald)
        .await?;
    tracing::info!("Mounted Logs");

    let tmp_dir = cfg.datadir().join("package-data/tmp");
    if tokio::fs::metadata(&tmp_dir).await.is_err() {
        tokio::fs::create_dir_all(&tmp_dir).await?;
    }
    let tmp_docker = cfg
        .datadir()
        .join(format!("package-data/tmp/{CONTAINER_TOOL}"));
    let tmp_docker_exists = tokio::fs::metadata(&tmp_docker).await.is_ok();
    if should_rebuild && tmp_docker_exists {
        tokio::fs::remove_dir_all(&tmp_docker).await?;
    }
    if CONTAINER_TOOL == "docker" {
        Command::new("systemctl")
            .arg("stop")
            .arg("docker")
            .invoke(crate::ErrorKind::Docker)
            .await?;
    }
    crate::disk::mount::util::bind(&tmp_docker, CONTAINER_DATADIR, false).await?;

    if CONTAINER_TOOL == "docker" {
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
    }
    tracing::info!("Mounted Docker Data");

    if should_rebuild || !tmp_docker_exists {
        tracing::info!("Creating Docker Network");
        create_bridge_network("start9", "172.18.0.1/24", "br-start9").await?;
        tracing::info!("Created Docker Network");

        tracing::info!("Loading System Docker Images");
        crate::install::load_images("/usr/lib/embassy/system-images").await?;
        tracing::info!("Loaded System Docker Images");

        tracing::info!("Loading Package Docker Images");
        crate::install::load_images(cfg.datadir().join(PKG_ARCHIVE_DIR)).await?;
        tracing::info!("Loaded Package Docker Images");
    }

    tracing::info!("Enabling Docker QEMU Emulation");
    Command::new(CONTAINER_TOOL)
        .arg("run")
        .arg("--privileged")
        .arg("--rm")
        .arg("start9/x_system/binfmt")
        .arg("--install")
        .arg("all")
        .invoke(crate::ErrorKind::Docker)
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

    if server_info.zram {
        crate::system::enable_zram().await?
    }
    server_info.ip_info = crate::net::dhcp::init_ips().await?;
    server_info.status_info = ServerStatus {
        updated: false,
        update_progress: None,
        backup_progress: None,
    };

    server_info.system_start_time = time().await?;

    server_info.save(&mut handle).await?;

    crate::version::init(&mut handle, &secret_store, &receipts).await?;

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
