use std::fs::Permissions;
use std::os::unix::fs::PermissionsExt;
use std::path::Path;
use std::time::{Duration, SystemTime};

use color_eyre::eyre::eyre;
use models::ResultExt;
use rand::random;
use sqlx::{Pool, Postgres};
use tokio::process::Command;
use tracing::instrument;

use crate::account::AccountInfo;
use crate::context::config::ServerConfig;
use crate::db::model::ServerStatus;
use crate::disk::mount::util::unmount;
use crate::middleware::auth::LOCAL_AUTH_COOKIE_PATH;
use crate::prelude::*;
use crate::util::cpupower::{get_available_governors, get_preferred_governor, set_governor};
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

// must be idempotent
#[tracing::instrument(skip_all)]
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
    if exists
    // maybe migrate
    {
        let incomplete_path = db_dir.join(format!("{pg_version}.migration.incomplete"));
        if tokio::fs::metadata(&incomplete_path).await.is_ok() // previous migration was incomplete
        && tokio::fs::metadata(&pg_version_path).await.is_ok()
        {
            tokio::fs::remove_dir_all(&pg_version_path).await?;
        }
        if tokio::fs::metadata(&pg_version_path).await.is_err()
        // need to migrate
        {
            let conf_dir = Path::new("/etc/postgresql").join(pg_version.to_string());
            let conf_dir_tmp = {
                let mut tmp = conf_dir.clone();
                tmp.set_extension("tmp");
                tmp
            };
            if tokio::fs::metadata(&conf_dir).await.is_ok() {
                Command::new("mv")
                    .arg(&conf_dir)
                    .arg(&conf_dir_tmp)
                    .invoke(ErrorKind::Filesystem)
                    .await?;
            }
            let mut old_version = pg_version;
            while old_version > 13
            /* oldest pg version included in startos */
            {
                old_version -= 1;
                let old_datadir = db_dir.join(old_version.to_string());
                if tokio::fs::metadata(&old_datadir).await.is_ok() {
                    tokio::fs::File::create(&incomplete_path)
                        .await?
                        .sync_all()
                        .await?;
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
                Command::new("mv")
                    .arg(&conf_dir_tmp)
                    .arg(&conf_dir)
                    .invoke(ErrorKind::Filesystem)
                    .await?;
            }
            tokio::fs::remove_file(&incomplete_path).await?;
        }
        if tokio::fs::metadata(&incomplete_path).await.is_ok() {
            unreachable!() // paranoia
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
    pub db: patch_db::PatchDb,
}

#[instrument(skip_all)]
pub async fn init(cfg: &ServerConfig) -> Result<InitResult, Error> {
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

    let db = cfg.db().await?;
    let peek = db.peek().await;
    tracing::info!("Opened PatchDB");

    crate::ssh::sync_keys(
        &peek.as_private().as_ssh_keys().de()?,
        "/home/start9/.ssh/authorized_keys",
    )
    .await?;
    tracing::info!("Synced SSH Keys");

    let account = AccountInfo::load(&peek)?;

    let mut server_info = peek.as_public().as_server_info().de()?;

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
    match Command::new("chattr")
        .arg("-R")
        .arg("+C")
        .arg("/var/log/journal")
        .invoke(ErrorKind::Filesystem)
        .await
    {
        Ok(_) => Ok(()),
        Err(e) if e.source.to_string().contains("Operation not supported") => Ok(()),
        Err(e) => Err(e),
    }?;
    Command::new("systemctl")
        .arg("restart")
        .arg("systemd-journald")
        .invoke(crate::ErrorKind::Journald)
        .await?;
    tracing::info!("Mounted Logs");

    let tmp_dir = cfg.datadir().join("package-data/tmp");
    if should_rebuild && tokio::fs::metadata(&tmp_dir).await.is_ok() {
        tokio::fs::remove_dir_all(&tmp_dir).await?;
    }
    if tokio::fs::metadata(&tmp_dir).await.is_err() {
        tokio::fs::create_dir_all(&tmp_dir).await?;
    }
    let tmp_var = cfg.datadir().join(format!("package-data/tmp/var"));
    if tokio::fs::metadata(&tmp_var).await.is_ok() {
        tokio::fs::remove_dir_all(&tmp_var).await?;
    }
    crate::disk::mount::util::bind(&tmp_var, "/var/tmp", false).await?;

    let governor = if let Some(governor) = &server_info.governor {
        if get_available_governors().await?.contains(governor) {
            Some(governor)
        } else {
            tracing::warn!("CPU Governor \"{governor}\" Not Available");
            None
        }
    } else {
        get_preferred_governor().await?
    };
    if let Some(governor) = governor {
        tracing::info!("Setting CPU Governor to \"{governor}\"");
        set_governor(governor).await?;
        tracing::info!("Set CPU Governor");
    }

    server_info.ntp_synced = false;
    let mut not_made_progress = 0u32;
    for _ in 0..1800 {
        if check_time_is_synchronized().await? {
            server_info.ntp_synced = true;
            break;
        }
        let t = SystemTime::now();
        tokio::time::sleep(Duration::from_secs(1)).await;
        if t.elapsed()
            .map(|t| t > Duration::from_secs_f64(1.1))
            .unwrap_or(true)
        {
            not_made_progress = 0;
        } else {
            not_made_progress += 1;
        }
        if not_made_progress > 30 {
            break;
        }
    }
    if !server_info.ntp_synced {
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
        shutting_down: false,
        restarting: false,
    };

    db.mutate(|v| {
        v.as_public_mut().as_server_info_mut().ser(&server_info)?;
        Ok(())
    })
    .await?;

    crate::version::init(&db).await?;

    db.mutate(|d| {
        let model = d.de()?;
        d.ser(&model)
    })
    .await?;

    if should_rebuild {
        match tokio::fs::remove_file(SYSTEM_REBUILD_PATH).await {
            Ok(()) => Ok(()),
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(()),
            Err(e) => Err(e),
        }?;
    }

    tracing::info!("System initialized.");

    Ok(InitResult { db })
}
