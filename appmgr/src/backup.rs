use std::os::unix::process::ExitStatusExt;
use std::path::Path;

use argon2::Config;
use emver::Version;
use futures::try_join;
use futures::TryStreamExt;
use rand::Rng;
use serde::Serialize;

use crate::util::from_yaml_async_reader;
use crate::util::to_yaml_async_writer;
use crate::util::Invoke;
use crate::util::PersistencePath;
use crate::version::VersionT;
use crate::Error;
use crate::ResultExt;

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct Metadata {
    pub app_version: Version,
    pub os_version: &'static Version,
}

pub async fn create_backup<P: AsRef<Path>>(
    path: P,
    app_id: &str,
    password: &str,
) -> Result<(), Error> {
    let path = tokio::fs::canonicalize(path).await?;
    crate::ensure_code!(
        path.is_dir(),
        crate::error::FILESYSTEM_ERROR,
        "Backup Path Must Be Directory"
    );
    let metadata_path = path.join("metadata.yaml");
    let pw_path = path.join("password");
    let data_path = path.join("data");
    let tor_path = path.join("tor");
    let volume_path = Path::new(crate::VOLUMES).join(app_id);
    let hidden_service_path =
        Path::new(crate::tor::HIDDEN_SERVICE_DIR_ROOT).join(format!("app-{}", app_id));

    if pw_path.exists() {
        use tokio::io::AsyncReadExt;

        let mut f = tokio::fs::File::open(&pw_path).await?;
        let mut hash = String::new();
        f.read_to_string(&mut hash).await?;
        crate::ensure_code!(
            argon2::verify_encoded(&hash, password.as_bytes())
                .with_code(crate::error::INVALID_BACKUP_PASSWORD)?,
            crate::error::INVALID_BACKUP_PASSWORD,
            "Invalid Backup Decryption Password"
        );
    }
    {
        // save password
        use tokio::io::AsyncWriteExt;
        let salt = rand::thread_rng().gen::<[u8; 32]>();
        let hash = argon2::hash_encoded(password.as_bytes(), &salt, &Config::default()).unwrap(); // this is safe because apparently the API was poorly designed
        let mut f = tokio::fs::File::create(pw_path).await?;
        f.write_all(hash.as_bytes()).await?;
        f.flush().await?;
    }

    let info = crate::apps::info(app_id).await?;
    to_yaml_async_writer(
        tokio::fs::File::create(metadata_path).await?,
        &Metadata {
            app_version: info.version,
            os_version: crate::version::Current::new().semver(),
        },
    )
    .await?;

    let status = crate::apps::status(app_id, false).await?;
    let exclude = if volume_path.is_dir() {
        let ignore_path = volume_path.join(".backupignore");
        if ignore_path.is_file() {
            use tokio::io::AsyncBufReadExt;
            tokio::io::BufReader::new(tokio::fs::File::open(ignore_path).await?)
                .lines()
                .try_filter(|l| futures::future::ready(!l.is_empty()))
                .try_collect()
                .await?
        } else {
            Vec::new()
        }
    } else {
        return Err(format_err!("Volume For {} Does Not Exist", app_id))
            .with_code(crate::error::NOT_FOUND);
    };
    let running = status.status == crate::apps::DockerStatus::Running;
    if running {
        crate::control::pause_app(&app_id).await?;
    }
    let mut data_cmd = tokio::process::Command::new("duplicity");
    for exclude in exclude {
        if exclude.starts_with('!') {
            data_cmd.arg(format!(
                "--include={}",
                volume_path.join(exclude.trim_start_matches('!')).display()
            ));
        } else {
            data_cmd.arg(format!("--exclude={}", volume_path.join(exclude).display()));
        }
    }
    let data_res = data_cmd
        .env("PASSPHRASE", password)
        .arg(volume_path)
        .arg(format!("file://{}", data_path.display()))
        .invoke("Duplicity")
        .await;
    let tor_res = tokio::process::Command::new("duplicity")
        .env("PASSPHRASE", password)
        .arg(hidden_service_path)
        .arg(format!("file://{}", tor_path.display()))
        .invoke("Duplicity")
        .await;
    if running {
        if crate::apps::info(&app_id).await?.needs_restart {
            crate::control::restart_app(&app_id).await?;
        } else {
            crate::control::resume_app(&app_id).await?;
        }
    }
    data_res?;
    tor_res?;

    Ok(())
}

pub async fn restore_backup<P: AsRef<Path>>(
    path: P,
    app_id: &str,
    password: &str,
) -> Result<(), Error> {
    let path = tokio::fs::canonicalize(path).await?;
    crate::ensure_code!(
        path.is_dir(),
        crate::error::FILESYSTEM_ERROR,
        "Backup Path Must Be Directory"
    );
    let metadata_path = path.join("metadata.yaml");
    let pw_path = path.join("password");
    let data_path = path.join("data");
    let tor_path = path.join("tor");
    let volume_path = Path::new(crate::VOLUMES).join(app_id);
    let hidden_service_path =
        Path::new(crate::tor::HIDDEN_SERVICE_DIR_ROOT).join(format!("app-{}", app_id));

    if pw_path.exists() {
        use tokio::io::AsyncReadExt;

        let mut f = tokio::fs::File::open(&pw_path).await?;
        let mut hash = String::new();
        f.read_to_string(&mut hash).await?;
        crate::ensure_code!(
            argon2::verify_encoded(&hash, password.as_bytes())
                .with_code(crate::error::INVALID_BACKUP_PASSWORD)?,
            crate::error::INVALID_BACKUP_PASSWORD,
            "Invalid Backup Decryption Password"
        );
    }

    let status = crate::apps::status(app_id, false).await?;
    let running = status.status == crate::apps::DockerStatus::Running;
    if running {
        crate::control::stop_app(app_id, true, false).await?;
    }

    let mut data_cmd = tokio::process::Command::new("duplicity");
    data_cmd
        .env("PASSPHRASE", password)
        .arg("--force")
        .arg(format!("file://{}", data_path.display()))
        .arg(&volume_path);

    let mut tor_cmd = tokio::process::Command::new("duplicity");
    tor_cmd
        .env("PASSPHRASE", password)
        .arg("--force")
        .arg(format!("file://{}", tor_path.display()))
        .arg(&hidden_service_path);

    let (data_output, tor_output) = try_join!(data_cmd.status(), tor_cmd.status())?;
    crate::ensure_code!(
        data_output.success(),
        crate::error::GENERAL_ERROR,
        "Duplicity Error"
    );
    crate::ensure_code!(
        tor_output.success(),
        crate::error::GENERAL_ERROR,
        "Duplicity Error"
    );

    // Fix the tor address in apps.yaml
    let mut yhdl = crate::apps::list_info_mut().await?;
    if let Some(app_info) = yhdl.get_mut(app_id) {
        app_info.tor_address = Some(crate::tor::read_tor_address(app_id, None).await?);
    }
    yhdl.commit().await?;

    tokio::fs::copy(
        metadata_path,
        Path::new(crate::VOLUMES)
            .join(app_id)
            .join("start9")
            .join("restore.yaml"),
    )
    .await?;

    // Attempt to configure the service with the config coming from restoration
    let cfg_path = Path::new(crate::VOLUMES)
        .join(app_id)
        .join("start9")
        .join("config.yaml");
    if cfg_path.exists() {
        let cfg = from_yaml_async_reader(tokio::fs::File::open(cfg_path).await?).await?;
        if let Err(e) = crate::config::configure(app_id, cfg, None, false).await {
            log::warn!("Could not restore backup configuration: {}", e);
        }
    }

    crate::tor::restart().await?;
    // Delete the fullchain certificate, so it can be regenerated with the restored tor pubkey address
    PersistencePath::from_ref("apps")
        .join(&app_id)
        .join("cert-local.fullchain.crt.pem")
        .delete()
        .await?;
    crate::tor::write_lan_services(
        &crate::tor::services_map(&PersistencePath::from_ref(crate::SERVICES_YAML)).await?,
    )
    .await?;
    let svc_exit = std::process::Command::new("service")
        .args(&["nginx", "reload"])
        .status()?;
    crate::ensure_code!(
        svc_exit.success(),
        crate::error::GENERAL_ERROR,
        "Failed to Reload Nginx: {}",
        svc_exit
            .code()
            .or_else(|| { svc_exit.signal().map(|a| 128 + a) })
            .unwrap_or(0)
    );

    Ok(())
}

pub async fn backup_to_partition(
    logicalname: &str,
    app_id: &str,
    password: &str,
) -> Result<(), Error> {
    let backup_mount_path = Path::new(crate::BACKUP_MOUNT_POINT);
    let guard = crate::disks::MountGuard::new(logicalname, &backup_mount_path).await?;
    let backup_dir_path = backup_mount_path.join(crate::BACKUP_DIR).join(app_id);
    tokio::fs::create_dir_all(&backup_dir_path).await?;

    let res = create_backup(backup_dir_path, app_id, password).await;

    guard.unmount().await?;

    res
}

pub async fn restore_from_partition(
    logicalname: &str,
    app_id: &str,
    password: &str,
) -> Result<(), Error> {
    let backup_mount_path = Path::new(crate::BACKUP_MOUNT_POINT);
    let guard = crate::disks::MountGuard::new(logicalname, &backup_mount_path).await?;
    let backup_dir_path = backup_mount_path.join(crate::BACKUP_DIR).join(app_id);

    let res = restore_backup(backup_dir_path, app_id, password).await;

    guard.unmount().await?;

    res
}
