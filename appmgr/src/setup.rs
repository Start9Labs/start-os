use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::Duration;

use color_eyre::eyre::eyre;
use futures::future::BoxFuture;
use futures::{FutureExt, TryFutureExt, TryStreamExt};
use openssl::x509::X509;
use rpc_toolkit::command;
use rpc_toolkit::yajrc::RpcError;
use serde::{Deserialize, Serialize};
use sqlx::{Executor, Sqlite};
use tokio::fs::File;
use tokio::io::AsyncWriteExt;
use torut::onion::{OnionAddressV3, TorSecretKeyV3};
use tracing::instrument;

use crate::backup::restore::recover_full_embassy;
use crate::backup::target::BackupTargetFS;
use crate::context::rpc::RpcContextConfig;
use crate::context::SetupContext;
use crate::db::model::RecoveredPackageInfo;
use crate::disk::main::DEFAULT_PASSWORD;
use crate::disk::mount::filesystem::block_dev::BlockDev;
use crate::disk::mount::filesystem::cifs::Cifs;
use crate::disk::mount::guard::TmpMountGuard;
use crate::disk::util::{pvscan, recovery_info, DiskInfo, EmbassyOsRecoveryInfo};
use crate::hostname::PRODUCT_KEY_PATH;
use crate::id::Id;
use crate::init::init;
use crate::install::PKG_PUBLIC_DIR;
use crate::net::ssl::SslManager;
use crate::s9pk::manifest::PackageId;
use crate::sound::BEETHOVEN;
use crate::util::io::{dir_size, from_yaml_async_reader};
use crate::util::Version;
use crate::volume::{data_dir, VolumeId};
use crate::{ensure_code, Error, ResultExt};

#[instrument(skip(secrets))]
pub async fn password_hash<Ex>(secrets: &mut Ex) -> Result<String, Error>
where
    for<'a> &'a mut Ex: Executor<'a, Database = Sqlite>,
{
    let password = sqlx::query!("SELECT password FROM account")
        .fetch_one(secrets)
        .await?
        .password;

    Ok(password)
}

#[command(subcommands(status, disk, execute, recovery, cifs, complete))]
pub fn setup() -> Result<(), Error> {
    Ok(())
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct StatusRes {
    product_key: bool,
    migrating: bool,
}

#[command(rpc_only, metadata(authenticated = false))]
pub async fn status(#[context] ctx: SetupContext) -> Result<StatusRes, Error> {
    Ok(StatusRes {
        product_key: tokio::fs::metadata("/embassy-os/product_key.txt")
            .await
            .is_ok(),
        migrating: ctx.recovery_status.read().await.is_some(),
    })
}

#[command(subcommands(list_disks))]
pub fn disk() -> Result<(), Error> {
    Ok(())
}

#[command(rename = "list", rpc_only, metadata(authenticated = false))]
pub async fn list_disks() -> Result<Vec<DiskInfo>, Error> {
    crate::disk::list(None).await
}

#[command(subcommands(v2, recovery_status))]
pub fn recovery() -> Result<(), Error> {
    Ok(())
}

#[command(subcommands(set))]
pub fn v2() -> Result<(), Error> {
    Ok(())
}

#[command(rpc_only, metadata(authenticated = false))]
pub async fn set(#[context] ctx: SetupContext, #[arg] logicalname: PathBuf) -> Result<(), Error> {
    let guard = TmpMountGuard::mount(&BlockDev::new(&logicalname)).await?;
    let product_key = tokio::fs::read_to_string(guard.as_ref().join("root/agent/product_key"))
        .await?
        .trim()
        .to_owned();
    guard.unmount().await?;
    *ctx.cached_product_key.write().await = Some(Arc::new(product_key));
    *ctx.selected_v2_drive.write().await = Some(logicalname);
    Ok(())
}

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct RecoveryStatus {
    pub bytes_transferred: u64,
    pub total_bytes: u64,
    pub complete: bool,
}

#[command(rename = "status", rpc_only, metadata(authenticated = false))]
pub async fn recovery_status(
    #[context] ctx: SetupContext,
) -> Result<Option<RecoveryStatus>, RpcError> {
    ctx.recovery_status.read().await.clone().transpose()
}

#[command(subcommands(verify_cifs))]
pub fn cifs() -> Result<(), Error> {
    Ok(())
}

#[command(rename = "verify", rpc_only)]
pub async fn verify_cifs(
    #[arg] hostname: String,
    #[arg] path: PathBuf,
    #[arg] username: String,
    #[arg] password: Option<String>,
) -> Result<EmbassyOsRecoveryInfo, Error> {
    let guard = TmpMountGuard::mount(&Cifs {
        hostname,
        path,
        username,
        password,
    })
    .await?;
    let embassy_os = recovery_info(&guard).await?;
    guard.unmount().await?;
    embassy_os.ok_or_else(|| Error::new(eyre!("No Backup Found"), crate::ErrorKind::NotFound))
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct SetupResult {
    tor_address: String,
    lan_address: String,
    root_ca: String,
}

#[command(rpc_only)]
pub async fn execute(
    #[context] ctx: SetupContext,
    #[arg(rename = "embassy-logicalname")] embassy_logicalname: PathBuf,
    #[arg(rename = "embassy-password")] embassy_password: String,
    #[arg(rename = "recovery-source")] mut recovery_source: Option<BackupTargetFS>,
    #[arg(rename = "recovery-password")] recovery_password: Option<String>,
) -> Result<SetupResult, Error> {
    if let Some(v2_drive) = &*ctx.selected_v2_drive.read().await {
        recovery_source = Some(BackupTargetFS::Disk(BlockDev::new(v2_drive.clone())))
    }
    match execute_inner(
        ctx,
        embassy_logicalname,
        embassy_password,
        recovery_source,
        recovery_password,
    )
    .await
    {
        Ok((tor_addr, root_ca)) => {
            tracing::info!("Setup Successful! Tor Address: {}", tor_addr);
            Ok(SetupResult {
                tor_address: format!("http://{}", tor_addr),
                lan_address: format!("https://embassy-{}.local", crate::hostname::get_id().await?),
                root_ca: String::from_utf8(root_ca.to_pem()?)?,
            })
        }
        Err(e) => {
            tracing::error!("Error Setting Up Embassy: {}", e);
            tracing::debug!("{:?}", e);
            Err(e)
        }
    }
}

#[instrument(skip(ctx))]
#[command(rpc_only)]
pub async fn complete(#[context] ctx: SetupContext) -> Result<(), Error> {
    let guid = if let Some(guid) = &*ctx.disk_guid.read().await {
        guid.clone()
    } else {
        return Err(Error::new(
            eyre!("setup.execute has not completed successfully"),
            crate::ErrorKind::InvalidRequest,
        ));
    };
    if tokio::fs::metadata(PRODUCT_KEY_PATH).await.is_err() {
        let mut pkey_file = File::create(PRODUCT_KEY_PATH).await?;
        pkey_file
            .write_all(ctx.product_key().await?.as_bytes())
            .await?;
        pkey_file.sync_all().await?;
    }
    let mut guid_file = File::create("/embassy-os/disk.guid").await?;
    guid_file.write_all(guid.as_bytes()).await?;
    guid_file.sync_all().await?;
    ctx.shutdown.send(()).expect("failed to shutdown");
    Ok(())
}

#[instrument(skip(ctx, embassy_password, recovery_password))]
pub async fn execute_inner(
    ctx: SetupContext,
    embassy_logicalname: PathBuf,
    embassy_password: String,
    recovery_source: Option<BackupTargetFS>,
    recovery_password: Option<String>,
) -> Result<(OnionAddressV3, X509), Error> {
    if ctx.recovery_status.read().await.is_some() {
        return Err(Error::new(
            eyre!("Cannot execute setup while in recovery!"),
            crate::ErrorKind::InvalidRequest,
        ));
    }
    let guid = Arc::new(
        crate::disk::main::create(
            &[embassy_logicalname],
            &pvscan().await?,
            &ctx.datadir,
            DEFAULT_PASSWORD,
        )
        .await?,
    );
    crate::disk::main::import(&*guid, &ctx.datadir, DEFAULT_PASSWORD).await?;

    let res = if let Some(recovery_source) = recovery_source {
        let (tor_addr, root_ca, recover_fut) = recover(
            ctx.clone(),
            guid.clone(),
            embassy_password,
            recovery_source,
            recovery_password,
        )
        .await?;
        init(&RpcContextConfig::load(ctx.config_path.as_ref()).await?).await?;
        tokio::spawn(async move {
            if let Err(e) = recover_fut
                .and_then(|_| async {
                    *ctx.disk_guid.write().await = Some(guid);
                    if let Some(Ok(recovery_status)) = &mut *ctx.recovery_status.write().await {
                        recovery_status.complete = true;
                    }
                    Ok(())
                })
                .await
            {
                BEETHOVEN.play().await.unwrap_or_default(); // ignore error in playing the song
                tracing::error!("Error recovering drive!: {}", e);
                tracing::debug!("{:?}", e);
                *ctx.recovery_status.write().await = Some(Err(e.into()));
            }
        });
        (tor_addr, root_ca)
    } else {
        let res = fresh_setup(&ctx, &embassy_password).await?;
        init(&RpcContextConfig::load(ctx.config_path.as_ref()).await?).await?;
        *ctx.disk_guid.write().await = Some(guid);
        res
    };

    Ok(res)
}

async fn fresh_setup(
    ctx: &SetupContext,
    embassy_password: &str,
) -> Result<(OnionAddressV3, X509), Error> {
    let password = argon2::hash_encoded(
        embassy_password.as_bytes(),
        &rand::random::<[u8; 16]>()[..],
        &argon2::Config::default(),
    )
    .with_kind(crate::ErrorKind::PasswordHashGeneration)?;
    let tor_key = TorSecretKeyV3::generate();
    let key_vec = tor_key.as_bytes().to_vec();
    let sqlite_pool = ctx.secret_store().await?;
    sqlx::query!(
        "REPLACE INTO account (id, password, tor_key) VALUES (?, ?, ?)",
        0,
        password,
        key_vec,
    )
    .execute(&mut sqlite_pool.acquire().await?)
    .await?;
    let (_, root_ca) = SslManager::init(sqlite_pool.clone())
        .await?
        .export_root_ca()
        .await?;
    sqlite_pool.close().await;
    Ok((tor_key.public().get_onion_address(), root_ca))
}

#[instrument(skip(ctx, embassy_password, recovery_password))]
async fn recover(
    ctx: SetupContext,
    guid: Arc<String>,
    embassy_password: String,
    recovery_source: BackupTargetFS,
    recovery_password: Option<String>,
) -> Result<(OnionAddressV3, X509, BoxFuture<'static, Result<(), Error>>), Error> {
    let recovery_source = TmpMountGuard::mount(&recovery_source).await?;
    let recovery_version = recovery_info(&recovery_source)
        .await?
        .as_ref()
        .map(|i| i.version.clone())
        .unwrap_or_default();
    let res = if recovery_version.major() == 0 && recovery_version.minor() == 2 {
        let (tor_addr, root_ca) = fresh_setup(&ctx, &embassy_password).await?;
        (
            tor_addr,
            root_ca,
            recover_v2(ctx.clone(), recovery_source).boxed(),
        )
    } else if recovery_version.major() == 0 && recovery_version.minor() == 3 {
        recover_full_embassy(
            ctx.clone(),
            guid.clone(),
            embassy_password,
            recovery_source,
            recovery_password,
        )
        .await?
    } else {
        return Err(Error::new(
            eyre!("Unsupported version of EmbassyOS: {}", recovery_version),
            crate::ErrorKind::VersionIncompatible,
        ));
    };

    Ok(res)
}

fn dir_copy<'a, P0: AsRef<Path> + 'a + Send + Sync, P1: AsRef<Path> + 'a + Send + Sync>(
    src: P0,
    dst: P1,
    ctr: &'a AtomicU64,
) -> BoxFuture<'a, Result<(), Error>> {
    async move {
        let dst_path = dst.as_ref();
        tokio_stream::wrappers::ReadDirStream::new(tokio::fs::read_dir(src.as_ref()).await?)
            .map_err(|e| Error::new(e, crate::ErrorKind::Filesystem))
            .try_for_each(|e| async move {
                let m = e.metadata().await?;
                let src_path = e.path();
                let dst_path = dst_path.join(e.file_name());
                if m.is_file() {
                    tokio::fs::copy(&src_path, &dst_path).await.with_ctx(|_| {
                        (
                            crate::ErrorKind::Filesystem,
                            format!("cp {} -> {}", src_path.display(), dst_path.display()),
                        )
                    })?;
                    ctr.fetch_add(m.len(), Ordering::Relaxed);
                } else if m.is_dir() {
                    tokio::fs::create_dir_all(&dst_path).await.with_ctx(|_| {
                        (
                            crate::ErrorKind::Filesystem,
                            format!("mkdir {}", dst_path.display()),
                        )
                    })?;
                    tokio::fs::set_permissions(&dst_path, m.permissions())
                        .await
                        .with_ctx(|_| {
                            (
                                crate::ErrorKind::Filesystem,
                                format!("chmod {}", dst_path.display()),
                            )
                        })?;
                    dir_copy(src_path, dst_path, ctr).await?;
                } else if m.file_type().is_symlink() {
                    tokio::fs::symlink(
                        tokio::fs::read_link(&src_path).await.with_ctx(|_| {
                            (
                                crate::ErrorKind::Filesystem,
                                format!("readlink {}", src_path.display()),
                            )
                        })?,
                        &dst_path,
                    )
                    .await
                    .with_ctx(|_| {
                        (
                            crate::ErrorKind::Filesystem,
                            format!("cp -P {} -> {}", src_path.display(), dst_path.display()),
                        )
                    })?;
                    // Do not set permissions (see https://unix.stackexchange.com/questions/87200/change-permissions-for-a-symbolic-link)
                }
                Ok(())
            })
            .await?;
        Ok(())
    }
    .boxed()
}

#[instrument(skip(ctx))]
async fn recover_v2(ctx: SetupContext, recovery_source: TmpMountGuard) -> Result<(), Error> {
    let secret_store = ctx.secret_store().await?;
    let db = ctx.db(&secret_store).await?;
    let mut handle = db.handle();

    let apps_yaml_path = recovery_source
        .as_ref()
        .join("root")
        .join("appmgr")
        .join("apps.yaml");
    #[derive(Deserialize)]
    struct LegacyAppInfo {
        title: String,
        version: Version,
    }
    let packages: BTreeMap<PackageId, LegacyAppInfo> =
        from_yaml_async_reader(File::open(&apps_yaml_path).await.with_ctx(|_| {
            (
                crate::ErrorKind::Filesystem,
                apps_yaml_path.display().to_string(),
            )
        })?)
        .await?;

    let volume_path = recovery_source.as_ref().join("root/volumes");
    let mut total_bytes = 0;
    for (pkg_id, _) in &packages {
        let volume_src_path = volume_path.join(&pkg_id);
        total_bytes += dir_size(&volume_src_path).await.with_ctx(|_| {
            (
                crate::ErrorKind::Filesystem,
                volume_src_path.display().to_string(),
            )
        })?;
    }
    *ctx.recovery_status.write().await = Some(Ok(RecoveryStatus {
        bytes_transferred: 0,
        total_bytes,
        complete: false,
    }));
    let bytes_transferred = AtomicU64::new(0);
    let volume_id = VolumeId::Custom(Id::try_from("main".to_owned())?);
    for (pkg_id, info) in packages {
        let volume_src_path = volume_path.join(&pkg_id);
        let volume_dst_path = data_dir(&ctx.datadir, &pkg_id, &volume_id);
        tokio::fs::create_dir_all(&volume_dst_path)
            .await
            .with_ctx(|_| {
                (
                    crate::ErrorKind::Filesystem,
                    volume_dst_path.display().to_string(),
                )
            })?;
        tokio::select!(
            res = dir_copy(
                &volume_src_path,
                &volume_dst_path,
                &bytes_transferred
            ) => res?,
            _ = async {
                loop {
                    tokio::time::sleep(Duration::from_secs(1)).await;
                    *ctx.recovery_status.write().await = Some(Ok(RecoveryStatus {
                        bytes_transferred: bytes_transferred.load(Ordering::Relaxed),
                        total_bytes,
                        complete: false
                    }));
                }
            } => (),
        );
        let tor_src_path = recovery_source
            .as_ref()
            .join("var/lib/tor")
            .join(format!("app-{}", pkg_id))
            .join("hs_ed25519_secret_key");
        let key_vec = tokio::fs::read(&tor_src_path).await.with_ctx(|_| {
            (
                crate::ErrorKind::Filesystem,
                tor_src_path.display().to_string(),
            )
        })?;
        ensure_code!(
            key_vec.len() == 96,
            crate::ErrorKind::Tor,
            "{} not 96 bytes",
            tor_src_path.display()
        );
        let key_vec = key_vec[32..].to_vec();
        sqlx::query!(
            "REPLACE INTO tor (package, interface, key) VALUES (?, 'main', ?)",
            *pkg_id,
            key_vec,
        )
        .execute(&mut secret_store.acquire().await?)
        .await?;
        let icon_leaf = AsRef::<Path>::as_ref(&pkg_id)
            .join(info.version.as_str())
            .join("icon.png");
        let icon_src_path = recovery_source
            .as_ref()
            .join("root/agent/icons")
            .join(format!("{}.png", pkg_id));
        let icon_dst_path = ctx.datadir.join(PKG_PUBLIC_DIR).join(&icon_leaf);
        if let Some(parent) = icon_dst_path.parent() {
            tokio::fs::create_dir_all(&parent)
                .await
                .with_ctx(|_| (crate::ErrorKind::Filesystem, parent.display().to_string()))?;
        }
        tokio::fs::copy(&icon_src_path, &icon_dst_path)
            .await
            .with_ctx(|_| {
                (
                    crate::ErrorKind::Filesystem,
                    format!(
                        "cp {} -> {}",
                        icon_src_path.display(),
                        icon_dst_path.display()
                    ),
                )
            })?;
        let icon_url = Path::new("/public/package-data").join(&icon_leaf);
        crate::db::DatabaseModel::new()
            .recovered_packages()
            .idx_model(&pkg_id)
            .put(
                &mut handle,
                &RecoveredPackageInfo {
                    title: info.title,
                    icon: icon_url.display().to_string(),
                    version: info.version,
                },
            )
            .await?;
    }

    secret_store.close().await;
    recovery_source.unmount().await?;
    Ok(())
}
