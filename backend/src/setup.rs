use std::path::PathBuf;
use std::sync::Arc;

use color_eyre::eyre::eyre;
use futures::future::BoxFuture;
use futures::{StreamExt, TryFutureExt};
use josekit::jwk::Jwk;
use openssl::x509::X509;
use patch_db::DbHandle;
use rpc_toolkit::command;
use rpc_toolkit::yajrc::RpcError;
use serde::{Deserialize, Serialize};
use sqlx::{Connection, Executor, Postgres};
use tokio::fs::File;
use tokio::io::AsyncWriteExt;
use torut::onion::{OnionAddressV3, TorSecretKeyV3};
use tracing::instrument;

use crate::backup::restore::recover_full_embassy;
use crate::backup::target::BackupTargetFS;
use crate::context::rpc::RpcContextConfig;
use crate::context::setup::SetupResult;
use crate::context::SetupContext;
use crate::disk::fsck::RepairStrategy;
use crate::disk::main::DEFAULT_PASSWORD;
use crate::disk::mount::filesystem::cifs::Cifs;
use crate::disk::mount::filesystem::ReadOnly;
use crate::disk::mount::guard::TmpMountGuard;
use crate::disk::util::{pvscan, recovery_info, DiskInfo, EmbassyOsRecoveryInfo};
use crate::disk::REPAIR_DISK_PATH;
use crate::hostname::{get_hostname, HostNameReceipt, Hostname};
use crate::init::init;
use crate::middleware::encrypt::EncryptedWire;
use crate::net::ssl::SslManager;
use crate::sound::BEETHOVEN;
use crate::util::rsync::{Rsync, RsyncOptions};
use crate::{Error, ErrorKind, ResultExt};

#[instrument(skip(secrets))]
pub async fn password_hash<Ex>(secrets: &mut Ex) -> Result<String, Error>
where
    for<'a> &'a mut Ex: Executor<'a, Database = Postgres>,
{
    let password = sqlx::query!("SELECT password FROM account")
        .fetch_one(secrets)
        .await?
        .password;

    Ok(password)
}

#[command(subcommands(status, disk, attach, execute, recovery, cifs, complete, get_pubkey))]
pub fn setup() -> Result<(), Error> {
    Ok(())
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct StatusRes {
    migrating: bool,
}

#[command(rpc_only, metadata(authenticated = false))]
pub async fn status(#[context] ctx: SetupContext) -> Result<StatusRes, Error> {
    Ok(StatusRes {
        migrating: ctx.recovery_status.read().await.is_some(),
    })
}

#[command(subcommands(list_disks))]
pub fn disk() -> Result<(), Error> {
    Ok(())
}

#[command(rename = "list", rpc_only, metadata(authenticated = false))]
pub async fn list_disks(#[context] ctx: SetupContext) -> Result<Vec<DiskInfo>, Error> {
    crate::disk::util::list(&ctx.os_partitions).await
}

async fn setup_init(
    ctx: &SetupContext,
    password: Option<String>,
) -> Result<(Hostname, OnionAddressV3, X509), Error> {
    init(&RpcContextConfig::load(ctx.config_path.clone()).await?).await?;
    let secrets = ctx.secret_store().await?;
    let db = ctx.db(&secrets).await?;
    let mut secrets_handle = secrets.acquire().await?;
    let mut db_handle = db.handle();
    let mut secrets_tx = secrets_handle.begin().await?;
    let mut db_tx = db_handle.begin().await?;

    if let Some(password) = password {
        let set_password_receipt = crate::auth::SetPasswordReceipt::new(&mut db_tx).await?;
        crate::auth::set_password(
            &mut db_tx,
            &set_password_receipt,
            &mut secrets_tx,
            &password,
        )
        .await?;
    }

    let tor_key = crate::net::tor::os_key(&mut secrets_tx).await?;

    db_tx.commit().await?;
    secrets_tx.commit().await?;

    let hostname_receipts = HostNameReceipt::new(&mut db_handle).await?;
    let hostname = get_hostname(&mut db_handle, &hostname_receipts).await?;

    let (_, root_ca) = SslManager::init(secrets, &mut db_handle)
        .await?
        .export_root_ca()
        .await?;
    Ok((hostname, tor_key.public().get_onion_address(), root_ca))
}

#[command(rpc_only)]
pub async fn attach(
    #[context] ctx: SetupContext,
    #[arg] guid: Arc<String>,
    #[arg(rename = "embassy-password")] password: Option<EncryptedWire>,
) -> Result<SetupResult, Error> {
    let password: Option<String> = match password {
        Some(a) => match a.decrypt(&*ctx) {
            a @ Some(_) => a,
            None => {
                return Err(Error::new(
                    color_eyre::eyre::eyre!("Couldn't decode password"),
                    crate::ErrorKind::Unknown,
                ));
            }
        },
        None => None,
    };
    let requires_reboot = crate::disk::main::import(
        &*guid,
        &ctx.datadir,
        if tokio::fs::metadata(REPAIR_DISK_PATH).await.is_ok() {
            RepairStrategy::Aggressive
        } else {
            RepairStrategy::Preen
        },
        DEFAULT_PASSWORD,
    )
    .await?;
    if tokio::fs::metadata(REPAIR_DISK_PATH).await.is_ok() {
        tokio::fs::remove_file(REPAIR_DISK_PATH)
            .await
            .with_ctx(|_| (ErrorKind::Filesystem, REPAIR_DISK_PATH))?;
    }
    if requires_reboot.0 {
        crate::disk::main::export(&*guid, &ctx.datadir).await?;
        return Err(Error::new(
            eyre!(
                "Errors were corrected with your disk, but the Embassy must be restarted in order to proceed"
            ),
            ErrorKind::DiskManagement,
        ));
    }
    let (hostname, tor_addr, root_ca) = setup_init(&ctx, password).await?;
    let setup_result = SetupResult {
        tor_address: format!("http://{}", tor_addr),
        lan_address: hostname.lan_address(),
        root_ca: String::from_utf8(root_ca.to_pem()?)?,
    };
    *ctx.setup_result.write().await = Some((guid, setup_result.clone()));
    Ok(setup_result)
}

#[command(subcommands(recovery_status))]
pub fn recovery() -> Result<(), Error> {
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

/// We want to be able to get a secret, a shared private key with the frontend
/// This way the frontend can send a secret, like the password for the setup/ recovory
/// without knowing the password over clearnet. We use the public key shared across the network
/// since it is fine to share the public, and encrypt against the public.
#[command(rename = "get-pubkey", rpc_only, metadata(authenticated = false))]
pub async fn get_pubkey(#[context] ctx: SetupContext) -> Result<Jwk, RpcError> {
    let secret = ctx.current_secret.clone();
    let pub_key = secret.to_public_key()?;
    Ok(pub_key)
}

#[command(subcommands(verify_cifs))]
pub fn cifs() -> Result<(), Error> {
    Ok(())
}

#[command(rename = "verify", rpc_only)]
pub async fn verify_cifs(
    #[context] ctx: SetupContext,
    #[arg] hostname: String,
    #[arg] path: PathBuf,
    #[arg] username: String,
    #[arg] password: Option<EncryptedWire>,
) -> Result<EmbassyOsRecoveryInfo, Error> {
    let password: Option<String> = password.map(|x| x.decrypt(&*ctx)).flatten();
    let guard = TmpMountGuard::mount(
        &Cifs {
            hostname,
            path,
            username,
            password,
        },
        ReadOnly,
    )
    .await?;
    let embassy_os = recovery_info(&guard).await?;
    guard.unmount().await?;
    embassy_os.ok_or_else(|| Error::new(eyre!("No Backup Found"), crate::ErrorKind::NotFound))
}

#[derive(Debug, Deserialize)]
#[serde(tag = "type")]
#[serde(rename_all = "kebab-case")]
pub enum RecoverySource {
    Migrate { guid: String },
    Backup { target: BackupTargetFS },
}

#[command(rpc_only)]
pub async fn execute(
    #[context] ctx: SetupContext,
    #[arg(rename = "embassy-logicalname")] embassy_logicalname: PathBuf,
    #[arg(rename = "embassy-password")] embassy_password: EncryptedWire,
    #[arg(rename = "recovery-source")] recovery_source: Option<RecoverySource>,
    #[arg(rename = "recovery-password")] recovery_password: Option<EncryptedWire>,
) -> Result<SetupResult, Error> {
    let embassy_password = match embassy_password.decrypt(&*ctx) {
        Some(a) => a,
        None => {
            return Err(Error::new(
                color_eyre::eyre::eyre!("Couldn't decode embassy-password"),
                crate::ErrorKind::Unknown,
            ))
        }
    };
    let recovery_password: Option<String> = match recovery_password {
        Some(a) => match a.decrypt(&*ctx) {
            Some(a) => Some(a),
            None => {
                return Err(Error::new(
                    color_eyre::eyre::eyre!("Couldn't decode recovery-password"),
                    crate::ErrorKind::Unknown,
                ))
            }
        },
        None => None,
    };
    match execute_inner(
        ctx.clone(),
        embassy_logicalname,
        embassy_password,
        recovery_source,
        recovery_password,
    )
    .await
    {
        Ok((hostname, tor_addr, root_ca)) => {
            tracing::info!("Setup Successful! Tor Address: {}", tor_addr);
            Ok(SetupResult {
                tor_address: format!("http://{}", tor_addr),
                lan_address: hostname.lan_address(),
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
pub async fn complete(#[context] ctx: SetupContext) -> Result<SetupResult, Error> {
    let (guid, setup_result) = if let Some((guid, setup_result)) = &*ctx.setup_result.read().await {
        (guid.clone(), setup_result.clone())
    } else {
        return Err(Error::new(
            eyre!("setup.execute has not completed successfully"),
            crate::ErrorKind::InvalidRequest,
        ));
    };
    let secrets = ctx.secret_store().await?;
    let mut db = ctx.db(&secrets).await?.handle();
    let receipts = crate::hostname::HostNameReceipt::new(&mut db).await?;
    let hostname = crate::hostname::get_hostname(&mut db, &receipts).await?;
    let si = crate::db::DatabaseModel::new().server_info();
    let id = crate::hostname::get_id(&mut db, &receipts).await?;
    si.clone().id().put(&mut db, &id).await?;
    si.lan_address()
        .put(&mut db, &hostname.lan_address().parse().unwrap())
        .await?;
    let mut guid_file = File::create("/media/embassy/config/disk.guid").await?;
    guid_file.write_all(guid.as_bytes()).await?;
    guid_file.sync_all().await?;
    ctx.shutdown.send(()).expect("failed to shutdown");
    Ok(setup_result)
}

#[instrument(skip(ctx, embassy_password, recovery_password))]
pub async fn execute_inner(
    ctx: SetupContext,
    embassy_logicalname: PathBuf,
    embassy_password: String,
    recovery_source: Option<RecoverySource>,
    recovery_password: Option<String>,
) -> Result<(Hostname, OnionAddressV3, X509), Error> {
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
    let _ = crate::disk::main::import(
        &*guid,
        &ctx.datadir,
        RepairStrategy::Preen,
        DEFAULT_PASSWORD,
    )
    .await?;

    let res = if let Some(RecoverySource::Backup { target }) = recovery_source {
        let (tor_addr, root_ca, recover_fut) = recover(
            ctx.clone(),
            guid.clone(),
            embassy_password,
            target,
            recovery_password,
        )
        .await?;
        let db = init(&RpcContextConfig::load(ctx.config_path.clone()).await?)
            .await?
            .db;
        let hostname = {
            let mut handle = db.handle();
            let receipts = crate::hostname::HostNameReceipt::new(&mut handle).await?;
            get_hostname(&mut handle, &receipts).await?
        };
        let res = (hostname.clone(), tor_addr, root_ca.clone());
        tokio::spawn(async move {
            if let Err(e) = recover_fut
                .and_then(|_| async {
                    *ctx.setup_result.write().await = Some((
                        guid,
                        SetupResult {
                            tor_address: format!("http://{}", tor_addr),
                            lan_address: hostname.lan_address(),
                            root_ca: String::from_utf8(root_ca.to_pem()?)?,
                        },
                    ));
                    if let Some(Ok(recovery_status)) = &mut *ctx.recovery_status.write().await {
                        recovery_status.complete = true;
                    }
                    Ok(())
                })
                .await
            {
                (&BEETHOVEN).play().await.unwrap_or_default(); // ignore error in playing the song
                tracing::error!("Error recovering drive!: {}", e);
                tracing::debug!("{:?}", e);
                *ctx.recovery_status.write().await = Some(Err(e.into()));
            } else {
                tracing::info!("Recovery Complete!");
            }
        });
        res
    } else if let Some(RecoverySource::Migrate { guid: old_guid }) = recovery_source {
        let _ = crate::disk::main::mount_fs(
            &old_guid,
            "/media/embassy/migrate",
            "main",
            RepairStrategy::Preen,
            DEFAULT_PASSWORD,
        )
        .await?;
        Rsync::new(
            "/media/embassy/migrate/main",
            "/embassy-data/main",
            RsyncOptions {
                delete: true,
                force: true,
                ignore_existing: false,
            },
        )?
        .wait()
        .await?;
        let _ = crate::disk::main::mount_fs(
            &old_guid,
            "/media/embassy/migrate",
            "package-data",
            RepairStrategy::Preen,
            DEFAULT_PASSWORD,
        )
        .await?;
        let mut package_data_transfer = Rsync::new(
            "/media/embassy/migrate/package-data",
            "/embassy-data/package-data",
            RsyncOptions {
                delete: true,
                force: true,
                ignore_existing: false,
            },
        )?;
        *ctx.recovery_status.write().await = Some(Ok(RecoveryStatus {
            bytes_transferred: 0,
            total_bytes: 100,
            complete: false,
        }));
        let (hostname, tor_addr, root_ca) = setup_init(&ctx, Some(embassy_password)).await?;
        let res = (hostname.clone(), tor_addr.clone(), root_ca.clone());
        tokio::spawn(async move {
            if let Err(e) = async {
                while let Some(progress) = package_data_transfer.progress.next().await {
                    *ctx.recovery_status.write().await = Some(Ok(RecoveryStatus {
                        bytes_transferred: (progress * 100.0) as u64,
                        total_bytes: 100,
                        complete: false,
                    }));
                }
                package_data_transfer.wait().await?;
                Ok::<_, Error>(())
            }
            .and_then(|_| async {
                *ctx.setup_result.write().await = Some((
                    guid,
                    SetupResult {
                        tor_address: format!("http://{}", tor_addr),
                        lan_address: hostname.lan_address(),
                        root_ca: String::from_utf8(root_ca.to_pem()?)?,
                    },
                ));
                if let Some(Ok(recovery_status)) = &mut *ctx.recovery_status.write().await {
                    recovery_status.complete = true;
                }
                Ok(())
            })
            .await
            {
                (&BEETHOVEN).play().await.unwrap_or_default(); // ignore error in playing the song
                tracing::error!("Error recovering drive!: {}", e);
                tracing::debug!("{:?}", e);
                *ctx.recovery_status.write().await = Some(Err(e.into()));
            } else {
                tracing::info!("Recovery Complete!");
            }
        });
        res
    } else {
        let (tor_addr, root_ca) = fresh_setup(&ctx, &embassy_password).await?;
        let db = init(&RpcContextConfig::load(ctx.config_path.clone()).await?)
            .await?
            .db;
        let mut handle = db.handle();
        let receipts = crate::hostname::HostNameReceipt::new(&mut handle).await?;
        *ctx.setup_result.write().await = Some((
            guid,
            SetupResult {
                tor_address: format!("http://{}", tor_addr),
                lan_address: get_hostname(&mut handle, &receipts).await?.lan_address(),
                root_ca: String::from_utf8(root_ca.to_pem()?)?,
            },
        ));
        let hostname = get_hostname(&mut handle, &receipts).await?;
        (hostname, tor_addr, root_ca)
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
        "INSERT INTO account (id, password, tor_key) VALUES ($1, $2, $3) ON CONFLICT (id) DO UPDATE SET password = $2, tor_key = $3",
        0,
        password,
        key_vec,
    )
    .execute(&mut sqlite_pool.acquire().await?)
    .await?;
    let db = ctx.db(&sqlite_pool).await?;
    let (_, root_ca) = SslManager::init(sqlite_pool.clone(), &mut db.handle())
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
    let recovery_source = TmpMountGuard::mount(&recovery_source, ReadOnly).await?;
    recover_full_embassy(
        ctx.clone(),
        guid.clone(),
        embassy_password,
        recovery_source,
        recovery_password,
    )
    .await?;

    // migrate the tor address
    let tor_key_path = recovery_source
        .as_ref()
        .join("var")
        .join("lib")
        .join("tor")
        .join("agent")
        .join("hs_ed25519_secret_key");
    let tor_key_bytes = tokio::fs::read(tor_key_path).await?;
    let mut tor_key_array_tmp = [0u8; 64];
    tor_key_array_tmp.clone_from_slice(&tor_key_bytes[32..]);
    let tor_key: TorSecretKeyV3 = tor_key_array_tmp.into();
    let key_vec = tor_key.as_bytes().to_vec();
    let password = argon2::hash_encoded(
        embassy_password.as_bytes(),
        &rand::random::<[u8; 16]>()[..],
        &argon2::Config::default(),
    )
    .with_kind(crate::ErrorKind::PasswordHashGeneration)?;
    let sqlite_pool = ctx.secret_store().await?;
    sqlx::query!(
        "INSERT INTO account (id, password, tor_key) VALUES ($1, $2, $3) ON CONFLICT (id) DO UPDATE SET password = $2, tor_key = $3",
        0,
        password,
        key_vec
    )
    .execute(&mut sqlite_pool.acquire().await?)
    .await?;

    // rest of migration as future
    let fut = async move {
        let db = ctx.db(&secret_store).await?;
        let mut handle = db.handle();
        // lock everything to avoid issues with renamed packages (bitwarden)
        crate::db::DatabaseModel::new()
            .lock(&mut handle, LockType::Write)
            .await?;

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
            let (src_id, dst_id) = rename_pkg_id(pkg_id);
            let volume_src_path = volume_path.join(&src_id);
            let volume_dst_path = data_dir(&ctx.datadir, &dst_id, &volume_id);
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
                .join(format!("app-{}", src_id))
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
                "INSERT INTO tor (package, interface, key) VALUES ($1, 'main', $2) ON CONFLICT (package, interface) DO UPDATE SET key = $2",
                *dst_id,
                key_vec,
            )
            .execute(&mut secret_store.acquire().await?)
            .await?;
            let icon_leaf = AsRef::<Path>::as_ref(&dst_id)
                .join(info.version.as_str())
                .join("icon.png");
            let icon_src_path = recovery_source
                .as_ref()
                .join("root/agent/icons")
                .join(format!("{}.png", src_id));
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
        }

        secret_store.close().await;
        recovery_source.unmount().await?;
        Ok(())
    };
    Ok((
        tor_key.public().get_onion_address(),
        root_ca_cert,
        fut.boxed(),
    ))
}

fn rename_pkg_id(src_pkg_id: PackageId) -> (PackageId, PackageId) {
    if &*src_pkg_id == "bitwarden" {
        (src_pkg_id, "vaultwarden".parse().unwrap())
    } else {
        (src_pkg_id.clone(), src_pkg_id)
    }
}
