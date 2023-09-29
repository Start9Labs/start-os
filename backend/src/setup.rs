use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;

use color_eyre::eyre::eyre;
use josekit::jwk::Jwk;
use openssl::x509::X509;
use rpc_toolkit::command;
use rpc_toolkit::yajrc::RpcError;
use serde::{Deserialize, Serialize};
use sqlx::Connection;
use tokio::fs::File;
use tokio::io::AsyncWriteExt;
use tokio::try_join;
use torut::onion::OnionAddressV3;
use tracing::instrument;

use crate::account::AccountInfo;
use crate::backup::restore::recover_full_embassy;
use crate::backup::target::BackupTargetFS;
use crate::context::rpc::RpcContextConfig;
use crate::context::setup::SetupResult;
use crate::context::SetupContext;
use crate::disk::fsck::RepairStrategy;
use crate::disk::main::DEFAULT_PASSWORD;
use crate::disk::mount::filesystem::cifs::Cifs;
use crate::disk::mount::filesystem::ReadWrite;
use crate::disk::mount::guard::TmpMountGuard;
use crate::disk::util::{pvscan, recovery_info, DiskInfo, EmbassyOsRecoveryInfo};
use crate::disk::REPAIR_DISK_PATH;
use crate::hostname::Hostname;
use crate::init::{init, InitResult};
use crate::middleware::encrypt::EncryptedWire;
use crate::prelude::*;
use crate::util::io::{dir_copy, dir_size, Counter};
use crate::{Error, ErrorKind, ResultExt};

#[command(subcommands(status, disk, attach, execute, cifs, complete, get_pubkey, exit))]
pub fn setup() -> Result<(), Error> {
    Ok(())
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
    let InitResult { secret_store, db } =
        init(&RpcContextConfig::load(ctx.config_path.clone()).await?).await?;
    let mut secrets_handle = secret_store.acquire().await?;
    let mut secrets_tx = secrets_handle.begin().await?;

    let mut account = AccountInfo::load(&mut secrets_tx).await?;

    if let Some(password) = password {
        account.set_password(&password)?;
        account.save(&mut secrets_tx).await?;
        db.mutate(|m| {
            m.as_server_info_mut()
                .as_password_hash_mut()
                .ser(&account.password)
        })
        .await?;
    }

    secrets_tx.commit().await?;

    Ok((
        account.hostname,
        account.key.tor_address(),
        account.root_ca_cert,
    ))
}

#[command(rpc_only)]
pub async fn attach(
    #[context] ctx: SetupContext,
    #[arg] guid: Arc<String>,
    #[arg(rename = "embassy-password")] password: Option<EncryptedWire>,
) -> Result<(), Error> {
    let mut status = ctx.setup_status.write().await;
    if status.is_some() {
        return Err(Error::new(
            eyre!("Setup already in progress"),
            ErrorKind::InvalidRequest,
        ));
    }
    *status = Some(Ok(SetupStatus {
        bytes_transferred: 0,
        total_bytes: None,
        complete: false,
    }));
    drop(status);
    tokio::task::spawn(async move {
        if let Err(e) = async {
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
                if guid.ends_with("_UNENC") { None } else { Some(DEFAULT_PASSWORD) },
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
                        "Errors were corrected with your disk, but the server must be restarted in order to proceed"
                    ),
                    ErrorKind::DiskManagement,
                ));
            }
            let (hostname, tor_addr, root_ca) = setup_init(&ctx, password).await?;
            *ctx.setup_result.write().await = Some((guid, SetupResult {
                tor_address: format!("https://{}", tor_addr),
                lan_address: hostname.lan_address(),
                root_ca: String::from_utf8(root_ca.to_pem()?)?,
            }));
            *ctx.setup_status.write().await = Some(Ok(SetupStatus {
                bytes_transferred: 0,
                total_bytes: None,
                complete: true,
            }));
            Ok(())
        }.await {
            tracing::error!("Error Setting Up Embassy: {}", e);
            tracing::debug!("{:?}", e);
            *ctx.setup_status.write().await = Some(Err(e.into()));
        }
    });
    Ok(())
}

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct SetupStatus {
    pub bytes_transferred: u64,
    pub total_bytes: Option<u64>,
    pub complete: bool,
}

#[command(rpc_only, metadata(authenticated = false))]
pub async fn status(#[context] ctx: SetupContext) -> Result<Option<SetupStatus>, RpcError> {
    ctx.setup_status.read().await.clone().transpose()
}

/// We want to be able to get a secret, a shared private key with the frontend
/// This way the frontend can send a secret, like the password for the setup/ recovory
/// without knowing the password over clearnet. We use the public key shared across the network
/// since it is fine to share the public, and encrypt against the public.
#[command(rename = "get-pubkey", rpc_only, metadata(authenticated = false))]
pub async fn get_pubkey(#[context] ctx: SetupContext) -> Result<Jwk, RpcError> {
    let secret = ctx.as_ref().clone();
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
        ReadWrite,
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
) -> Result<(), Error> {
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
    let mut status = ctx.setup_status.write().await;
    if status.is_some() {
        return Err(Error::new(
            eyre!("Setup already in progress"),
            ErrorKind::InvalidRequest,
        ));
    }
    *status = Some(Ok(SetupStatus {
        bytes_transferred: 0,
        total_bytes: None,
        complete: false,
    }));
    drop(status);
    tokio::task::spawn({
        async move {
            let ctx = ctx.clone();
            let recovery_source = recovery_source;

            let embassy_password = embassy_password;
            let recovery_source = recovery_source;
            let recovery_password = recovery_password;
            match execute_inner(
                ctx.clone(),
                embassy_logicalname,
                embassy_password,
                recovery_source,
                recovery_password,
            )
            .await
            {
                Ok((guid, hostname, tor_addr, root_ca)) => {
                    tracing::info!("Setup Complete!");
                    *ctx.setup_result.write().await = Some((
                        guid,
                        SetupResult {
                            tor_address: format!("https://{}", tor_addr),
                            lan_address: hostname.lan_address(),
                            root_ca: String::from_utf8(
                                root_ca.to_pem().expect("failed to serialize root ca"),
                            )
                            .expect("invalid pem string"),
                        },
                    ));
                    *ctx.setup_status.write().await = Some(Ok(SetupStatus {
                        bytes_transferred: 0,
                        total_bytes: None,
                        complete: true,
                    }));
                }
                Err(e) => {
                    tracing::error!("Error Setting Up Server: {}", e);
                    tracing::debug!("{:?}", e);
                    *ctx.setup_status.write().await = Some(Err(e.into()));
                }
            }
        }
    });
    Ok(())
}

#[instrument(skip_all)]
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
    let mut guid_file = File::create("/media/embassy/config/disk.guid").await?;
    guid_file.write_all(guid.as_bytes()).await?;
    guid_file.sync_all().await?;
    Ok(setup_result)
}

#[instrument(skip_all)]
#[command(rpc_only)]
pub async fn exit(#[context] ctx: SetupContext) -> Result<(), Error> {
    ctx.shutdown.send(()).expect("failed to shutdown");
    Ok(())
}

#[instrument(skip_all)]
pub async fn execute_inner(
    ctx: SetupContext,
    embassy_logicalname: PathBuf,
    embassy_password: String,
    recovery_source: Option<RecoverySource>,
    recovery_password: Option<String>,
) -> Result<(Arc<String>, Hostname, OnionAddressV3, X509), Error> {
    let encryption_password = if ctx.disable_encryption {
        None
    } else {
        Some(DEFAULT_PASSWORD)
    };
    let guid = Arc::new(
        crate::disk::main::create(
            &[embassy_logicalname],
            &pvscan().await?,
            &ctx.datadir,
            encryption_password,
        )
        .await?,
    );
    let _ = crate::disk::main::import(
        &*guid,
        &ctx.datadir,
        RepairStrategy::Preen,
        encryption_password,
    )
    .await?;

    if let Some(RecoverySource::Backup { target }) = recovery_source {
        recover(ctx, guid, embassy_password, target, recovery_password).await
    } else if let Some(RecoverySource::Migrate { guid: old_guid }) = recovery_source {
        migrate(ctx, guid, &old_guid, embassy_password).await
    } else {
        let (hostname, tor_addr, root_ca) = fresh_setup(&ctx, &embassy_password).await?;
        Ok((guid, hostname, tor_addr, root_ca))
    }
}

async fn fresh_setup(
    ctx: &SetupContext,
    embassy_password: &str,
) -> Result<(Hostname, OnionAddressV3, X509), Error> {
    let account = AccountInfo::new(embassy_password)?;
    let sqlite_pool = ctx.secret_store().await?;
    account.save(&sqlite_pool).await?;
    sqlite_pool.close().await;
    let InitResult { secret_store, .. } =
        init(&RpcContextConfig::load(ctx.config_path.clone()).await?).await?;
    secret_store.close().await;
    Ok((
        account.hostname.clone(),
        account.key.tor_address(),
        account.root_ca_cert.clone(),
    ))
}

#[instrument(skip_all)]
async fn recover(
    ctx: SetupContext,
    guid: Arc<String>,
    embassy_password: String,
    recovery_source: BackupTargetFS,
    recovery_password: Option<String>,
) -> Result<(Arc<String>, Hostname, OnionAddressV3, X509), Error> {
    let recovery_source = TmpMountGuard::mount(&recovery_source, ReadWrite).await?;
    recover_full_embassy(
        ctx,
        guid.clone(),
        embassy_password,
        recovery_source,
        recovery_password,
    )
    .await
}

#[instrument(skip_all)]
async fn migrate(
    ctx: SetupContext,
    guid: Arc<String>,
    old_guid: &str,
    embassy_password: String,
) -> Result<(Arc<String>, Hostname, OnionAddressV3, X509), Error> {
    *ctx.setup_status.write().await = Some(Ok(SetupStatus {
        bytes_transferred: 0,
        total_bytes: None,
        complete: false,
    }));

    let _ = crate::disk::main::import(
        &old_guid,
        "/media/embassy/migrate",
        RepairStrategy::Preen,
        if guid.ends_with("_UNENC") {
            None
        } else {
            Some(DEFAULT_PASSWORD)
        },
    )
    .await?;

    let main_transfer_args = ("/media/embassy/migrate/main/", "/embassy-data/main/");
    let package_data_transfer_args = (
        "/media/embassy/migrate/package-data/",
        "/embassy-data/package-data/",
    );

    let tmpdir = Path::new(package_data_transfer_args.0).join("tmp");
    if tokio::fs::metadata(&tmpdir).await.is_ok() {
        tokio::fs::remove_dir_all(&tmpdir).await?;
    }

    let ordering = std::sync::atomic::Ordering::Relaxed;

    let main_transfer_size = Counter::new(0, ordering);
    let package_data_transfer_size = Counter::new(0, ordering);

    let size = tokio::select! {
        res = async {
            let (main_size, package_data_size) = try_join!(
                dir_size(main_transfer_args.0, Some(&main_transfer_size)),
                dir_size(package_data_transfer_args.0, Some(&package_data_transfer_size))
            )?;
            Ok::<_, Error>(main_size + package_data_size)
        } => { res? },
        res = async {
            loop {
                tokio::time::sleep(Duration::from_secs(1)).await;
                *ctx.setup_status.write().await = Some(Ok(SetupStatus {
                    bytes_transferred: 0,
                    total_bytes: Some(main_transfer_size.load() + package_data_transfer_size.load()),
                    complete: false,
                }));
            }
        } => res,
    };

    *ctx.setup_status.write().await = Some(Ok(SetupStatus {
        bytes_transferred: 0,
        total_bytes: Some(size),
        complete: false,
    }));

    let main_transfer_progress = Counter::new(0, ordering);
    let package_data_transfer_progress = Counter::new(0, ordering);

    tokio::select! {
        res = async {
            try_join!(
                dir_copy(main_transfer_args.0, main_transfer_args.1, Some(&main_transfer_progress)),
                dir_copy(package_data_transfer_args.0, package_data_transfer_args.1, Some(&package_data_transfer_progress))
            )?;
            Ok::<_, Error>(())
        } => { res? },
        res = async {
            loop {
                tokio::time::sleep(Duration::from_secs(1)).await;
                *ctx.setup_status.write().await = Some(Ok(SetupStatus {
                    bytes_transferred: main_transfer_progress.load() + package_data_transfer_progress.load(),
                    total_bytes: Some(size),
                    complete: false,
                }));
            }
        } => res,
    }

    let (hostname, tor_addr, root_ca) = setup_init(&ctx, Some(embassy_password)).await?;

    crate::disk::main::export(&old_guid, "/media/embassy/migrate").await?;

    Ok((guid, hostname, tor_addr, root_ca))
}
