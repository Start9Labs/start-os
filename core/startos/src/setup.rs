use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;

use color_eyre::eyre::eyre;
use const_format::formatcp;
use josekit::jwk::Jwk;
use patch_db::json_ptr::ROOT;
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{from_fn_async, Context, Empty, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use tokio::io::AsyncWriteExt;
use tokio::process::Command;
use tokio::try_join;
use tracing::instrument;
use ts_rs::TS;

use crate::account::AccountInfo;
use crate::auth::write_shadow;
use crate::backup::restore::recover_full_embassy;
use crate::backup::target::BackupTargetFS;
use crate::context::rpc::InitRpcContextPhases;
use crate::context::setup::SetupResult;
use crate::context::{RpcContext, SetupContext};
use crate::db::model::Database;
use crate::disk::fsck::RepairStrategy;
use crate::disk::main::DEFAULT_PASSWORD;
use crate::disk::mount::filesystem::cifs::Cifs;
use crate::disk::mount::filesystem::ReadWrite;
use crate::disk::mount::guard::{GenericMountGuard, TmpMountGuard};
use crate::disk::util::{pvscan, recovery_info, DiskInfo, StartOsRecoveryInfo};
use crate::disk::REPAIR_DISK_PATH;
use crate::init::{init, InitPhases, InitResult};
use crate::net::ssl::root_ca_start_time;
use crate::prelude::*;
use crate::progress::{FullProgress, PhaseProgressTrackerHandle, ProgressUnits};
use crate::rpc_continuations::Guid;
use crate::system::sync_kiosk;
use crate::util::crypto::EncryptedWire;
use crate::util::io::{create_file, dir_copy, dir_size, Counter};
use crate::util::Invoke;
use crate::{Error, ErrorKind, ResultExt, DATA_DIR, MAIN_DATA, PACKAGE_DATA, PLATFORM};

pub fn setup<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "status",
            from_fn_async(status)
                .with_metadata("authenticated", Value::Bool(false))
                .no_cli(),
        )
        .subcommand("disk", disk::<C>())
        .subcommand("attach", from_fn_async(attach).no_cli())
        .subcommand("execute", from_fn_async(execute).no_cli())
        .subcommand("cifs", cifs::<C>())
        .subcommand("complete", from_fn_async(complete).no_cli())
        .subcommand(
            "get-pubkey",
            from_fn_async(get_pubkey)
                .with_metadata("authenticated", Value::Bool(false))
                .no_cli(),
        )
        .subcommand("exit", from_fn_async(exit).no_cli())
        .subcommand("logs", crate::system::logs::<SetupContext>())
        .subcommand(
            "logs",
            from_fn_async(crate::logs::cli_logs::<SetupContext, Empty>).no_display(),
        )
}

pub fn disk<C: Context>() -> ParentHandler<C> {
    ParentHandler::new().subcommand(
        "list",
        from_fn_async(list_disks)
            .with_metadata("authenticated", Value::Bool(false))
            .no_cli(),
    )
}

pub async fn list_disks(ctx: SetupContext) -> Result<Vec<DiskInfo>, Error> {
    crate::disk::util::list(&ctx.os_partitions).await
}

async fn setup_init(
    ctx: &SetupContext,
    password: Option<String>,
    kiosk: Option<bool>,
    init_phases: InitPhases,
) -> Result<(AccountInfo, InitResult), Error> {
    let init_result = init(&ctx.webserver, &ctx.config, init_phases).await?;

    let account = init_result
        .net_ctrl
        .db
        .mutate(|m| {
            let mut account = AccountInfo::load(m)?;
            if let Some(password) = &password {
                account.set_password(password)?;
            }
            account.save(m)?;
            let info = m.as_public_mut().as_server_info_mut();
            info.as_password_hash_mut().ser(&account.password)?;
            if let Some(kiosk) = kiosk {
                info.as_kiosk_mut().ser(&Some(kiosk))?;
            }

            Ok(account)
        })
        .await
        .result?;

    sync_kiosk(kiosk).await?;

    if let Some(password) = &password {
        write_shadow(&password).await?;
    }

    Ok((account, init_result))
}

#[derive(Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AttachParams {
    #[serde(rename = "startOsPassword")]
    password: Option<EncryptedWire>,
    guid: Arc<String>,
    #[ts(optional)]
    kiosk: Option<bool>,
}

pub async fn attach(
    ctx: SetupContext,
    AttachParams {
        password,
        guid: disk_guid,
        kiosk,
    }: AttachParams,
) -> Result<SetupProgress, Error> {
    let setup_ctx = ctx.clone();
    ctx.run_setup(move || async move {
            let progress = &setup_ctx.progress;
            let mut disk_phase = progress.add_phase("Opening data drive".into(), Some(10));
            let init_phases = InitPhases::new(&progress);
            let rpc_ctx_phases = InitRpcContextPhases::new(&progress);

            let password: Option<String> = match password {
                Some(a) => match a.decrypt(&setup_ctx) {
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

            disk_phase.start();
            let requires_reboot = crate::disk::main::import(
                &*disk_guid,
                DATA_DIR,
                if tokio::fs::metadata(REPAIR_DISK_PATH).await.is_ok() {
                    RepairStrategy::Aggressive
                } else {
                    RepairStrategy::Preen
                },
                if disk_guid.ends_with("_UNENC") { None } else { Some(DEFAULT_PASSWORD) },
            )
            .await?;
            if tokio::fs::metadata(REPAIR_DISK_PATH).await.is_ok() {
                tokio::fs::remove_file(REPAIR_DISK_PATH)
                    .await
                    .with_ctx(|_| (ErrorKind::Filesystem, REPAIR_DISK_PATH))?;
            }
            if requires_reboot.0 {
                crate::disk::main::export(&*disk_guid, DATA_DIR).await?;
                return Err(Error::new(
                    eyre!(
                        "Errors were corrected with your disk, but the server must be restarted in order to proceed"
                    ),
                    ErrorKind::DiskManagement,
                ));
            }
            disk_phase.complete();

            let (account, net_ctrl) = setup_init(&setup_ctx, password, kiosk, init_phases).await?;

            let rpc_ctx = RpcContext::init(&setup_ctx.webserver, &setup_ctx.config, disk_guid, Some(net_ctrl), rpc_ctx_phases).await?;

            Ok(((&account).try_into()?, rpc_ctx))
        })?;

    Ok(ctx.progress().await)
}

#[derive(Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
#[serde(tag = "status")]
pub enum SetupStatusRes {
    Complete(SetupResult),
    Running(SetupProgress),
}

#[derive(Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SetupProgress {
    pub progress: FullProgress,
    pub guid: Guid,
}

pub async fn status(ctx: SetupContext) -> Result<Option<SetupStatusRes>, Error> {
    if let Some(res) = ctx.result.get() {
        match res {
            Ok((res, _)) => Ok(Some(SetupStatusRes::Complete(res.clone()))),
            Err(e) => Err(e.clone_output()),
        }
    } else {
        if ctx.task.initialized() {
            Ok(Some(SetupStatusRes::Running(ctx.progress().await)))
        } else {
            Ok(None)
        }
    }
}

/// We want to be able to get a secret, a shared private key with the frontend
/// This way the frontend can send a secret, like the password for the setup/ recovory
/// without knowing the password over clearnet. We use the public key shared across the network
/// since it is fine to share the public, and encrypt against the public.
pub async fn get_pubkey(ctx: SetupContext) -> Result<Jwk, RpcError> {
    let secret = AsRef::<Jwk>::as_ref(&ctx).clone();
    let pub_key = secret.to_public_key()?;
    Ok(pub_key)
}

pub fn cifs<C: Context>() -> ParentHandler<C> {
    ParentHandler::new().subcommand("verify", from_fn_async(verify_cifs).no_cli())
}

#[derive(Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct VerifyCifsParams {
    hostname: String,
    path: PathBuf,
    username: String,
    password: Option<EncryptedWire>,
}

// #[command(rename = "verify", rpc_only)]
pub async fn verify_cifs(
    ctx: SetupContext,
    VerifyCifsParams {
        hostname,
        path,
        username,
        password,
    }: VerifyCifsParams,
) -> Result<BTreeMap<String, StartOsRecoveryInfo>, Error> {
    let password: Option<String> = password.map(|x| x.decrypt(&ctx)).flatten();
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
    let start_os = recovery_info(guard.path()).await?;
    guard.unmount().await?;
    if start_os.is_empty() {
        return Err(Error::new(
            eyre!("No Backup Found"),
            crate::ErrorKind::NotFound,
        ));
    }
    Ok(start_os)
}

#[derive(Debug, Deserialize, Serialize, TS)]
#[serde(tag = "type")]
#[serde(rename_all = "camelCase")]
#[serde(rename_all_fields = "camelCase")]
pub enum RecoverySource<Password> {
    Migrate {
        guid: String,
    },
    Backup {
        target: BackupTargetFS,
        password: Password,
        server_id: String,
    },
}

#[derive(Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SetupExecuteParams {
    start_os_logicalname: PathBuf,
    start_os_password: EncryptedWire,
    recovery_source: Option<RecoverySource<EncryptedWire>>,
    #[ts(optional)]
    kiosk: Option<bool>,
}

// #[command(rpc_only)]
pub async fn execute(
    ctx: SetupContext,
    SetupExecuteParams {
        start_os_logicalname,
        start_os_password,
        recovery_source,
        kiosk,
    }: SetupExecuteParams,
) -> Result<SetupProgress, Error> {
    let start_os_password = match start_os_password.decrypt(&ctx) {
        Some(a) => a,
        None => {
            return Err(Error::new(
                color_eyre::eyre::eyre!("Couldn't decode startOsPassword"),
                crate::ErrorKind::Unknown,
            ))
        }
    };
    let recovery = match recovery_source {
        Some(RecoverySource::Backup {
            target,
            password,
            server_id,
        }) => Some(RecoverySource::Backup {
            target,
            password: password.decrypt(&ctx).ok_or_else(|| {
                Error::new(
                    color_eyre::eyre::eyre!("Couldn't decode recoveryPassword"),
                    crate::ErrorKind::Unknown,
                )
            })?,
            server_id,
        }),
        Some(RecoverySource::Migrate { guid }) => Some(RecoverySource::Migrate { guid }),
        None => None,
    };

    let setup_ctx = ctx.clone();
    ctx.run_setup(move || {
        execute_inner(
            setup_ctx,
            start_os_logicalname,
            start_os_password,
            recovery,
            kiosk,
        )
    })?;

    Ok(ctx.progress().await)
}

#[instrument(skip_all)]
// #[command(rpc_only)]
pub async fn complete(ctx: SetupContext) -> Result<SetupResult, Error> {
    match ctx.result.get() {
        Some(Ok((res, ctx))) => {
            let mut guid_file = create_file("/media/startos/config/disk.guid").await?;
            guid_file.write_all(ctx.disk_guid.as_bytes()).await?;
            guid_file.sync_all().await?;
            Command::new("systemd-firstboot")
                .arg("--root=/media/startos/config/overlay/")
                .arg(format!("--hostname={}", res.hostname.0))
                .invoke(ErrorKind::ParseSysInfo)
                .await?;
            Command::new("sync").invoke(ErrorKind::Filesystem).await?;

            Ok(res.clone())
        }
        Some(Err(e)) => Err(e.clone_output()),
        None => Err(Error::new(
            eyre!("setup.execute has not completed successfully"),
            crate::ErrorKind::InvalidRequest,
        )),
    }
}

#[instrument(skip_all)]
// #[command(rpc_only)]
pub async fn exit(ctx: SetupContext) -> Result<(), Error> {
    ctx.shutdown.send(()).expect("failed to shutdown");
    Ok(())
}

#[instrument(skip_all)]
pub async fn execute_inner(
    ctx: SetupContext,
    start_os_logicalname: PathBuf,
    start_os_password: String,
    recovery_source: Option<RecoverySource<String>>,
    kiosk: Option<bool>,
) -> Result<(SetupResult, RpcContext), Error> {
    let progress = &ctx.progress;
    let mut disk_phase = progress.add_phase("Formatting data drive".into(), Some(10));
    let restore_phase = match recovery_source.as_ref() {
        Some(RecoverySource::Backup { .. }) => {
            Some(progress.add_phase("Restoring backup".into(), Some(100)))
        }
        Some(RecoverySource::Migrate { .. }) => {
            Some(progress.add_phase("Transferring data".into(), Some(100)))
        }
        None => None,
    };
    let init_phases = InitPhases::new(&progress);
    let rpc_ctx_phases = InitRpcContextPhases::new(&progress);

    disk_phase.start();
    let encryption_password = if ctx.disable_encryption {
        None
    } else {
        Some(DEFAULT_PASSWORD)
    };
    let guid = Arc::new(
        crate::disk::main::create(
            &[start_os_logicalname],
            &pvscan().await?,
            DATA_DIR,
            encryption_password,
        )
        .await?,
    );
    let _ = crate::disk::main::import(&*guid, DATA_DIR, RepairStrategy::Preen, encryption_password)
        .await?;
    disk_phase.complete();

    let progress = SetupExecuteProgress {
        init_phases,
        restore_phase,
        rpc_ctx_phases,
    };

    match recovery_source {
        Some(RecoverySource::Backup {
            target,
            password,
            server_id,
        }) => {
            recover(
                &ctx,
                guid,
                start_os_password,
                target,
                server_id,
                password,
                kiosk,
                progress,
            )
            .await
        }
        Some(RecoverySource::Migrate { guid: old_guid }) => {
            migrate(&ctx, guid, &old_guid, start_os_password, kiosk, progress).await
        }
        None => fresh_setup(&ctx, guid, &start_os_password, kiosk, progress).await,
    }
}

pub struct SetupExecuteProgress {
    pub init_phases: InitPhases,
    pub restore_phase: Option<PhaseProgressTrackerHandle>,
    pub rpc_ctx_phases: InitRpcContextPhases,
}

async fn fresh_setup(
    ctx: &SetupContext,
    guid: Arc<String>,
    start_os_password: &str,
    kiosk: Option<bool>,
    SetupExecuteProgress {
        init_phases,
        rpc_ctx_phases,
        ..
    }: SetupExecuteProgress,
) -> Result<(SetupResult, RpcContext), Error> {
    let account = AccountInfo::new(start_os_password, root_ca_start_time().await?)?;
    let db = ctx.db().await?;
    let kiosk = Some(kiosk.unwrap_or(true)).filter(|_| &*PLATFORM != "raspberrypi");
    sync_kiosk(kiosk).await?;
    db.put(&ROOT, &Database::init(&account, kiosk)?).await?;
    drop(db);

    let init_result = init(&ctx.webserver, &ctx.config, init_phases).await?;

    let rpc_ctx = RpcContext::init(
        &ctx.webserver,
        &ctx.config,
        guid,
        Some(init_result),
        rpc_ctx_phases,
    )
    .await?;

    write_shadow(start_os_password).await?;

    Ok(((&account).try_into()?, rpc_ctx))
}

#[instrument(skip_all)]
async fn recover(
    ctx: &SetupContext,
    guid: Arc<String>,
    start_os_password: String,
    recovery_source: BackupTargetFS,
    server_id: String,
    recovery_password: String,
    kiosk: Option<bool>,
    progress: SetupExecuteProgress,
) -> Result<(SetupResult, RpcContext), Error> {
    let recovery_source = TmpMountGuard::mount(&recovery_source, ReadWrite).await?;
    recover_full_embassy(
        ctx,
        guid.clone(),
        start_os_password,
        recovery_source,
        &server_id,
        &recovery_password,
        kiosk,
        progress,
    )
    .await
}

#[instrument(skip_all)]
async fn migrate(
    ctx: &SetupContext,
    guid: Arc<String>,
    old_guid: &str,
    start_os_password: String,
    kiosk: Option<bool>,
    SetupExecuteProgress {
        init_phases,
        restore_phase,
        rpc_ctx_phases,
    }: SetupExecuteProgress,
) -> Result<(SetupResult, RpcContext), Error> {
    let mut restore_phase = restore_phase.or_not_found("restore progress")?;

    restore_phase.start();
    restore_phase.set_units(Some(ProgressUnits::Bytes));
    let _ = crate::disk::main::import(
        &old_guid,
        "/media/startos/migrate",
        RepairStrategy::Preen,
        if guid.ends_with("_UNENC") {
            None
        } else {
            Some(DEFAULT_PASSWORD)
        },
    )
    .await?;

    let main_transfer_args = ("/media/startos/migrate/main/", formatcp!("{MAIN_DATA}/"));
    let package_data_transfer_args = (
        "/media/startos/migrate/package-data/",
        formatcp!("{PACKAGE_DATA}/"),
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
                restore_phase.set_total(main_transfer_size.load() + package_data_transfer_size.load());
            }
        } => res,
    };

    restore_phase.set_total(size);

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
                restore_phase.set_done(main_transfer_progress.load() + package_data_transfer_progress.load());
            }
        } => res,
    }

    crate::disk::main::export(&old_guid, "/media/startos/migrate").await?;
    restore_phase.complete();

    let (account, net_ctrl) = setup_init(&ctx, Some(start_os_password), kiosk, init_phases).await?;

    let rpc_ctx = RpcContext::init(
        &ctx.webserver,
        &ctx.config,
        guid,
        Some(net_ctrl),
        rpc_ctx_phases,
    )
    .await?;

    Ok(((&account).try_into()?, rpc_ctx))
}
