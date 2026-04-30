use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::time::Duration;

use color_eyre::eyre::eyre;
use const_format::formatcp;
use josekit::jwk::Jwk;
use patch_db::json_ptr::ROOT;
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{Context, Empty, HandlerExt, ParentHandler, from_fn_async};

use crate::context::CliContext;
use serde::{Deserialize, Serialize};
use tokio::io::AsyncWriteExt;
use tokio::process::Command;
use tokio::try_join;
use tracing::instrument;
use ts_rs::TS;

use crate::account::AccountInfo;
use crate::auth::write_shadow;
use crate::backup::restore::recover_full_server;
use crate::backup::target::BackupTargetFS;
use crate::bins::set_locale;
use crate::context::rpc::InitRpcContextPhases;
use crate::context::setup::SetupResult;
use crate::context::{RpcContext, SetupContext};
use crate::db::model::Database;
use crate::disk::REPAIR_DISK_PATH;
use crate::disk::fsck::RepairStrategy;
use crate::disk::main::DEFAULT_PASSWORD;
use crate::disk::mount::filesystem::ReadWrite;
use crate::disk::mount::filesystem::cifs::Cifs;
use crate::disk::mount::guard::{GenericMountGuard, TmpMountGuard};
use crate::disk::util::{DiskInfo, StartOsRecoveryInfo, pvscan, recovery_info};
use crate::hostname::ServerHostnameInfo;
use crate::init::{InitPhases, InitResult, init};
use crate::net::ssl::root_ca_start_time;
use crate::prelude::*;
use crate::progress::{FullProgress, PhaseProgressTrackerHandle, ProgressUnits};
use crate::rpc_continuations::Guid;
use crate::shutdown::Shutdown;
use crate::system::{KeyboardOptions, SetLanguageParams, save_language, sync_kiosk};
use crate::util::Invoke;
use crate::util::crypto::EncryptedWire;
use crate::util::io::{Counter, create_file, dir_copy, dir_size, read_file_to_string};
use crate::util::serde::{HandlerExtSerde, IoFormat, Pem};
use crate::{DATA_DIR, Error, ErrorKind, MAIN_DATA, PACKAGE_DATA, PLATFORM, ResultExt};

pub fn setup<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "status",
            from_fn_async(status)
                .with_metadata("authenticated", Value::Bool(false))
                .with_display_serializable()
                .with_about("about.setup-status")
                .with_call_remote::<CliContext>(),
        )
        .subcommand("disk", disk::<C>())
        .subcommand("attach", from_fn_async(attach).no_cli())
        .subcommand(
            "install-os",
            from_fn_async(crate::os_install::install_os)
                .with_display_serializable()
                .with_about("about.setup-install-os")
                .with_call_remote::<CliContext>(),
        )
        .subcommand("execute", from_fn_async(execute).no_cli())
        .subcommand("cifs", cifs::<C>())
        .subcommand(
            "complete",
            from_fn_async(complete)
                .with_display_serializable()
                .with_about("about.setup-complete")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "get-pubkey",
            from_fn_async(get_pubkey)
                .with_metadata("authenticated", Value::Bool(false))
                .with_display_serializable()
                .with_about("about.setup-get-pubkey")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "exit",
            from_fn_async(exit)
                .no_display()
                .with_about("about.setup-exit")
                .with_call_remote::<CliContext>(),
        )
        .subcommand("logs", crate::system::logs::<SetupContext>())
        .subcommand(
            "logs",
            from_fn_async(crate::logs::cli_logs::<SetupContext, Empty>)
                .no_display()
                .with_about("about.display-os-logs"),
        )
        .subcommand(
            "restart",
            from_fn_async(restart)
                .no_display()
                .with_about("about.setup-restart")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "shutdown",
            from_fn_async(shutdown)
                .no_display()
                .with_about("about.setup-shutdown")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "set-language",
            from_fn_async(set_language)
                .no_display()
                .with_about("about.setup-set-language")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "set-keyboard",
            from_fn_async(set_keyboard)
                .no_display()
                .with_about("about.setup-set-keyboard")
                .with_call_remote::<CliContext>(),
        )
}

pub fn disk<C: Context>() -> ParentHandler<C> {
    ParentHandler::new().subcommand(
        "list",
        from_fn_async(list_disks)
            .with_metadata("authenticated", Value::Bool(false))
            .with_display_serializable()
            .with_about("about.setup-disk-list")
            .with_call_remote::<CliContext>(),
    )
}

const LIVE_MEDIUM_PATH: &str = "/run/live/medium";

pub async fn list_disks(ctx: SetupContext) -> Result<Vec<DiskInfo>, Error> {
    let mut disks = crate::disk::util::list(
        &crate::disk::OsPartitionInfo::from_fstab()
            .await
            .unwrap_or_default(),
    )
    .await?;

    // Filter out the disk containing the live medium (installer USB)
    if let Ok(Some(live_medium_source)) =
        crate::disk::util::get_mount_source(LIVE_MEDIUM_PATH).await
    {
        disks.retain(|disk| disk.logicalname != live_medium_source);
    }

    Ok(disks)
}

#[instrument(skip_all)]
async fn setup_init(
    ctx: &SetupContext,
    password: Option<String>,
    kiosk: bool,
    hostname: Option<ServerHostnameInfo>,
    init_phases: InitPhases,
) -> Result<(AccountInfo, InitResult), Error> {
    let init_result = init(&ctx.webserver, &ctx.config.peek(|c| c.clone()), init_phases).await?;
    let language = ctx.language.peek(|a| a.clone());
    let keyboard = ctx.keyboard.peek(|a| a.clone());

    let account = init_result
        .net_ctrl
        .db
        .mutate(|m| {
            let mut account = AccountInfo::load(m)?;
            if let Some(password) = &password {
                account.set_password(password)?;
            }
            if let Some(hostname) = hostname {
                account.hostname = hostname;
            }
            account.save(m)?;
            let info = m.as_public_mut().as_server_info_mut();
            info.as_password_hash_mut().ser(&account.password)?;
            info.as_kiosk_mut()
                .ser(&Some(kiosk).filter(|_| &*PLATFORM != "raspberrypi"))?;
            if let Some(language) = language.clone() {
                info.as_language_mut().ser(&Some(language))?;
            }
            if let Some(keyboard) = keyboard.clone() {
                info.as_keyboard_mut().ser(&Some(keyboard))?;
            }

            Ok(account)
        })
        .await
        .result?;

    sync_kiosk(kiosk).await?;

    if let Some(language) = language {
        save_language(&*language).await?;
    }
    if let Some(keyboard) = keyboard {
        keyboard.save().await?;
    }

    if let Some(password) = &password {
        write_shadow(&password).await?;
    }

    Ok((account, init_result))
}

#[derive(Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AttachParams {
    pub password: Option<EncryptedWire>,
    pub guid: InternedString,
    pub kiosk: bool,
}

#[instrument(skip_all)]
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
        let mut disk_phase = progress.add_phase(t!("setup.opening-data-drive").into(), Some(10));
        let init_phases = InitPhases::new(&progress);
        let rpc_ctx_phases = InitRpcContextPhases::new(&progress);

        let password: Option<String> = match password {
            Some(a) => match a.decrypt(&setup_ctx) {
                a @ Some(_) => a,
                None => {
                    return Err(Error::new(
                        color_eyre::eyre::eyre!("{}", t!("setup.couldnt-decode-password")),
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
            if disk_guid.ends_with("_UNENC") {
                None
            } else {
                Some(DEFAULT_PASSWORD)
            },
            Some(&*progress),
        )
        .await?;
        let _ = setup_ctx.disk_guid.set(disk_guid.clone());
        if tokio::fs::metadata(REPAIR_DISK_PATH).await.is_ok() {
            tokio::fs::remove_file(REPAIR_DISK_PATH)
                .await
                .with_ctx(|_| (ErrorKind::Filesystem, REPAIR_DISK_PATH))?;
        }
        if requires_reboot.0 {
            crate::disk::main::export(&*disk_guid, DATA_DIR).await?;
            return Err(Error::new(
                eyre!("{}", t!("setup.disk-errors-corrected-restart-required")),
                ErrorKind::DiskManagement,
            ));
        }
        disk_phase.complete();

        let (account, net_ctrl) =
            setup_init(&setup_ctx, password, kiosk, None, init_phases).await?;

        let rpc_ctx = RpcContext::init(
            &setup_ctx.webserver,
            &setup_ctx.config.peek(|c| c.clone()),
            disk_guid,
            Some(net_ctrl),
            rpc_ctx_phases,
        )
        .await?;

        Ok((
            SetupResult {
                hostname: account.hostname.hostname,
                root_ca: Pem(account.root_ca_cert),
                needs_restart: setup_ctx.install_rootfs.peek(|a| a.is_some()),
            },
            rpc_ctx,
        ))
    })?;

    Ok(ctx.progress().await)
}

#[derive(Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "kebab-case")]
#[ts(export)]
#[serde(tag = "status")]
pub enum SetupStatusRes {
    NeedsInstall,
    Incomplete(SetupInfo),
    Running(SetupProgress),
    Complete(SetupResult),
}

#[derive(Default, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct SetupInfo {
    pub guid: Option<InternedString>,
    pub attach: bool,
    pub mok_enrolled: bool,
}

#[derive(Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SetupProgress {
    pub progress: FullProgress,
    pub guid: Guid,
}

pub async fn status(ctx: SetupContext) -> Result<SetupStatusRes, Error> {
    if let Some(res) = ctx.result.get() {
        match res {
            Ok((res, _)) => Ok(SetupStatusRes::Complete(res.clone())),
            Err(e) => Err(e.clone_output()),
        }
    } else {
        if ctx.task.initialized() {
            Ok(SetupStatusRes::Running(ctx.progress().await))
        } else {
            let path = Path::new("/media/startos/config/setup.json");
            if tokio::fs::metadata(path).await.is_err() {
                return Ok(SetupStatusRes::NeedsInstall);
            }
            IoFormat::Json
                .from_slice(read_file_to_string(path).await?.as_bytes())
                .map(SetupStatusRes::Incomplete)
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
            eyre!("{}", t!("setup.no-backup-found")),
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

pub async fn setup_data_drive(
    ctx: &SetupContext,
    logicalname: &Path,
) -> Result<InternedString, Error> {
    let encryption_password = if ctx.disable_encryption {
        None
    } else {
        Some(DEFAULT_PASSWORD)
    };
    let guid = crate::disk::main::create(
        &[logicalname],
        &pvscan().await?,
        DATA_DIR,
        encryption_password,
    )
    .await?;
    let _ = crate::disk::main::import(
        &*guid,
        DATA_DIR,
        RepairStrategy::Preen,
        encryption_password,
        None,
    )
    .await?;
    let _ = ctx.disk_guid.set(guid.clone());
    Ok(guid)
}

#[derive(Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SetupExecuteParams {
    guid: InternedString,
    password: Option<EncryptedWire>,
    recovery_source: Option<RecoverySource<EncryptedWire>>,
    kiosk: bool,
    name: Option<InternedString>,
    hostname: Option<InternedString>,
}

// #[command(rpc_only)]
pub async fn execute(
    ctx: SetupContext,
    SetupExecuteParams {
        guid,
        password,
        recovery_source,
        kiosk,
        name,
        hostname,
    }: SetupExecuteParams,
) -> Result<SetupProgress, Error> {
    let password = password
        .map(|p| {
            p.decrypt(&ctx).ok_or_else(|| {
                Error::new(
                    color_eyre::eyre::eyre!("{}", t!("setup.couldnt-decode-startos-password")),
                    crate::ErrorKind::Unknown,
                )
            })
        })
        .transpose()?;
    let recovery = match recovery_source {
        Some(RecoverySource::Backup {
            target,
            password,
            server_id,
        }) => Some(RecoverySource::Backup {
            target,
            password: password.decrypt(&ctx).ok_or_else(|| {
                Error::new(
                    color_eyre::eyre::eyre!("{}", t!("setup.couldnt-decode-recovery-password")),
                    crate::ErrorKind::Unknown,
                )
            })?,
            server_id,
        }),
        Some(RecoverySource::Migrate { guid }) => Some(RecoverySource::Migrate { guid }),
        None => None,
    };

    let hostname = ServerHostnameInfo::new_opt(name, hostname)?;

    let setup_ctx = ctx.clone();
    ctx.run_setup(move || execute_inner(setup_ctx, guid, password, recovery, kiosk, hostname))?;

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
                .arg(format!("--hostname={}", res.hostname.as_ref()))
                .invoke(ErrorKind::ParseSysInfo)
                .await?;
            Command::new("sync").invoke(ErrorKind::Filesystem).await?;

            Ok(res.clone())
        }
        Some(Err(e)) => Err(e.clone_output()),
        None => Err(Error::new(
            eyre!("{}", t!("setup.execute-not-completed")),
            crate::ErrorKind::InvalidRequest,
        )),
    }
}

#[instrument(skip_all)]
pub async fn exit(ctx: SetupContext) -> Result<(), Error> {
    let shutdown = if let Some((rootfs, config)) = ctx.install_rootfs.replace(None) {
        config.unmount(false).await?;
        rootfs.unmount().await?;
        Some(Shutdown {
            disk_guid: ctx.disk_guid.get().cloned(),
            restart: true,
        })
    } else {
        None
    };

    ctx.shutdown
        .send(shutdown)
        .map_err(|e| eyre!("failed to shutdown: {e}"))
        .log_err();
    Ok(())
}

#[instrument(skip_all)]
pub async fn restart(ctx: SetupContext) -> Result<(), Error> {
    if let Some((rootfs, config)) = ctx.install_rootfs.replace(None) {
        config.unmount(false).await?;
        rootfs.unmount().await?;
    }
    ctx.shutdown
        .send(Some(Shutdown {
            disk_guid: ctx.disk_guid.get().cloned(),
            restart: true,
        }))
        .map_err(|e| eyre!("failed to shutdown: {e}"))
        .log_err();
    Ok(())
}

#[instrument(skip_all)]
pub async fn shutdown(ctx: SetupContext) -> Result<(), Error> {
    if let Some((rootfs, config)) = ctx.install_rootfs.replace(None) {
        config.unmount(false).await?;
        rootfs.unmount().await?;
    }
    ctx.shutdown
        .send(Some(Shutdown {
            disk_guid: ctx.disk_guid.get().cloned(),
            restart: false,
        }))
        .map_err(|e| eyre!("failed to shutdown: {e}"))
        .log_err();
    Ok(())
}

#[instrument(skip_all)]
pub async fn execute_inner(
    ctx: SetupContext,
    guid: InternedString,
    password: Option<String>,
    recovery_source: Option<RecoverySource<String>>,
    kiosk: bool,
    hostname: Option<ServerHostnameInfo>,
) -> Result<(SetupResult, RpcContext), Error> {
    let progress = &ctx.progress;

    if !crate::disk::mount::util::is_mountpoint(Path::new(DATA_DIR).join("main")).await? {
        let mut disk_phase = progress.add_phase(t!("setup.opening-data-drive").into(), Some(10));
        disk_phase.start();
        let requires_reboot = crate::disk::main::import(
            &*guid,
            DATA_DIR,
            if tokio::fs::metadata(REPAIR_DISK_PATH).await.is_ok() {
                RepairStrategy::Aggressive
            } else {
                RepairStrategy::Preen
            },
            if guid.ends_with("_UNENC") {
                None
            } else {
                Some(DEFAULT_PASSWORD)
            },
            Some(progress),
        )
        .await?;
        let _ = ctx.disk_guid.set(guid.clone());
        crate::util::io::delete_file(REPAIR_DISK_PATH).await?;
        if requires_reboot.0 {
            crate::disk::main::export(&*guid, DATA_DIR).await?;
            return Err(Error::new(
                eyre!("{}", t!("setup.disk-errors-corrected-restart-required")),
                ErrorKind::DiskManagement,
            ));
        }
        disk_phase.complete();
    }

    let restore_phase = match recovery_source.as_ref() {
        Some(RecoverySource::Backup { .. }) => {
            Some(progress.add_phase(t!("setup.restoring-backup").into(), Some(100)))
        }
        Some(RecoverySource::Migrate { .. }) => {
            Some(progress.add_phase(t!("setup.transferring-data").into(), Some(100)))
        }
        None => None,
    };
    let init_phases = InitPhases::new(&progress);
    let rpc_ctx_phases = InitRpcContextPhases::new(&progress);

    let progress = SetupExecuteProgress {
        init_phases,
        restore_phase,
        rpc_ctx_phases,
    };

    match recovery_source {
        Some(RecoverySource::Backup {
            target,
            password: recovery_password,
            server_id,
        }) => {
            recover(
                &ctx,
                guid,
                password,
                target,
                server_id,
                recovery_password,
                kiosk,
                hostname,
                progress,
            )
            .await
        }
        Some(RecoverySource::Migrate { guid: old_guid }) => {
            migrate(&ctx, guid, &old_guid, password, kiosk, hostname, progress).await
        }
        None => {
            fresh_setup(
                &ctx,
                guid,
                &password.ok_or_else(|| {
                    Error::new(
                        eyre!("{}", t!("setup.password-required")),
                        ErrorKind::InvalidRequest,
                    )
                })?,
                kiosk,
                hostname,
                progress,
            )
            .await
        }
    }
}

pub struct SetupExecuteProgress {
    pub init_phases: InitPhases,
    pub restore_phase: Option<PhaseProgressTrackerHandle>,
    pub rpc_ctx_phases: InitRpcContextPhases,
}

async fn fresh_setup(
    ctx: &SetupContext,
    guid: InternedString,
    password: &str,
    kiosk: bool,
    hostname: Option<ServerHostnameInfo>,
    SetupExecuteProgress {
        init_phases,
        rpc_ctx_phases,
        ..
    }: SetupExecuteProgress,
) -> Result<(SetupResult, RpcContext), Error> {
    let account = AccountInfo::new(password, root_ca_start_time().await, hostname)?;

    let db = ctx.db().await?;
    sync_kiosk(kiosk).await?;

    let language = ctx.language.peek(|a| a.clone());
    let keyboard = ctx.keyboard.peek(|a| a.clone());

    if let Some(language) = &language {
        save_language(&**language).await?;
    }

    if let Some(keyboard) = &keyboard {
        keyboard.save().await?;
    }

    db.put(&ROOT, &Database::init(&account, kiosk, language, keyboard)?)
        .await?;
    drop(db);

    let config = ctx.config.peek(|c| c.clone());

    let init_result = init(&ctx.webserver, &config, init_phases).await?;

    let rpc_ctx = RpcContext::init(
        &ctx.webserver,
        &config,
        guid,
        Some(init_result),
        rpc_ctx_phases,
    )
    .await?;

    write_shadow(password).await?;

    Ok((
        SetupResult {
            hostname: account.hostname.hostname,
            root_ca: Pem(account.root_ca_cert),
            needs_restart: ctx.install_rootfs.peek(|a| a.is_some()),
        },
        rpc_ctx,
    ))
}

#[instrument(skip_all)]
async fn recover(
    ctx: &SetupContext,
    guid: InternedString,
    password: Option<String>,
    recovery_source: BackupTargetFS,
    server_id: String,
    recovery_password: String,
    kiosk: bool,
    hostname: Option<ServerHostnameInfo>,
    progress: SetupExecuteProgress,
) -> Result<(SetupResult, RpcContext), Error> {
    let recovery_source = TmpMountGuard::mount(&recovery_source, ReadWrite).await?;
    recover_full_server(
        ctx,
        guid.clone(),
        password,
        recovery_source,
        &server_id,
        &recovery_password,
        kiosk,
        hostname,
        progress,
    )
    .await
}

#[instrument(skip_all)]
async fn migrate(
    ctx: &SetupContext,
    guid: InternedString,
    old_guid: &str,
    password: Option<String>,
    kiosk: bool,
    hostname: Option<ServerHostnameInfo>,
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
        Some(&ctx.progress),
    )
    .await?;

    let main_transfer_args = ("/media/startos/migrate/main/", formatcp!("{MAIN_DATA}/"));
    let package_data_transfer_args = (
        "/media/startos/migrate/package-data/",
        formatcp!("{PACKAGE_DATA}/"),
    );

    let tmpdir = Path::new(package_data_transfer_args.0).join("tmp");
    crate::util::io::delete_dir(&tmpdir).await?;

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

    let (account, net_ctrl) = setup_init(&ctx, password, kiosk, hostname, init_phases).await?;

    let rpc_ctx = RpcContext::init(
        &ctx.webserver,
        &ctx.config.peek(|c| c.clone()),
        guid,
        Some(net_ctrl),
        rpc_ctx_phases,
    )
    .await?;

    Ok((
        SetupResult {
            hostname: account.hostname.hostname,
            root_ca: Pem(account.root_ca_cert),
            needs_restart: ctx.install_rootfs.peek(|a| a.is_some()),
        },
        rpc_ctx,
    ))
}

pub async fn set_language(
    ctx: SetupContext,
    SetLanguageParams { language }: SetLanguageParams,
) -> Result<(), Error> {
    set_locale(&*language);
    ctx.language.replace(Some(language));
    Ok(())
}

pub async fn set_keyboard(ctx: SetupContext, options: KeyboardOptions) -> Result<(), Error> {
    options.apply_to_session().await?;
    ctx.keyboard.replace(Some(options));
    Ok(())
}
