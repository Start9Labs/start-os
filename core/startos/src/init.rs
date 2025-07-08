use std::fs::Permissions;
use std::io::Cursor;
use std::net::{Ipv4Addr, SocketAddr, SocketAddrV4};
use std::os::unix::fs::PermissionsExt;
use std::path::Path;
use std::sync::Arc;
use std::time::{Duration, SystemTime};

use axum::extract::ws::{self};
use color_eyre::eyre::eyre;
use const_format::formatcp;
use futures::{StreamExt, TryStreamExt};
use itertools::Itertools;
use models::ResultExt;
use rand::random;
use rpc_toolkit::{from_fn_async, Context, Empty, HandlerArgs, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use tokio::process::Command;
use tracing::instrument;
use ts_rs::TS;

use crate::account::AccountInfo;
use crate::context::config::ServerConfig;
use crate::context::{CliContext, InitContext};
use crate::db::model::public::ServerStatus;
use crate::db::model::Database;
use crate::disk::mount::util::unmount;
use crate::hostname::Hostname;
use crate::middleware::auth::LOCAL_AUTH_COOKIE_PATH;
use crate::net::net_controller::{NetController, NetService};
use crate::net::utils::find_wifi_iface;
use crate::net::web_server::{UpgradableListener, WebServerAcceptorSetter};
use crate::prelude::*;
use crate::progress::{
    FullProgress, FullProgressTracker, PhaseProgressTrackerHandle, PhasedProgressBar, ProgressUnits,
};
use crate::rpc_continuations::{Guid, RpcContinuation};
use crate::s9pk::v2::pack::{CONTAINER_DATADIR, CONTAINER_TOOL};
use crate::ssh::SSH_DIR;
use crate::system::{get_mem_info, sync_kiosk};
use crate::util::io::{create_file, open_file, IOHook};
use crate::util::lshw::lshw;
use crate::util::net::WebSocketExt;
use crate::util::{cpupower, Invoke};
use crate::{Error, MAIN_DATA, PACKAGE_DATA};

pub const SYSTEM_REBUILD_PATH: &str = "/media/startos/config/system-rebuild";
pub const STANDBY_MODE_PATH: &str = "/media/startos/config/standby";

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
        unmount("/var/lib/postgresql", true).await?;
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
                    create_file(&incomplete_path).await?.sync_all().await?;
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
    pub net_ctrl: Arc<NetController>,
    pub os_net_service: NetService,
}

pub struct InitPhases {
    preinit: Option<PhaseProgressTrackerHandle>,
    local_auth: PhaseProgressTrackerHandle,
    load_database: PhaseProgressTrackerHandle,
    load_ssh_keys: PhaseProgressTrackerHandle,
    start_net: PhaseProgressTrackerHandle,
    mount_logs: PhaseProgressTrackerHandle,
    load_ca_cert: PhaseProgressTrackerHandle,
    load_wifi: PhaseProgressTrackerHandle,
    init_tmp: PhaseProgressTrackerHandle,
    set_governor: PhaseProgressTrackerHandle,
    sync_clock: PhaseProgressTrackerHandle,
    enable_zram: PhaseProgressTrackerHandle,
    update_server_info: PhaseProgressTrackerHandle,
    launch_service_network: PhaseProgressTrackerHandle,
    validate_db: PhaseProgressTrackerHandle,
    postinit: Option<PhaseProgressTrackerHandle>,
}
impl InitPhases {
    pub fn new(handle: &FullProgressTracker) -> Self {
        Self {
            preinit: if Path::new("/media/startos/config/preinit.sh").exists() {
                Some(handle.add_phase("Running preinit.sh".into(), Some(5)))
            } else {
                None
            },
            local_auth: handle.add_phase("Enabling local authentication".into(), Some(1)),
            load_database: handle.add_phase("Loading database".into(), Some(5)),
            load_ssh_keys: handle.add_phase("Loading SSH Keys".into(), Some(1)),
            start_net: handle.add_phase("Starting network controller".into(), Some(1)),
            mount_logs: handle.add_phase("Switching logs to write to data drive".into(), Some(1)),
            load_ca_cert: handle.add_phase("Loading CA certificate".into(), Some(1)),
            load_wifi: handle.add_phase("Loading WiFi configuration".into(), Some(1)),
            init_tmp: handle.add_phase("Initializing temporary files".into(), Some(1)),
            set_governor: handle.add_phase("Setting CPU performance profile".into(), Some(1)),
            sync_clock: handle.add_phase("Synchronizing system clock".into(), Some(10)),
            enable_zram: handle.add_phase("Enabling ZRAM".into(), Some(1)),
            update_server_info: handle.add_phase("Updating server info".into(), Some(1)),
            launch_service_network: handle.add_phase("Launching service intranet".into(), Some(1)),
            validate_db: handle.add_phase("Validating database".into(), Some(1)),
            postinit: if Path::new("/media/startos/config/postinit.sh").exists() {
                Some(handle.add_phase("Running postinit.sh".into(), Some(5)))
            } else {
                None
            },
        }
    }
}

pub async fn run_script<P: AsRef<Path>>(path: P, mut progress: PhaseProgressTrackerHandle) {
    let script = path.as_ref();
    progress.start();
    if let Err(e) = async {
        let script = tokio::fs::read_to_string(script).await?;
        progress.set_total(script.as_bytes().iter().filter(|b| **b == b'\n').count() as u64);
        progress.set_units(Some(ProgressUnits::Bytes));
        let mut reader = IOHook::new(Cursor::new(script.as_bytes()));
        reader.post_read(|buf| progress += buf.iter().filter(|b| **b == b'\n').count() as u64);
        Command::new("/bin/bash")
            .input(Some(&mut reader))
            .invoke(ErrorKind::Unknown)
            .await?;
        // TODO: inherit?

        Ok::<_, Error>(())
    }
    .await
    {
        tracing::error!("Error Running {}: {}", script.display(), e);
        tracing::debug!("{:?}", e);
    }
    progress.complete();
}

#[instrument(skip_all)]
pub async fn init(
    webserver: &WebServerAcceptorSetter<UpgradableListener>,
    cfg: &ServerConfig,
    InitPhases {
        preinit,
        mut local_auth,
        mut load_database,
        mut load_ssh_keys,
        mut start_net,
        mut mount_logs,
        mut load_ca_cert,
        mut load_wifi,
        mut init_tmp,
        mut set_governor,
        mut sync_clock,
        mut enable_zram,
        mut update_server_info,
        mut launch_service_network,
        mut validate_db,
        postinit,
    }: InitPhases,
) -> Result<InitResult, Error> {
    if let Some(progress) = preinit {
        run_script("/media/startos/config/preinit.sh", progress).await;
    }

    local_auth.start();
    tokio::fs::create_dir_all("/run/startos")
        .await
        .with_ctx(|_| (crate::ErrorKind::Filesystem, "mkdir -p /run/startos"))?;
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
            .arg("root:startos")
            .arg(LOCAL_AUTH_COOKIE_PATH)
            .invoke(crate::ErrorKind::Filesystem)
            .await?;
    }
    local_auth.complete();

    load_database.start();
    let db = cfg.db().await?;
    crate::version::Current::default().pre_init(&db).await?;
    let db = TypedPatchDb::<Database>::load_unchecked(db);
    let peek = db.peek().await;
    load_database.complete();
    tracing::info!("Opened PatchDB");

    load_ssh_keys.start();
    crate::ssh::sync_keys(
        &Hostname(peek.as_public().as_server_info().as_hostname().de()?),
        &peek.as_private().as_ssh_privkey().de()?,
        &peek.as_private().as_ssh_pubkeys().de()?,
        SSH_DIR,
    )
    .await?;
    load_ssh_keys.complete();
    tracing::info!("Synced SSH Keys");

    let account = AccountInfo::load(&peek)?;

    start_net.start();
    let net_ctrl = Arc::new(
        NetController::init(
            db.clone(),
            cfg.tor_control
                .unwrap_or(SocketAddr::from(([127, 0, 0, 1], 9051))),
            cfg.tor_socks.unwrap_or(SocketAddr::V4(SocketAddrV4::new(
                Ipv4Addr::new(127, 0, 0, 1),
                9050,
            ))),
            &account.hostname,
        )
        .await?,
    );
    webserver.try_upgrade(|a| net_ctrl.net_iface.upgrade_listener(a))?;
    let os_net_service = net_ctrl.os_bindings().await?;
    start_net.complete();

    mount_logs.start();
    let log_dir = Path::new(MAIN_DATA).join("logs");
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
    mount_logs.complete();
    tokio::io::copy(
        &mut open_file("/run/startos/init.log").await?,
        &mut tokio::io::stderr(),
    )
    .await?;
    tracing::info!("Mounted Logs");

    load_ca_cert.start();
    // write to ca cert store
    tokio::fs::write(
        "/usr/local/share/ca-certificates/startos-root-ca.crt",
        account.root_ca_cert.to_pem()?,
    )
    .await?;
    Command::new("update-ca-certificates")
        .invoke(crate::ErrorKind::OpenSsl)
        .await?;
    load_ca_cert.complete();

    load_wifi.start();
    let wifi_interface = find_wifi_iface().await?;
    let wifi = db
        .mutate(|db| {
            let wifi = db
                .as_public_mut()
                .as_server_info_mut()
                .as_network_mut()
                .as_wifi_mut();
            wifi.as_interface_mut().ser(&wifi_interface)?;
            wifi.de()
        })
        .await
        .result?;
    crate::net::wifi::synchronize_network_manager(MAIN_DATA, &wifi).await?;
    load_wifi.complete();
    tracing::info!("Synchronized WiFi");

    init_tmp.start();
    let tmp_dir = Path::new(PACKAGE_DATA).join("tmp");
    if tokio::fs::metadata(&tmp_dir).await.is_ok() {
        tokio::fs::remove_dir_all(&tmp_dir).await?;
    }
    if tokio::fs::metadata(&tmp_dir).await.is_err() {
        tokio::fs::create_dir_all(&tmp_dir).await?;
    }
    let tmp_var = Path::new(PACKAGE_DATA).join("tmp/var");
    if tokio::fs::metadata(&tmp_var).await.is_ok() {
        tokio::fs::remove_dir_all(&tmp_var).await?;
    }
    crate::disk::mount::util::bind(&tmp_var, "/var/tmp", false).await?;
    let downloading = Path::new(PACKAGE_DATA).join("archive/downloading");
    if tokio::fs::metadata(&downloading).await.is_ok() {
        tokio::fs::remove_dir_all(&downloading).await?;
    }
    let tmp_docker = Path::new(PACKAGE_DATA).join(formatcp!("tmp/{CONTAINER_TOOL}"));
    crate::disk::mount::util::bind(&tmp_docker, CONTAINER_DATADIR, false).await?;
    init_tmp.complete();

    let server_info = db.peek().await.into_public().into_server_info();
    set_governor.start();
    let selected_governor = server_info.as_governor().de()?;
    let governor = if let Some(governor) = &selected_governor {
        if cpupower::get_available_governors()
            .await?
            .contains(governor)
        {
            Some(governor)
        } else {
            tracing::warn!("CPU Governor \"{governor}\" Not Available");
            None
        }
    } else {
        cpupower::get_preferred_governor().await?
    };
    if let Some(governor) = governor {
        tracing::info!("Setting CPU Governor to \"{governor}\"");
        cpupower::set_governor(governor).await?;
        tracing::info!("Set CPU Governor");
    }
    set_governor.complete();

    sync_clock.start();
    let mut ntp_synced = false;
    let mut not_made_progress = 0u32;
    for _ in 0..1800 {
        if check_time_is_synchronized().await? {
            ntp_synced = true;
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
    if !ntp_synced {
        tracing::warn!("Timed out waiting for system time to synchronize");
    } else {
        tracing::info!("Syncronized system clock");
    }
    sync_clock.complete();

    enable_zram.start();
    if server_info.as_zram().de()? {
        crate::system::enable_zram().await?;
        tracing::info!("Enabled ZRAM");
    }
    enable_zram.complete();

    update_server_info.start();
    sync_kiosk(server_info.as_kiosk().de()?).await?;
    let ram = get_mem_info().await?.total.0 as u64 * 1024 * 1024;
    let devices = lshw().await?;
    let status_info = ServerStatus {
        updated: false,
        update_progress: None,
        backup_progress: None,
        shutting_down: false,
        restarting: false,
    };
    db.mutate(|v| {
        let server_info = v.as_public_mut().as_server_info_mut();
        server_info.as_ntp_synced_mut().ser(&ntp_synced)?;
        server_info.as_ram_mut().ser(&ram)?;
        server_info.as_devices_mut().ser(&devices)?;
        server_info.as_status_info_mut().ser(&status_info)?;
        Ok(())
    })
    .await
    .result?;
    tracing::info!("Updated server info");
    update_server_info.complete();

    launch_service_network.start();
    Command::new("systemctl")
        .arg("start")
        .arg("lxc-net.service")
        .invoke(ErrorKind::Lxc)
        .await?;
    tracing::info!("Launched service intranet");
    launch_service_network.complete();

    validate_db.start();
    db.mutate(|d| {
        let model = d.de()?;
        d.ser(&model)
    })
    .await
    .result?;
    tracing::info!("Validated database");
    validate_db.complete();

    if let Some(progress) = postinit {
        run_script("/media/startos/config/postinit.sh", progress).await;
    }

    tracing::info!("System initialized.");

    Ok(InitResult {
        net_ctrl,
        os_net_service,
    })
}

pub fn init_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "logs",
            crate::system::logs::<InitContext>().with_about("Disply OS logs"),
        )
        .subcommand(
            "logs",
            from_fn_async(crate::logs::cli_logs::<InitContext, Empty>)
                .no_display()
                .with_about("Display OS logs"),
        )
        .subcommand(
            "kernel-logs",
            crate::system::kernel_logs::<InitContext>().with_about("Display kernel logs"),
        )
        .subcommand(
            "kernel-logs",
            from_fn_async(crate::logs::cli_logs::<InitContext, Empty>)
                .no_display()
                .with_about("Display kernel logs"),
        )
        .subcommand("subscribe", from_fn_async(init_progress).no_cli())
        .subcommand(
            "subscribe",
            from_fn_async(cli_init_progress)
                .no_display()
                .with_about("Get initialization progress"),
        )
}

#[derive(Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct InitProgressRes {
    pub progress: FullProgress,
    pub guid: Guid,
}

pub async fn init_progress(ctx: InitContext) -> Result<InitProgressRes, Error> {
    let progress_tracker = ctx.progress.clone();
    let progress = progress_tracker.snapshot();
    let mut error = ctx.error.subscribe();
    let guid = Guid::new();
    ctx.rpc_continuations
        .add(
            guid.clone(),
            RpcContinuation::ws(
                |mut ws| async move {
                    let res = tokio::try_join!(
                        async {
                            let mut stream =
                                progress_tracker.stream(Some(Duration::from_millis(100)));
                            while let Some(progress) = stream.next().await {
                                ws.send(ws::Message::Text(
                                    serde_json::to_string(&progress)
                                        .with_kind(ErrorKind::Serialization)?
                                        .into(),
                                ))
                                .await
                                .with_kind(ErrorKind::Network)?;
                                if progress.overall.is_complete() {
                                    break;
                                }
                            }

                            Ok::<_, Error>(())
                        },
                        async {
                            if let Some(e) = error
                                .wait_for(|e| e.is_some())
                                .await
                                .ok()
                                .and_then(|e| e.as_ref().map(|e| e.clone_output()))
                            {
                                Err::<(), _>(e)
                            } else {
                                Ok(())
                            }
                        }
                    );

                    if let Err(e) = ws
                        .close_result(res.map(|_| "complete").map_err(|e| {
                            tracing::error!("error in init progress websocket: {e}");
                            tracing::debug!("{e:?}");
                            e
                        }))
                        .await
                    {
                        tracing::error!("error closing init progress websocket: {e}");
                        tracing::debug!("{e:?}");
                    }
                },
                Duration::from_secs(30),
            ),
        )
        .await;
    Ok(InitProgressRes { progress, guid })
}

pub async fn cli_init_progress(
    HandlerArgs {
        context: ctx,
        parent_method,
        method,
        raw_params,
        ..
    }: HandlerArgs<CliContext>,
) -> Result<(), Error> {
    let res: InitProgressRes = from_value(
        ctx.call_remote::<InitContext>(
            &parent_method
                .into_iter()
                .chain(method.into_iter())
                .join("."),
            raw_params,
        )
        .await?,
    )?;
    let mut ws = ctx.ws_continuation(res.guid).await?;
    let mut bar = PhasedProgressBar::new("Initializing...");
    while let Some(msg) = ws.try_next().await.with_kind(ErrorKind::Network)? {
        if let tokio_tungstenite::tungstenite::Message::Text(msg) = msg {
            bar.update(&serde_json::from_str(&msg).with_kind(ErrorKind::Deserialization)?);
        }
    }
    Ok(())
}
