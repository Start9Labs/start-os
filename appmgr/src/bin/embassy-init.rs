use std::path::Path;

use embassy::context::rpc::RpcContextConfig;
use embassy::context::{DiagnosticContext, SetupContext};
use embassy::db::model::ServerStatus;
use embassy::db::DatabaseModel;
use embassy::disk::main::DEFAULT_PASSWORD;
use embassy::middleware::cors::cors;
use embassy::middleware::diagnostic::diagnostic;
use embassy::middleware::encrypt::encrypt;
#[cfg(feature = "avahi")]
use embassy::net::mdns::MdnsController;
use embassy::sound::MARIO_COIN;
use embassy::util::{Invoke, Version};
use embassy::{Error, ResultExt};
use http::StatusCode;
use rpc_toolkit::rpc_server;
use tokio::process::Command;

fn status_fn(_: i32) -> StatusCode {
    StatusCode::OK
}

async fn init(cfg_path: Option<&str>) -> Result<(), Error> {
    let cfg = RpcContextConfig::load(cfg_path).await?;
    embassy::disk::util::mount("LABEL=EMBASSY", "/embassy-os").await?;
    if tokio::fs::metadata("/embassy-os/disk.guid").await.is_err() {
        #[cfg(feature = "avahi")]
        let _mdns = MdnsController::init();
        tokio::fs::write(
            "/etc/nginx/sites-available/default",
            include_str!("../nginx/setup-wizard.conf"),
        )
        .await
        .with_ctx(|_| {
            (
                embassy::ErrorKind::Filesystem,
                "/etc/nginx/sites-available/default",
            )
        })?;
        Command::new("systemctl")
            .arg("reload")
            .arg("nginx")
            .invoke(embassy::ErrorKind::Nginx)
            .await?;
        let ctx = SetupContext::init(cfg_path).await?;
        let keysource_ctx = ctx.clone();
        let keysource = move || {
            let ctx = keysource_ctx.clone();
            async move { ctx.product_key().await }
        };
        let encrypt = encrypt(keysource);
        MARIO_COIN.play().await?;
        rpc_server!({
            command: embassy::setup_api,
            context: ctx.clone(),
            status: status_fn,
            middleware: [
                cors,
                encrypt,
            ]
        })
        .with_graceful_shutdown({
            let mut shutdown = ctx.shutdown.subscribe();
            async move {
                shutdown.recv().await.expect("context dropped");
            }
        })
        .await
        .with_kind(embassy::ErrorKind::Network)?;
        let pool_name = ctx.zfs_pool_name.clone();
        drop(ctx);
        embassy::disk::main::export(&*pool_name).await?;
    }

    embassy::disk::main::load(
        tokio::fs::read_to_string("/embassy-os/disk.guid")
            .await?
            .trim(),
        cfg.zfs_pool_name(),
        cfg.datadir(),
        DEFAULT_PASSWORD,
    )
    .await?;
    log::info!("Loaded Disk");
    let secret_store = cfg.secret_store().await?;
    let log_dir = cfg.datadir().join("main").join("logs");
    if tokio::fs::metadata(&log_dir).await.is_err() {
        tokio::fs::create_dir_all(&log_dir).await?;
    }
    embassy::disk::util::bind(&log_dir, "/var/log/journal", false).await?;
    Command::new("systemctl")
        .arg("restart")
        .arg("systemd-journald")
        .invoke(embassy::ErrorKind::Journald)
        .await?;
    log::info!("Mounted Logs");
    let tmp_docker = cfg.datadir().join("tmp").join("docker");
    if tokio::fs::metadata(&tmp_docker).await.is_ok() {
        tokio::fs::remove_dir_all(&tmp_docker).await?;
    }
    Command::new("cp")
        .arg("-r")
        .arg("/var/lib/docker")
        .arg(&tmp_docker)
        .invoke(embassy::ErrorKind::Filesystem)
        .await?;
    Command::new("systemctl")
        .arg("stop")
        .arg("docker")
        .invoke(embassy::ErrorKind::Docker)
        .await?;
    embassy::disk::util::bind(&tmp_docker, "/var/lib/docker", false).await?;
    Command::new("systemctl")
        .arg("reset-failed")
        .arg("docker")
        .invoke(embassy::ErrorKind::Docker)
        .await?;
    Command::new("systemctl")
        .arg("start")
        .arg("docker")
        .invoke(embassy::ErrorKind::Docker)
        .await?;
    log::info!("Mounted Docker Data");
    embassy::install::load_images(cfg.datadir()).await?;
    log::info!("Loaded Docker Images");
    embassy::ssh::sync_keys_from_db(&secret_store, "/root/.ssh/authorized_keys").await?;
    log::info!("Synced SSH Keys");
    // todo!("sync wifi");
    embassy::hostname::sync_hostname().await?;
    log::info!("Synced Hostname");

    if tokio::fs::metadata("/var/www/html/main/public")
        .await
        .is_err()
    {
        tokio::fs::create_dir_all("/var/www/html/main/public").await?
    }
    if tokio::fs::symlink_metadata("/var/www/html/main/public/package-data")
        .await
        .is_err()
    {
        tokio::fs::symlink(
            cfg.datadir().join("package-data").join("public"),
            "/var/www/html/main/public/package-data",
        )
        .await?;
    }
    log::info!("Enabled nginx public dir");
    embassy::net::wifi::synchronize_wpa_supplicant_conf(&cfg.datadir().join("main")).await?;

    let db = cfg.db(&secret_store).await?;
    let mut handle = db.handle();
    let mut info = embassy::db::DatabaseModel::new()
        .server_info()
        .get_mut(&mut handle)
        .await?;
    match info.status {
        ServerStatus::Running | ServerStatus::Updated | ServerStatus::BackingUp => {
            info.status = ServerStatus::Running;
        }
        ServerStatus::Updating => {
            info.update_progress = None;
            info.status = ServerStatus::Running;
        }
    }
    info.version = emver::Version::new(0, 3, 0, 0).into();
    // TODO: run migrations
    info.save(&mut handle).await?;

    Ok(())
}

async fn run_script_if_exists<P: AsRef<Path>>(path: P) {
    let script = path.as_ref();
    if script.exists() {
        match Command::new("/bin/bash").arg(script).spawn() {
            Ok(mut c) => {
                if let Err(e) = c.wait().await {
                    log::error!("Error Running {}: {}", script.display(), e)
                }
            }
            Err(e) => log::error!("Error Running {}: {}", script.display(), e),
        }
    }
}

async fn inner_main(cfg_path: Option<&str>) -> Result<(), Error> {
    embassy::sound::BEP.play().await?;

    run_script_if_exists("/embassy-os/preinit.sh").await;

    let res = if let Err(e) = init(cfg_path).await {
        (|| async {
            log::error!("{}", e.source);
            log::debug!("{}", e.source);
            embassy::sound::BEETHOVEN.play().await?;
            #[cfg(feature = "avahi")]
            let _mdns = MdnsController::init();
            tokio::fs::write(
                "/etc/nginx/sites-available/default",
                include_str!("../nginx/diagnostic-ui.conf"),
            )
            .await
            .with_ctx(|_| {
                (
                    embassy::ErrorKind::Filesystem,
                    "/etc/nginx/sites-available/default",
                )
            })?;
            Command::new("systemctl")
                .arg("reload")
                .arg("nginx")
                .invoke(embassy::ErrorKind::Nginx)
                .await?;
            let ctx = DiagnosticContext::init(cfg_path, e).await?;
            rpc_server!({
                command: embassy::diagnostic_api,
                context: ctx.clone(),
                status: status_fn,
                middleware: [
                    cors,
                    diagnostic,
                ]
            })
            .with_graceful_shutdown({
                let mut shutdown = ctx.shutdown.subscribe();
                async move {
                    shutdown.recv().await.expect("context dropped");
                }
            })
            .await
            .with_kind(embassy::ErrorKind::Network)?;
            Ok::<_, Error>(())
        })()
        .await
    } else {
        Ok(())
    };

    run_script_if_exists("/embassy-os/postinit.sh").await;

    res
}

fn main() {
    let matches = clap::App::new("embassyd")
        .arg(
            clap::Arg::with_name("config")
                .short("c")
                .long("config")
                .takes_value(true),
        )
        .arg(
            clap::Arg::with_name("verbosity")
                .short("v")
                .multiple(true)
                .takes_value(false),
        )
        .get_matches();

    simple_logging::log_to_stderr(match matches.occurrences_of("verbosity") {
        0 => log::LevelFilter::Off,
        1 => log::LevelFilter::Error,
        2 => log::LevelFilter::Warn,
        3 => log::LevelFilter::Info,
        4 => log::LevelFilter::Debug,
        _ => log::LevelFilter::Trace,
    });
    let cfg_path = matches.value_of("config");
    let res = {
        let rt = tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
            .expect("failed to initialize runtime");
        rt.block_on(inner_main(cfg_path))
    };

    match res {
        Ok(_) => (),
        Err(e) => {
            eprintln!("{}", e.source);
            log::debug!("{:?}", e.source);
            drop(e.source);
            std::process::exit(e.kind as i32)
        }
    }
}
