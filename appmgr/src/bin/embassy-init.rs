use std::path::Path;
use std::sync::Arc;

use embassy::context::rpc::RpcContextConfig;
use embassy::context::{RecoveryContext, SetupContext};
use embassy::hostname::get_product_key;
use embassy::middleware::encrypt::encrypt;
use embassy::util::Invoke;
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
    if tokio::fs::metadata("/embassy-os/disk.guid").await.is_ok() {
        embassy::disk::main::load(
            &cfg,
            tokio::fs::read_to_string("/embassy-os/disk.guid")
                .await?
                .trim(),
            "password",
        )
        .await?;
        log::info!("Loaded Disk");
    } else {
        let ctx = SetupContext::init(cfg_path).await?;
        let encrypt = encrypt(Arc::new(get_product_key().await?));
        rpc_server!({
            command: embassy::setup_api,
            context: ctx.clone(),
            status: status_fn,
            middleware: [
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
    }

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
        .invoke(embassy::ErrorKind::Journald)
        .await?;
    embassy::disk::util::bind(&tmp_docker, "/var/lib/docker", false).await?;
    Command::new("systemctl")
        .arg("start")
        .arg("docker")
        .invoke(embassy::ErrorKind::Journald)
        .await?;
    log::info!("Mounted Docker Data");
    embassy::ssh::sync_keys_from_db(&secret_store, "/root/.ssh/authorized_keys").await?;
    log::info!("Synced SSH Keys");
    // todo!("sync wifi");
    embassy::hostname::sync_hostname().await?;
    log::info!("Synced Hostname");

    if tokio::fs::metadata("/var/www/html/public").await.is_err() {
        tokio::fs::create_dir_all("/var/www/html/public").await?
    }
    if tokio::fs::symlink_metadata("/var/www/html/public/package-data")
        .await
        .is_err()
    {
        tokio::fs::symlink(
            cfg.datadir().join("package-data").join("public"),
            "/var/www/html/public/package-data",
        )
        .await?;
    }
    log::info!("Enabled nginx public dir");

    Ok(())
}

// BLOCKING
fn run_script_if_exists<P: AsRef<Path>>(path: P) {
    use std::process::Command;

    let script = path.as_ref();
    if script.exists() {
        match Command::new("/bin/bash").arg(script).spawn() {
            Ok(mut c) => {
                if let Err(e) = c.wait() {
                    log::error!("Error Running {}: {}", script.display(), e)
                }
            }
            Err(e) => log::error!("Error Running {}: {}", script.display(), e),
        }
    }
}

async fn inner_main(cfg_path: Option<&str>) -> Result<(), Error> {
    if let Err(e) = init(cfg_path).await {
        log::error!("{}", e.source);
        log::debug!("{}", e.source);
        embassy::sound::BEETHOVEN.play().await?;
        let ctx = RecoveryContext::init(cfg_path).await?;
        rpc_server!({
            command: embassy::recovery_api,
            context: ctx.clone(),
            status: status_fn,
            middleware: [ ]
        })
        .with_graceful_shutdown({
            let mut shutdown = ctx.shutdown.subscribe();
            async move {
                shutdown.recv().await.expect("context dropped");
            }
        })
        .await
        .with_kind(embassy::ErrorKind::Network)?;
    }

    Ok(())
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

    run_script_if_exists("/embassy-os/preinit.sh");

    let res = {
        let rt = tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
            .expect("failed to initialize runtime");
        rt.block_on(inner_main(cfg_path))
    };

    run_script_if_exists("/embassy-os/postinit.sh");

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
