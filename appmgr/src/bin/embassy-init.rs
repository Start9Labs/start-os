use std::path::Path;

use embassy::context::rpc::RpcContextConfig;
use embassy::context::{RecoveryContext, SetupContext};
use embassy::{Error, ResultExt};
use http::StatusCode;
use rpc_toolkit::rpc_server;

fn status_fn(_: i32) -> StatusCode {
    StatusCode::OK
}

async fn init(cfg_path: Option<&str>) -> Result<(), Error> {
    let cfg = RpcContextConfig::load(cfg_path).await?;
    if tokio::fs::metadata("/boot/embassy-os/disk.guid")
        .await
        .is_ok()
    {
        embassy::disk::main::load(
            &cfg,
            tokio::fs::read_to_string("/boot/embassy-os/disk.guid")
                .await?
                .trim(),
            "password",
        )
        .await?;
    } else {
        let ctx = SetupContext::init(cfg_path).await?;
        rpc_server!({
            command: embassy::setup_api,
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

    embassy::disk::util::bind(
        cfg.datadir().join("main").join("logs"),
        "/var/log/journal",
        false,
    )
    .await?;
    // cp -r "/var/lib/docker", "/tmp/docker-data"
    embassy::disk::util::bind(
        "/tmp/docker-data",
        "/var/lib/journal",
        false,
    )
    .await?;
    embassy::ssh::sync_keys_from_db(todo!(), "/root/.ssh/authorized_keys").await?;
    todo!("sync wifi");
    embassy::hostname::sync_hostname().await?;

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
        embassy::sound::BEETHOVEN.play().await?;
        log::error!("{}", e.source);
        log::debug!("{}", e.source);
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

    run_script_if_exists("/boot/embassy-os/preinit.sh");

    let res = {
        let rt = tokio::runtime::Runtime::new().expect("failed to initialize runtime");
        rt.block_on(inner_main(cfg_path))
    };

    run_script_if_exists("/boot/embassy-os/postinit.sh");

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
