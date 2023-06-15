use std::net::{Ipv4Addr, Ipv6Addr, SocketAddr};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;

use embassy::context::rpc::RpcContextConfig;
use embassy::context::{DiagnosticContext, InstallContext, SetupContext};
use embassy::disk::fsck::RepairStrategy;
use embassy::disk::main::DEFAULT_PASSWORD;
use embassy::disk::REPAIR_DISK_PATH;
use embassy::init::STANDBY_MODE_PATH;
use embassy::net::web_server::WebServer;
use embassy::shutdown::Shutdown;
use embassy::sound::CHIME;
use embassy::util::logger::EmbassyLogger;
use embassy::util::Invoke;
use embassy::{Error, ErrorKind, ResultExt, OS_ARCH};
use tokio::process::Command;
use tracing::instrument;

#[instrument(skip_all)]
async fn setup_or_init(cfg_path: Option<PathBuf>) -> Result<(), Error> {
    Command::new("ln")
        .arg("-sf")
        .arg("/usr/lib/embassy/scripts/fake-apt")
        .arg("/usr/local/bin/apt")
        .invoke(crate::ErrorKind::OpenSsh)
        .await?;
    Command::new("ln")
        .arg("-sf")
        .arg("/usr/lib/embassy/scripts/fake-apt")
        .arg("/usr/local/bin/apt-get")
        .invoke(crate::ErrorKind::OpenSsh)
        .await?;
    Command::new("ln")
        .arg("-sf")
        .arg("/usr/lib/embassy/scripts/fake-apt")
        .arg("/usr/local/bin/aptitude")
        .invoke(crate::ErrorKind::OpenSsh)
        .await?;

    Command::new("make-ssl-cert")
        .arg("generate-default-snakeoil")
        .arg("--force-overwrite")
        .invoke(crate::ErrorKind::OpenSsl)
        .await?;

    if tokio::fs::metadata("/run/live/medium").await.is_ok() {
        Command::new("sed")
            .arg("-i")
            .arg("s/PasswordAuthentication no/PasswordAuthentication yes/g")
            .arg("/etc/ssh/sshd_config")
            .invoke(crate::ErrorKind::Filesystem)
            .await?;
        Command::new("systemctl")
            .arg("reload")
            .arg("ssh")
            .invoke(crate::ErrorKind::OpenSsh)
            .await?;

        let ctx = InstallContext::init(cfg_path).await?;

        let server = WebServer::install(
            [
                SocketAddr::new(Ipv4Addr::UNSPECIFIED.into(), 80),
                SocketAddr::new(Ipv6Addr::UNSPECIFIED.into(), 80),
            ],
            ctx.clone(),
        )
        .await?;

        tokio::time::sleep(Duration::from_secs(1)).await; // let the record state that I hate this
        CHIME.play().await?;

        ctx.shutdown
            .subscribe()
            .recv()
            .await
            .expect("context dropped");

        server.shutdown().await;

        Command::new("reboot")
            .invoke(embassy::ErrorKind::Unknown)
            .await?;
    } else if tokio::fs::metadata("/media/embassy/config/disk.guid")
        .await
        .is_err()
    {
        let ctx = SetupContext::init(cfg_path).await?;

        let server = WebServer::setup(
            [
                SocketAddr::new(Ipv4Addr::UNSPECIFIED.into(), 80),
                SocketAddr::new(Ipv6Addr::UNSPECIFIED.into(), 80),
            ],
            ctx.clone(),
        )
        .await?;

        tokio::time::sleep(Duration::from_secs(1)).await; // let the record state that I hate this
        CHIME.play().await?;
        ctx.shutdown
            .subscribe()
            .recv()
            .await
            .expect("context dropped");

        server.shutdown().await;

        tokio::task::yield_now().await;
        if let Err(e) = Command::new("killall")
            .arg("firefox-esr")
            .invoke(ErrorKind::NotFound)
            .await
        {
            tracing::error!("Failed to kill kiosk: {}", e);
            tracing::debug!("{:?}", e);
        }
    } else {
        let cfg = RpcContextConfig::load(cfg_path).await?;
        let guid_string = tokio::fs::read_to_string("/media/embassy/config/disk.guid") // unique identifier for volume group - keeps track of the disk that goes with your embassy
            .await?;
        let guid = guid_string.trim();
        let requires_reboot = embassy::disk::main::import(
            guid,
            cfg.datadir(),
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
                .with_ctx(|_| (embassy::ErrorKind::Filesystem, REPAIR_DISK_PATH))?;
        }
        if requires_reboot.0 {
            embassy::disk::main::export(guid, cfg.datadir()).await?;
            Command::new("reboot")
                .invoke(embassy::ErrorKind::Unknown)
                .await?;
        }
        tracing::info!("Loaded Disk");
        embassy::init::init(&cfg).await?;
    }

    Ok(())
}

async fn run_script_if_exists<P: AsRef<Path>>(path: P) {
    let script = path.as_ref();
    if script.exists() {
        match Command::new("/bin/bash").arg(script).spawn() {
            Ok(mut c) => {
                if let Err(e) = c.wait().await {
                    tracing::error!("Error Running {}: {}", script.display(), e);
                    tracing::debug!("{:?}", e);
                }
            }
            Err(e) => {
                tracing::error!("Error Running {}: {}", script.display(), e);
                tracing::debug!("{:?}", e);
            }
        }
    }
}

#[instrument(skip_all)]
async fn inner_main(cfg_path: Option<PathBuf>) -> Result<Option<Shutdown>, Error> {
    if OS_ARCH == "raspberrypi" && tokio::fs::metadata(STANDBY_MODE_PATH).await.is_ok() {
        tokio::fs::remove_file(STANDBY_MODE_PATH).await?;
        Command::new("sync").invoke(ErrorKind::Filesystem).await?;
        embassy::sound::SHUTDOWN.play().await?;
        futures::future::pending::<()>().await;
    }

    embassy::sound::BEP.play().await?;

    run_script_if_exists("/media/embassy/config/preinit.sh").await;

    let res = if let Err(e) = setup_or_init(cfg_path.clone()).await {
        async move {
            tracing::error!("{}", e.source);
            tracing::debug!("{}", e.source);
            embassy::sound::BEETHOVEN.play().await?;

            let ctx = DiagnosticContext::init(
                cfg_path,
                if tokio::fs::metadata("/media/embassy/config/disk.guid")
                    .await
                    .is_ok()
                {
                    Some(Arc::new(
                        tokio::fs::read_to_string("/media/embassy/config/disk.guid") // unique identifier for volume group - keeps track of the disk that goes with your embassy
                            .await?
                            .trim()
                            .to_owned(),
                    ))
                } else {
                    None
                },
                e,
            )
            .await?;

            let server = WebServer::diagnostic(
                [
                    SocketAddr::new(Ipv4Addr::UNSPECIFIED.into(), 80),
                    SocketAddr::new(Ipv6Addr::UNSPECIFIED.into(), 80),
                ],
                ctx.clone(),
            )
            .await?;

            let shutdown = ctx.shutdown.subscribe().recv().await.unwrap();

            server.shutdown().await;

            Ok(shutdown)
        }
        .await
    } else {
        Ok(None)
    };

    run_script_if_exists("/media/embassy/config/postinit.sh").await;

    res
}

fn main() {
    let matches = clap::App::new("embassy-init")
        .arg(
            clap::Arg::with_name("config")
                .short('c')
                .long("config")
                .takes_value(true),
        )
        .get_matches();

    EmbassyLogger::init();

    let cfg_path = matches.value_of("config").map(|p| Path::new(p).to_owned());
    let res = {
        let rt = tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
            .expect("failed to initialize runtime");
        rt.block_on(inner_main(cfg_path))
    };

    match res {
        Ok(Some(shutdown)) => shutdown.execute(),
        Ok(None) => (),
        Err(e) => {
            eprintln!("{}", e.source);
            tracing::debug!("{:?}", e.source);
            drop(e.source);
            std::process::exit(e.kind as i32)
        }
    }
}
