use embassy::context::rpc::RpcContextConfig;
use embassy::Error;

async fn init(cfg: &RpcContextConfig) -> Result<(), Error> {
    // mount disk
    if embassy::disk::main::importable().await? {
        embassy::disk::main::load("password").await?;
    } else {
        // embassy::setup::host_setup().await?;
    }
    embassy::disk::util::bind("/embassy-data/main/logs", "/var/log/journal", false).await?;
    embassy::ssh::sync_keys_from_db(todo!(), "/root/.ssh/authorized_keys").await?;
    todo!("sync wifi");
    embassy::hostname::sync_hostname().await?;

    Ok(())
}

async fn inner_main(cfg_path: Option<&str>) -> Result<(), Error> {
    if let Err(e) = init(&RpcContextConfig::load(cfg_path).await?).await {
        embassy::sound::BEETHOVEN.play().await?;
        log::error!("{}", e.source);
        log::debug!("{}", e.source)
    } else {
        embassy::sound::MARIO_COIN.play().await?
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
    let rt = tokio::runtime::Runtime::new().expect("failed to initialize runtime");
    match rt.block_on(inner_main(cfg_path)) {
        Ok(_) => (),
        Err(e) => {
            drop(rt);
            eprintln!("{}", e.source);
            log::debug!("{:?}", e.source);
            drop(e.source);
            std::process::exit(e.kind as i32)
        }
    }
}
