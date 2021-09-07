use std::time::Duration;

use anyhow::anyhow;
use embassy::context::RpcContext;
use embassy::db::model::Database;
use embassy::db::subscribe;
use embassy::hostname::{get_hostname, get_id};
use embassy::middleware::auth::auth;
use embassy::middleware::cors::cors;
use embassy::net::tor::{os_key, tor_health_check};
use embassy::shutdown::Shutdown;
use embassy::status::{check_all, synchronize_all};
use embassy::util::daemon;
use embassy::{Error, ErrorKind, ResultExt};
use futures::{FutureExt, TryFutureExt};
use patch_db::json_ptr::JsonPointer;
use reqwest::{Client, Proxy};
use rpc_toolkit::hyper::{Body, Response, Server, StatusCode};
use rpc_toolkit::{rpc_server, Context};
use tokio::signal::unix::signal;

fn status_fn(_: i32) -> StatusCode {
    StatusCode::OK
}

fn err_to_500(e: Error) -> Response<Body> {
    log::error!("{}", e);
    Response::builder()
        .status(StatusCode::INTERNAL_SERVER_ERROR)
        .body(Body::empty())
        .unwrap()
}

async fn inner_main(cfg_path: Option<&str>) -> Result<Option<Shutdown>, Error> {
    let rpc_ctx = RpcContext::init(cfg_path).await?;
    let mut shutdown_recv = rpc_ctx.shutdown.subscribe();

    let sig_handler_ctx = rpc_ctx.clone();
    let sig_handler = tokio::spawn(async move {
        use tokio::signal::unix::SignalKind;
        futures::future::select_all(
            [
                SignalKind::interrupt(),
                SignalKind::quit(),
                SignalKind::terminate(),
            ]
            .iter()
            .map(|s| {
                async move {
                    signal(*s)
                        .expect(&format!("register {:?} handler", s))
                        .recv()
                        .await
                }
                .boxed()
            }),
        )
        .await;
        sig_handler_ctx
            .shutdown
            .send(None)
            .expect("send shutdown signal");
    });

    if !rpc_ctx.db.exists(&<JsonPointer>::default()).await? {
        rpc_ctx
            .db
            .put(
                &<JsonPointer>::default(),
                &Database::init(
                    get_id().await?,
                    &get_hostname().await?,
                    &os_key(&mut rpc_ctx.secret_store.acquire().await?).await?,
                ),
                None,
            )
            .await?;
    }
    let auth = auth(rpc_ctx.clone());
    let ctx = rpc_ctx.clone();
    let server = rpc_server!({
        command: embassy::main_api,
        context: ctx,
        status: status_fn,
        middleware: [
            cors,
            auth,
        ]
    })
    .with_graceful_shutdown({
        let mut shutdown = rpc_ctx.shutdown.subscribe();
        async move {
            shutdown.recv().await.expect("context dropped");
        }
    });

    let rev_cache_ctx = rpc_ctx.clone();
    let revision_cache_task = tokio::spawn(async move {
        let mut sub = rev_cache_ctx.db.subscribe();
        let mut shutdown = rev_cache_ctx.shutdown.subscribe();
        loop {
            let rev = match tokio::select! {
                a = sub.recv() => a,
                _ = shutdown.recv() => break,
            } {
                Ok(a) => a,
                Err(_) => {
                    rev_cache_ctx.revision_cache.write().await.truncate(0);
                    continue;
                }
            }; // TODO: handle falling behind
            let mut cache = rev_cache_ctx.revision_cache.write().await;
            cache.push_back(rev);
            if cache.len() > rev_cache_ctx.revision_cache_size {
                cache.pop_front();
            }
        }
    });

    // let tor_health_check_task =
    //     embassy::daemon::tor_health_check::tor_health_check_daemon(&rpc_ctx.net_controller.tor);
    let ws_ctx = rpc_ctx.clone();
    let ws_server = {
        let builder = Server::bind(&ws_ctx.bind_ws);

        let make_svc = ::rpc_toolkit::hyper::service::make_service_fn(move |_| {
            let ctx = ws_ctx.clone();
            async move {
                Ok::<_, ::rpc_toolkit::hyper::Error>(::rpc_toolkit::hyper::service::service_fn(
                    move |req| {
                        let ctx = ctx.clone();
                        async move {
                            match req.uri().path() {
                                "/db" => Ok(subscribe(ctx, req).await.unwrap_or_else(err_to_500)),
                                _ => Response::builder()
                                    .status(StatusCode::NOT_FOUND)
                                    .body(Body::empty()),
                            }
                        }
                    },
                ))
            }
        });
        builder.serve(make_svc)
    }
    .with_graceful_shutdown({
        let mut shutdown = rpc_ctx.shutdown.subscribe();
        async move {
            shutdown.recv().await.expect("context dropped");
        }
    });

    let status_ctx = rpc_ctx.clone();
    let status_daemon = daemon(
        move || {
            let ctx = status_ctx.clone();
            async move {
                if let Err(e) = synchronize_all(&ctx).await {
                    log::error!("Error in Status Sync daemon: {}", e);
                    log::debug!("{:?}", e);
                } else {
                    log::info!("Status Sync completed successfully");
                }
            }
        },
        Duration::from_millis(500),
        rpc_ctx.shutdown.subscribe(),
    );
    let health_ctx = rpc_ctx.clone();
    let health_daemon = daemon(
        move || {
            let ctx = health_ctx.clone();
            async move {
                if let Err(e) = check_all(&ctx).await {
                    log::error!("Error in Health Check daemon: {}", e);
                    log::debug!("{:?}", e);
                } else {
                    log::info!("Health Check completed successfully");
                }
            }
        },
        Duration::from_millis(500),
        rpc_ctx.shutdown.subscribe(),
    );
    let tor_health_ctx = rpc_ctx.clone();
    let tor_client = Client::builder()
        .proxy(
            Proxy::all(format!("socks5h://{}:{}", rpc_ctx.host(), rpc_ctx.port()))
                .with_kind(crate::ErrorKind::Network)?,
        )
        .build()
        .with_kind(crate::ErrorKind::Network)?;
    let tor_health_daemon = daemon(
        move || {
            let ctx = tor_health_ctx.clone();
            let client = tor_client.clone();
            async move { tor_health_check(&client, &ctx.net_controller.tor).await }
        },
        Duration::from_secs(300),
        rpc_ctx.shutdown.subscribe(),
    );

    // embassy::sound::MARIO_COIN.play().await?;

    futures::try_join!(
        server.map_err(|e| Error::new(e, ErrorKind::Network)),
        revision_cache_task.map_err(|e| Error::new(
            anyhow!("{}", e).context("Revision Cache daemon panicked!"),
            ErrorKind::Unknown
        )),
        ws_server.map_err(|e| Error::new(e, ErrorKind::Network)),
        status_daemon.map_err(|e| Error::new(
            e.context("Status Sync daemon panicked!"),
            ErrorKind::Unknown
        )),
        health_daemon.map_err(|e| Error::new(
            e.context("Health Check daemon panicked!"),
            ErrorKind::Unknown
        )),
        tor_health_daemon
            .map_err(|e| Error::new(e.context("Tor Health daemon panicked!"), ErrorKind::Unknown)),
    )?;

    rpc_ctx.managers.empty().await?;

    sig_handler.abort();

    Ok(shutdown_recv
        .recv()
        .await
        .with_kind(crate::ErrorKind::Unknown)?)
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
        Ok(None) => (),
        Ok(Some(s)) => s.execute(),
        Err(e) => {
            eprintln!("{}", e.source);
            log::debug!("{:?}", e.source);
            drop(e.source);
            std::process::exit(e.kind as i32)
        }
    }
}
