use axum::routing::post;
use axum::Router;
use color_eyre::eyre::Error;
use rpc_toolkit::Server;
use startwrt_ctrl::{init_logging, main_api, ServerContext};
use std::future::ready;
use std::net::SocketAddr;
use std::time::Duration;
use tracing::instrument;

#[instrument(skip_all)]
async fn inner_main() -> Result<(), Error> {
    let ctx = ServerContext;
    let handler = Server::new(move || ready(Ok(ctx.clone())), main_api()).for_http();
    let addr = SocketAddr::from(([127, 0, 0, 1], 3301));
    let app = Router::new().route("/", post(handler));
    println!("listening on {}", addr);
    axum_server::bind(addr)
        .serve(app.into_make_service())
        .await?;
    Ok(())
}

pub fn main() {
    let _guard = init_logging("startwrt-ctrld");

    let res = {
        let rt = tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
            .expect("failed to initialize runtime");
        let res = rt.block_on(inner_main());
        rt.shutdown_timeout(Duration::from_secs(60));
        res
    };

    match res {
        Ok(_) => (),
        Err(e) => {
            eprintln!("{}", e);
            tracing::debug!("{:?}", e);
            std::process::exit(1)
        }
    }
}
