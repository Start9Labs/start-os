use std::time::Duration;

use serde_json::json;

use crate::net::tor::TorController;

lazy_static::lazy_static! {
    static ref PROXY: reqwest::Proxy = reqwest::Proxy::http("socks5h://localhost:9050").expect("PROXY");
    static ref CLIENT: reqwest::Client = reqwest::Client::builder().proxy(PROXY.clone()).build().expect("CLIENT");
}

pub async fn tor_health_check_daemon(tor_controller: &TorController) {
    loop {
        // call out to tor address
        let onion = tor_controller.embassyd_onion().await;
        let result = CLIENT
            .post(format!("http://{}/rpc/v1", onion))
            .body(
                json!({
                    "jsonrpc": "2.0",
                    "method": "echo",
                    "params": { "message": "Follow the orange rabbit" },
                })
                .to_string()
                .into_bytes(),
            )
            .send()
            .await;
        match result {
            // if success, do nothing
            Ok(_) => {}
            // if failure, disconnect tor control port, and restart tor controller
            Err(e) => {
                log::error!("Unable to reach self over tor: {}", e);
                loop {
                    match tor_controller.replace().await {
                        Ok(restarted) => {
                            if restarted {
                                log::error!("Tor has been recently restarted, refusing to restart");
                            }
                            break;
                        }
                        Err(e) => {
                            log::error!("Unable to restart tor: {}", e);
                        }
                    }
                }
            }
        }
        tokio::time::sleep(Duration::from_secs(300)).await;
    }
}
