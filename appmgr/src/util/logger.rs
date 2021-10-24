use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::Arc;

use reqwest::{Client, Url};
use serde::Serialize;
use tracing::Subscriber;
use tracing_subscriber::Layer;

use crate::version::COMMIT_HASH;

pub struct SharingLayer {
    log_epoch: Arc<AtomicU64>,
    sharing: Arc<AtomicBool>,
    share_dest: String,
}
impl<S: Subscriber> Layer<S> for SharingLayer {
    fn on_event(
        &self,
        event: &tracing::Event<'_>,
        _ctx: tracing_subscriber::layer::Context<'_, S>,
    ) {
        if self.sharing.load(Ordering::SeqCst) {
            #[derive(Serialize)]
            #[serde(rename_all = "kebab-case")]
            struct LogRequest<'a> {
                log_epoch: u64,
                commit_hash: &'static str,
                file: Option<&'a str>,
                line: Option<u32>,
                target: &'a str,
                level: &'static str,
                message: Option<String>,
            }
            if event.metadata().level() <= &tracing::Level::WARN {
                let body = LogRequest {
                    log_epoch: self.log_epoch.load(Ordering::SeqCst),
                    commit_hash: COMMIT_HASH,
                    file: event.metadata().file(),
                    line: event.metadata().line(),
                    target: event.metadata().target(),
                    level: event.metadata().level().as_str(),
                    message: event
                        .fields()
                        .find(|f| f.name() == "message")
                        .map(|f| f.to_string()),
                };
                // we don't care about the result and need it to be fast
                tokio::spawn(Client::new().post(&self.share_dest).json(&body).send());
            }
        }
    }
}

#[derive(Clone)]
pub struct EmbassyLogger {
    log_epoch: Arc<AtomicU64>,
    sharing: Arc<AtomicBool>,
}
impl EmbassyLogger {
    fn base_subscriber() -> impl Subscriber {
        use tracing_error::ErrorLayer;
        use tracing_subscriber::prelude::*;
        use tracing_subscriber::{fmt, EnvFilter};

        let filter_layer = EnvFilter::from_default_env();
        let fmt_layer = fmt::layer().with_target(true);

        tracing_subscriber::registry()
            .with(filter_layer)
            .with(fmt_layer)
            .with(ErrorLayer::default())
    }
    pub fn no_sharing() {
        use tracing_subscriber::prelude::*;

        Self::base_subscriber().init();
        color_eyre::install().expect("Color Eyre Init");
    }
    pub fn init(log_epoch: Arc<AtomicU64>, share_dest: Option<Url>, share_errors: bool) -> Self {
        use tracing_subscriber::prelude::*;

        let share_dest = match share_dest {
            None => "https://beta-registry-0-3.start9labs.com/error-logs".to_owned(), // TODO
            Some(a) => a.to_string(),
        };
        let sharing = Arc::new(AtomicBool::new(share_errors));
        let sharing_layer = SharingLayer {
            log_epoch: log_epoch.clone(),
            share_dest,
            sharing: sharing.clone(),
        };

        Self::base_subscriber().with(sharing_layer).init();
        color_eyre::install().expect("Color Eyre Init");

        EmbassyLogger { log_epoch, sharing }
    }
    pub fn set_sharing(&self, sharing: bool) {
        self.sharing.store(sharing, Ordering::SeqCst)
    }
}

#[tokio::test]
pub async fn order_level() {
    assert!(tracing::Level::WARN > tracing::Level::ERROR)
}

#[test]
pub fn module() {
    println!("{}", module_path!())
}
