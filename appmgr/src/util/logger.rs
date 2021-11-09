use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

use lazy_static::lazy_static;
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
                log_epoch: String,
                commit_hash: &'static str,
                file: Option<&'a str>,
                line: Option<u32>,
                target: &'a str,
                level: &'static str,
                log_message: Option<String>,
            }
            if event.metadata().level() <= &tracing::Level::WARN {
                struct Visitor(Option<String>);
                impl tracing::field::Visit for Visitor {
                    fn record_str(&mut self, field: &tracing::field::Field, value: &str) {
                        if field.name() == "message" {
                            self.0 = Some(value.to_owned());
                        }
                    }
                    fn record_error(
                        &mut self,
                        field: &tracing::field::Field,
                        value: &(dyn std::error::Error + 'static),
                    ) {
                        if field.name() == "message" {
                            self.0 = Some(value.to_string());
                        }
                    }
                    fn record_debug(
                        &mut self,
                        field: &tracing::field::Field,
                        value: &dyn std::fmt::Debug,
                    ) {
                        if field.name() == "message" {
                            self.0 = Some(format!("{:?}", value));
                        }
                    }
                }
                let mut message = Visitor(None);
                event.record(&mut message);
                let body = LogRequest {
                    log_epoch: self.log_epoch.load(Ordering::SeqCst).to_string(),
                    commit_hash: COMMIT_HASH,
                    file: event.metadata().file(),
                    line: event.metadata().line(),
                    target: event.metadata().target(),
                    level: event.metadata().level().as_str(),
                    log_message: message.0,
                };
                // we don't care about the result and need it to be fast
                tokio::spawn(Client::new().post(&self.share_dest).json(&body).send());
            }
        }
    }
}

lazy_static! {
    static ref LOGGER: Mutex<Option<(Arc<AtomicU64>, Arc<AtomicBool>)>> = Mutex::new(None);
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
        Self::init(None, false);
    }
    pub fn init(share_dest: Option<Url>, share_errors: bool) -> Self {
        use tracing_subscriber::prelude::*;

        let mut guard = LOGGER.lock().unwrap();
        let (log_epoch, sharing) = if let Some((log_epoch, sharing)) = guard.take() {
            sharing.store(share_errors, Ordering::SeqCst);
            (log_epoch, sharing)
        } else {
            let log_epoch = Arc::new(AtomicU64::new(rand::random()));
            let sharing = Arc::new(AtomicBool::new(share_errors));
            let share_dest = match share_dest {
                None => "https://beta-registry-0-3.start9labs.com/error-logs".to_owned(), // TODO
                Some(a) => a.to_string(),
            };
            let sharing_layer = SharingLayer {
                log_epoch: log_epoch.clone(),
                share_dest,
                sharing: sharing.clone(),
            };

            Self::base_subscriber().with(sharing_layer).init();
            color_eyre::install().expect("Color Eyre Init");
            (log_epoch, sharing)
        };
        *guard = Some((log_epoch.clone(), sharing.clone()));

        EmbassyLogger { log_epoch, sharing }
    }
    pub fn epoch(&self) -> Arc<AtomicU64> {
        self.log_epoch.clone()
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
