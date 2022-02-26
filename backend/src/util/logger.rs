use tracing::metadata::LevelFilter;
use tracing::Subscriber;
use tracing_subscriber::util::SubscriberInitExt;

#[derive(Clone)]
pub struct EmbassyLogger {}

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
    pub fn init() -> Self {
        Self::base_subscriber().init();
        color_eyre::install().unwrap_or_else(|_| tracing::warn!("tracing too many times"));

        EmbassyLogger {}
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
