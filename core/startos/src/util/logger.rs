use std::io;

use tracing::Subscriber;
use tracing_subscriber::util::SubscriberInitExt;

#[derive(Clone)]
pub struct EmbassyLogger {}

impl EmbassyLogger {
    fn base_subscriber() -> impl Subscriber {
        use tracing_error::ErrorLayer;
        use tracing_subscriber::prelude::*;
        use tracing_subscriber::{fmt, EnvFilter};

        let filter_layer = EnvFilter::builder()
            .with_default_directive(
                format!("{}=info", std::module_path!().split("::").next().unwrap())
                    .parse()
                    .unwrap(),
            )
            .from_env_lossy();
        #[cfg(feature = "unstable")]
        let filter_layer = filter_layer
            .add_directive("tokio=trace".parse().unwrap())
            .add_directive("runtime=trace".parse().unwrap());
        let fmt_layer = fmt::layer()
            .with_writer(io::stderr)
            .with_line_number(true)
            .with_file(true)
            .with_target(true);

        let sub = tracing_subscriber::registry()
            .with(filter_layer)
            .with(fmt_layer)
            .with(ErrorLayer::default());

        #[cfg(feature = "unstable")]
        let sub = sub.with(console_subscriber::spawn());

        sub
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
