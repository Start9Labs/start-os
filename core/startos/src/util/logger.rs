use std::fs::File;
use std::io::{self, Write};
use std::sync::{Arc, Mutex, MutexGuard};

use lazy_static::lazy_static;
use tracing::Subscriber;
use tracing_subscriber::fmt::MakeWriter;
use tracing_subscriber::util::SubscriberInitExt;

lazy_static! {
    pub static ref LOGGER: StartOSLogger = StartOSLogger::init();
}

#[derive(Clone)]
pub struct StartOSLogger {
    logfile: LogFile,
}

#[derive(Clone, Default)]
struct LogFile(Arc<Mutex<Option<File>>>);
impl<'a> MakeWriter<'a> for LogFile {
    type Writer = Box<dyn Write + 'a>;
    fn make_writer(&'a self) -> Self::Writer {
        let f = self.0.lock().unwrap();
        if f.is_some() {
            struct TeeWriter<'a>(MutexGuard<'a, Option<File>>);
            impl<'a> Write for TeeWriter<'a> {
                fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
                    let n = if let Some(f) = &mut *self.0 {
                        f.write(buf)?
                    } else {
                        buf.len()
                    };
                    io::stderr().write_all(&buf[..n])?;
                    Ok(n)
                }
                fn flush(&mut self) -> io::Result<()> {
                    if let Some(f) = &mut *self.0 {
                        f.flush()?;
                    }
                    Ok(())
                }
            }
            Box::new(TeeWriter(f))
        } else {
            drop(f);
            Box::new(io::stderr())
        }
    }
}

impl StartOSLogger {
    pub fn enable(&self) {}

    pub fn set_logfile(&self, logfile: Option<File>) {
        *self.logfile.0.lock().unwrap() = logfile;
    }

    fn base_subscriber(logfile: LogFile) -> impl Subscriber {
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
            .with_writer(logfile)
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
    fn init() -> Self {
        let logfile = LogFile::default();
        Self::base_subscriber(logfile.clone()).init();
        color_eyre::install().unwrap_or_else(|_| tracing::warn!("tracing too many times"));

        StartOSLogger { logfile }
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
