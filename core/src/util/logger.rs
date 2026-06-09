use std::fs::File;
use std::io::{self, Write};
use std::sync::{Arc, Mutex};

use lazy_static::lazy_static;
use tracing::Subscriber;
use tracing_appender::non_blocking::{NonBlocking, NonBlockingBuilder, WorkerGuard};
use tracing_subscriber::util::SubscriberInitExt;

lazy_static! {
    pub static ref LOGGER: StartOSLogger = StartOSLogger::init();
}

/// Hot-swappable on-disk log file.  Only contested between the appender
/// thread and `set_logfile` callers; tokio workers never touch it.
#[derive(Clone, Default)]
struct SharedLogFile(Arc<Mutex<Option<File>>>);

/// Tees each event to the optional log file and to stderr.  Owned by the
/// `tracing_appender::non_blocking` background thread, never invoked from
/// a tokio worker.
struct TeeWriter {
    file: SharedLogFile,
}

impl Write for TeeWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        // Best-effort to both sinks. The appender thread may block here if
        // stderr is wedged, but worker threads cannot.
        if let Ok(mut guard) = self.file.0.lock() {
            if let Some(f) = guard.as_mut() {
                let _ = f.write_all(buf);
            }
        }
        let _ = io::stderr().write_all(buf);
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        if let Ok(mut guard) = self.file.0.lock() {
            if let Some(f) = guard.as_mut() {
                let _ = f.flush();
            }
        }
        let _ = io::stderr().flush();
        Ok(())
    }
}

#[derive(Clone)]
pub struct StartOSLogger {
    logfile: SharedLogFile,
    // Keeps the appender background thread alive for the process lifetime.
    _guard: Arc<Mutex<Option<WorkerGuard>>>,
}

impl StartOSLogger {
    pub fn enable(&self) {}

    pub fn set_logfile(&self, logfile: Option<File>) {
        *self.logfile.0.lock().unwrap() = logfile;
    }

    fn base_subscriber(writer: NonBlocking) -> impl Subscriber {
        use tracing_error::ErrorLayer;
        use tracing_subscriber::prelude::*;
        use tracing_subscriber::{EnvFilter, fmt};

        let filter_layer = || {
            EnvFilter::builder()
                .with_default_directive(
                    format!("{}=info", std::module_path!().split("::").next().unwrap())
                        .parse()
                        .unwrap(),
                )
                .from_env_lossy()
        };

        let fmt_layer = fmt::layer()
            .with_writer(writer)
            .with_line_number(true)
            .with_file(true)
            .with_target(true)
            // 0.3.20's ANSI-injection guard (tokio-rs/tracing#3368) escapes ESC
            // unconditionally, mangling our colored logs (#3369); opt out — these
            // are our own trusted logs. Toggle added in 0.3.23 (#3484).
            .with_ansi_sanitization(false)
            .with_filter(filter_layer());

        let sub = tracing_subscriber::registry();

        #[cfg(feature = "console-subscriber")]
        let sub = sub.with(console_subscriber::spawn());

        let sub = sub.with(fmt_layer).with(ErrorLayer::default());

        sub
    }

    fn init() -> Self {
        let logfile = SharedLogFile::default();
        // Lossy: drop events under sustained backpressure rather than ever
        // letting a worker thread sync-block on logger I/O.
        let (writer, guard) = NonBlockingBuilder::default()
            .lossy(true)
            .buffered_lines_limit(65_536)
            .thread_name("startos-logger".into())
            .finish(TeeWriter {
                file: logfile.clone(),
            });
        Self::base_subscriber(writer).init();
        color_eyre::install().unwrap_or_else(|_| tracing::warn!("tracing too many times"));

        StartOSLogger {
            logfile,
            _guard: Arc::new(Mutex::new(Some(guard))),
        }
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
