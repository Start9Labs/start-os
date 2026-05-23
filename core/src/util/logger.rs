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

/// Optional, hot-swappable on-disk log file shared between the appender
/// thread (which writes to it) and `LOGGER::set_logfile` (which swaps it).
///
/// The mutex here is **only contested between the appender thread and
/// callers of `set_logfile`** (which is invoked twice in the lifetime of
/// the process, both during `startd` init).  Tokio worker threads never
/// touch this mutex: they push bytes into the appender's mpsc and return
/// immediately.  See `TeeWriter` below.
#[derive(Clone, Default)]
struct SharedLogFile(Arc<Mutex<Option<File>>>);

/// A blocking `io::Write` that tees each event to an optional rotating
/// log file and to stderr.
///
/// This writer is owned by `tracing_appender::non_blocking`'s background
/// OS thread; it is **never** invoked from a tokio worker.  That is the
/// whole point of this rewrite: under the previous implementation, every
/// tracing event from every task synchronously took an
/// `Arc<Mutex<Option<File>>>` and performed a blocking write to journald
/// while holding it.  If journald ever fell behind (for any reason —
/// disk stall, restart, ratelimit, anything) the holding worker stalled
/// inside the kernel's socket-send path, all subsequent worker threads
/// piled onto the mutex futex, and `block_in_place` could not rescue
/// them because they hit the futex before reaching `block_in_place`.
/// The result was a wedged accept loop and an unresponsive UI.
///
/// With this writer behind `NonBlocking`, worker threads push bytes into
/// a bounded mpsc and return immediately.  If the appender thread itself
/// blocks on stderr, the channel fills, and events past the limit are
/// dropped (lossy mode).  No tokio worker ever sync-blocks on logger I/O.
struct TeeWriter {
    file: SharedLogFile,
}

impl Write for TeeWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        // File first — it is our diagnostic record and the more important
        // of the two sinks.  Errors are swallowed: if the file write
        // fails, we still want stderr to get the event, and we never want
        // to surface an error to the appender (it would be logged via the
        // same broken path).
        if let Ok(mut guard) = self.file.0.lock() {
            if let Some(f) = guard.as_mut() {
                let _ = f.write_all(buf);
            }
        }
        // Stderr second, also best-effort.  If stderr is wedged (the
        // exact condition this rewrite is designed to survive), the
        // appender thread will block here — but only the appender
        // thread; worker threads are unaffected.
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
    // Keeping the WorkerGuard alive keeps the appender's background OS
    // thread alive for the lifetime of the process.  Wrapped in
    // `Arc<Mutex<Option<_>>>` only so that `StartOSLogger: Clone` (the
    // existing public bound) keeps working; the guard itself is never
    // dropped or replaced after init.
    _guard: Arc<Mutex<Option<WorkerGuard>>>,
}

impl StartOSLogger {
    pub fn enable(&self) {}

    /// Swap the optional on-disk log file.  Bytes already queued in the
    /// appender's mpsc may still land in the previous file; bytes queued
    /// after this call land in the new file (or nowhere if `None`).
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
            .with_filter(filter_layer());

        let sub = tracing_subscriber::registry();

        #[cfg(feature = "console-subscriber")]
        let sub = sub.with(console_subscriber::spawn());

        let sub = sub.with(fmt_layer).with(ErrorLayer::default());

        sub
    }

    fn init() -> Self {
        let logfile = SharedLogFile::default();
        // Lossy under sustained backpressure.  We accept dropped events
        // in exchange for a guarantee that worker threads never wedge on
        // logger I/O.  Buffer is generous so transient bursts (the
        // migration's multi-megabyte debug events) don't drop anything.
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
