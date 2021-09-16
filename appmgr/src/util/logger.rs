use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::Arc;

use log::{set_boxed_logger, set_max_level, LevelFilter, Metadata, Record};
use reqwest::{Client, Url};
use stderrlog::{StdErrLog, Timestamp};

#[derive(Clone)]
pub struct EmbassyLogger {
    log_level: log::LevelFilter,
    log_epoch: Arc<AtomicU64>,
    logger: StdErrLog,
    sharing: Arc<AtomicBool>,
    share_dest: Url,
}
impl EmbassyLogger {
    pub fn init(
        log_level: log::LevelFilter,
        log_epoch: Arc<AtomicU64>,
        share_dest: Option<Url>,
        share_errors: bool,
    ) -> Self {
        let share_dest = match share_dest {
            None => Url::parse("https://beta-registry-0-3.start9labs.com/error-logs").unwrap(), // TODO
            Some(a) => a,
        };
        let mut logger = stderrlog::new();
        logger.timestamp(Timestamp::Millisecond);
        match log_level {
            LevelFilter::Off => logger.quiet(true),
            LevelFilter::Error => logger.verbosity(0),
            LevelFilter::Warn => logger.verbosity(1),
            LevelFilter::Info => logger.verbosity(2),
            LevelFilter::Debug => logger.verbosity(3),
            LevelFilter::Trace => logger.verbosity(4),
        };
        let embassy_logger = EmbassyLogger {
            log_level,
            log_epoch,
            logger,
            sharing: Arc::new(AtomicBool::new(share_errors)),
            share_dest: share_dest,
        };
        set_boxed_logger(Box::new(embassy_logger.clone())).unwrap();
        set_max_level(log_level);
        embassy_logger
    }
    pub fn set_sharing(&self, sharing: bool) {
        self.sharing.store(sharing, Ordering::SeqCst)
    }
}

impl log::Log for EmbassyLogger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        self.logger.enabled(metadata)
    }
    fn log(&self, record: &Record) {
        self.logger.log(record);
        if self.sharing.load(Ordering::SeqCst) {
            if record.level() <= log::Level::Warn {
                let mut body = HashMap::new();
                body.insert(
                    "log-epoch",
                    format!("{}", self.log_epoch.load(Ordering::SeqCst)),
                );
                body.insert("log-message", format!("{}", record.args()));
                // we don't care about the result and need it to be fast
                tokio::spawn(
                    Client::new()
                        .post(self.share_dest.clone())
                        .json(&body)
                        .send(),
                );
            }
        }
    }
    fn flush(&self) {
        self.logger.flush()
    }
}

#[tokio::test]
pub async fn order_level() {
    assert!(log::Level::Warn > log::Level::Error)
}

#[test]
pub fn module() {
    println!("{}", module_path!())
}
