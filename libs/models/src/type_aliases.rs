use std::{future::Future, pin::Pin, sync::Arc, time::Duration};

use tokio::sync::mpsc::UnboundedSender;

/// Used by the js-executor, it is the ability to just create a command in an already running exec
pub type ExecCommand = Arc<
    dyn Fn(
            String,
            Vec<String>,
            UnboundedSender<embassy_container_init::Output>,
            Option<Duration>,
        ) -> Pin<Box<dyn Future<Output = Result<(), String>> + 'static>>
        + Send
        + Sync
        + 'static,
>;
