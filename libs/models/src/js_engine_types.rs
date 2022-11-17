use std::{future::Future, pin::Pin, sync::Arc, time::Duration};

use color_eyre::eyre::bail;
use embassy_container_init::{Input, Output, ProcessId, RpcId};
use tokio::sync::{
    mpsc::{UnboundedReceiver, UnboundedSender},
    Mutex,
};

/// Used by the js-executor, it is the ability to just create a command in an already running exec
pub type ExecCommand = Arc<
    dyn Fn(
            String,
            Vec<String>,
            UnboundedSender<embassy_container_init::Output>,
            Option<Duration>,
        ) -> Pin<Box<dyn Future<Output = Result<RpcId, String>> + 'static>>
        + Send
        + Sync
        + 'static,
>;

/// Used by the js-executor, it is the ability to just create a command in an already running exec
pub type SendKillSignal = Arc<
    dyn Fn(RpcId, u32) -> Pin<Box<dyn Future<Output = Result<(), String>> + 'static>>
        + Send
        + Sync
        + 'static,
>;

pub trait CommandInserter {
    fn insert_command(
        &self,
        command: String,
        args: Vec<String>,
        sender: UnboundedSender<embassy_container_init::Output>,
        timeout: Option<Duration>,
    ) -> Pin<Box<dyn Future<Output = Option<RpcId>>>>;

    fn send_signal(&self, id: RpcId, command: u32) -> Pin<Box<dyn Future<Output = ()>>>;
}

pub type ArcCommandInserter = Arc<Mutex<Option<Box<dyn CommandInserter>>>>;

pub struct ExecutingCommand {
    rpc_id: RpcId,
    /// Will exist until killed
    command_inserter: Arc<Mutex<Option<ArcCommandInserter>>>,
    owned_futures: Arc<Mutex<Vec<Pin<Box<dyn Future<Output = ()>>>>>>,
}

impl ExecutingCommand {
    pub async fn new(
        command_inserter: ArcCommandInserter,
        command: String,
        args: Vec<String>,
        timeout: Option<Duration>,
    ) -> Result<ExecutingCommand, color_eyre::Report> {
        let (sender, receiver) = tokio::sync::mpsc::unbounded_channel::<Output>();
        let rpc_id = {
            let locked_command_inserter = command_inserter.lock().await;
            let locked_command_inserter = match &*locked_command_inserter {
                Some(a) => a,
                None => bail!("Expecting containers.main in the package manifest".to_string()),
            };
            match locked_command_inserter
                .insert_command(command, args, sender, timeout)
                .await
            {
                Some(a) => a,
                None => bail!("Couldn't get command started ".to_string()),
            }
        };
        let executing_commands = ExecutingCommand {
            rpc_id,
            command_inserter: Arc::new(Mutex::new(Some(command_inserter.clone()))),
            owned_futures: Default::default(),
        };
        // let waiting = self.wait()
        Ok(executing_commands)
    }

    async fn wait(
        rpc_id: RpcId,
        mut outputs: UnboundedReceiver<Output>,
    ) -> Result<String, (Option<i32>, String)> {
        let (process_id_send, process_id_recv) = tokio::sync::oneshot::channel::<ProcessId>();
        let mut answer = String::new();
        let mut command_error = String::new();
        let mut status: Option<i32> = None;
        let mut process_id_send = Some(process_id_send);
        while let Some(output) = outputs.recv().await {
            match output {
                Output::ProcessId(process_id) => {
                    if let Some(process_id_send) = process_id_send.take() {
                        if let Err(err) = process_id_send.send(process_id) {
                            tracing::error!(
                                "Could not get a process id {process_id:?} sent for {rpc_id:?}"
                            );
                            tracing::debug!("{err:?}");
                        }
                    }
                }
                Output::Line(value) => {
                    answer.push_str(&value);
                    answer.push('\n');
                }
                Output::Error(error) => {
                    command_error.push_str(&error);
                    command_error.push('\n');
                }
                Output::Done(error_code) => {
                    status = error_code;
                    break;
                }
            }
        }
        if !command_error.is_empty() {
            return Err((status, command_error));
        }

        Ok(answer)
    }

    async fn send_signal(&self, signal: u32) {
        let locked = self.command_inserter.lock().await;
        let inner = match &*locked {
            Some(a) => a,
            None => return,
        };
        let locked = inner.lock().await;
        let command_inserter = match &*locked {
            Some(a) => a,
            None => return,
        };
        command_inserter.send_signal(self.rpc_id, signal);
    }
    /// Should only be called when output::done
    async fn killed(&self) {
        *self.owned_futures.lock().await = Default::default();
        *self.command_inserter.lock().await = Default::default();
    }
    pub fn rpc_id(&self) -> RpcId {
        self.rpc_id
    }
}

impl Drop for ExecutingCommand {
    fn drop(&mut self) {
        let command_inserter = self.command_inserter.clone();
        let rpc_id = self.rpc_id.clone();
        tokio::spawn(async move {
            let command_inserter_lock = command_inserter.lock().await;
            let command_inserter = match &*command_inserter_lock {
                Some(a) => a,
                None => {
                    return;
                }
            };
            command_inserter.send_kill_command(rpc_id, 9).await;
        });
    }
}
