use std::{collections::BTreeMap, process::Stdio, sync::Arc};

use async_stream::stream;
use futures::{pin_mut, Stream, StreamExt};
use tokio::{
    io::AsyncBufReadExt,
    sync::{oneshot, Mutex},
};
use tokio::{io::BufReader, process::Command};
use tracing::instrument;

use embassy_container_init::{Input, InputJsonRpc, JsonRpc, Output, OutputJsonRpc, RpcId};

const MAX_COMMANDS: usize = 10;

#[derive(Debug, Clone)]
struct Io {
    commands: Arc<Mutex<BTreeMap<RpcId, oneshot::Sender<()>>>>,
}

impl Io {
    async fn trigger_end_command(&self, id: RpcId) {
        if let Some(command) = self.commands.lock().await.remove(&id) {
            if command.send(()).is_err() {
                tracing::trace!("Command {id:?} could not be ended, possible error or was done");
            }
        }
    }

    async fn create_end_command(&self, id: RpcId) -> oneshot::Receiver<()> {
        let (send, receiver) = oneshot::channel();
        if let Some(other_command) = self.commands.lock().await.insert(id.clone(), send) {
            if other_command.send(()).is_err() {
                tracing::trace!(
                    "Found other command {id:?} could not be ended, possible error or was done"
                );
            }
        }
        receiver
    }

    fn start() -> Self {
        use tracing_error::ErrorLayer;
        use tracing_subscriber::prelude::*;
        use tracing_subscriber::{fmt, EnvFilter};

        let filter_layer = EnvFilter::new("embassy_container_init=trace");
        let fmt_layer = fmt::layer().with_target(true);

        tracing_subscriber::registry()
            .with(filter_layer)
            .with(fmt_layer)
            .with(ErrorLayer::default())
            .init();
        color_eyre::install().unwrap();
        Self {
            commands: Default::default(),
        }
    }

    #[instrument]
    fn command(&self, input: InputJsonRpc) -> impl Stream<Item = OutputJsonRpc> {
        let io = self.clone();
        stream! {
            let (id, command) = input.into_pair();
            match command {
                Input::Command {
                            ref command,
                            ref args,
                } => {
                    let mut cmd = Command::new(command);
                    cmd.args(args);

                    cmd.stdout(Stdio::piped());
                    cmd.stderr(Stdio::piped());
                    let mut child = match cmd.spawn() {
                        Err(_e) => return,
                        Ok(a) => a,
                    };

                    let stdout = child
                        .stdout
                        .take()
                        .expect("child did not have a handle to stdout");
                    let stderr = child
                        .stderr
                        .take()
                        .expect("child did not have a handle to stderr");

                    let mut buff_out = BufReader::new(stdout).lines();
                    let mut buff_err = BufReader::new(stderr).lines();

                    let spawned = tokio::spawn({
                        let id = id.clone();
                        async move {
                            let end_command_receiver = io.create_end_command(id.clone()).await;
                            tokio::select!{
                                status = child
                                    .wait() => {
                                        if let Err(err) = status {
                                            tracing::debug!("Child {id:?} got error: {err:?}")
                                        }
                                    },
                                _ = end_command_receiver => {
                                    if let Err(err) = child.kill().await {
                                        tracing::error!("Error while trying to kill a process {id:?}");
                                        tracing::debug!("{err:?}");
                                    }
                                },

                            }
                        }

                    });
                    while let Ok(Some(line)) = buff_out.next_line().await {
                        let output = Output::Line(line);
                        let output = JsonRpc::new(id.clone(), output);
                        tracing::trace!("OutputJsonRpc {{ id, output_rpc }} = {:?}", output);
                        yield output;
                    }
                    while let Ok(Some(line)) = buff_err.next_line().await {
                        yield JsonRpc::new(id.clone(), Output::Error(line));
                    }
                    yield JsonRpc::new(id, Output::Done());
                    if let Err(e) = spawned.await {
                        tracing::error!("command join failed");
                        tracing::debug!("{:?}", e);
                    }
                },
                Input::Kill() => {
                    io.trigger_end_command(id).await;
                }
            }
        }
    }
    fn inputs(&self) -> impl Stream<Item = String> {
        use std::io::BufRead;
        let (sender, receiver) = tokio::sync::mpsc::channel(100);
        tokio::task::spawn_blocking(move || {
            let stdin = std::io::stdin();
            pin_mut!(stdin);
            for line in stdin.lock().lines().flatten() {
                tracing::trace!("Line = {}", line);
                sender.blocking_send(line).unwrap();
            }
        });
        tokio_stream::wrappers::ReceiverStream::new(receiver)
    }

    async fn output(&self, outputs: impl Stream<Item = String>) {
        pin_mut!(outputs);
        while let Some(output) = outputs.next().await {
            tracing::info!("{}", output);
            println!("{}", output);
        }
    }
}

#[tokio::main]
async fn main() {
    let io = Io::start();
    let outputs = io
        .inputs()
        .filter_map(|x| async move { InputJsonRpc::maybe_parse(&x) })
        .flat_map_unordered(MAX_COMMANDS, |x| io.command(x).boxed())
        .filter_map(|x| async move { x.maybe_serialize() });

    io.output(outputs).await;
}
