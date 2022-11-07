use std::{collections::BTreeMap, process::Stdio, sync::Arc};

use async_stream::stream;
use futures::{pin_mut, Stream, StreamExt};
use tokio::{
    io::AsyncBufReadExt,
    process::Child,
    select,
    sync::{oneshot, Mutex},
};
use tokio::{io::BufReader, process::Command};
use tracing::instrument;

use embassy_container_init::{Input, InputJsonRpc, JsonRpc, Output, OutputJsonRpc, RpcId};

const MAX_COMMANDS: usize = 10;

enum DoneProgramStatus {
    Wait(Result<std::process::ExitStatus, std::io::Error>),
    Killed,
}
/// Created from the child and rpc, to prove that the cmd was the one who died
struct DoneProgram {
    id: RpcId,
    status: DoneProgramStatus,
}

/// Used to attach the running command with the rpc
struct ChildAndRpc {
    id: RpcId,
    child: Child,
}

impl ChildAndRpc {
    fn new(id: RpcId, mut command: tokio::process::Command) -> ::std::io::Result<Self> {
        Ok(Self {
            id,
            child: command.spawn()?,
        })
    }
    async fn wait(&mut self) -> DoneProgram {
        let status = DoneProgramStatus::Wait(self.child.wait().await);
        DoneProgram {
            id: self.id.clone(),
            status,
        }
    }
    async fn kill(mut self) -> DoneProgram {
        if let Err(err) = self.child.kill().await {
            let id = &self.id;
            tracing::error!("Error while trying to kill a process {id:?}");
            tracing::debug!("{err:?}");
        }
        DoneProgram {
            id: self.id.clone(),
            status: DoneProgramStatus::Killed,
        }
    }
}

/// Controlls the tracing + other io events
/// Can get the inputs from stdin
/// Can start a command from an intputrpc returning stream of outputs
/// Can output to stdout
#[derive(Debug, Clone)]
struct Io {
    commands: Arc<Mutex<BTreeMap<RpcId, oneshot::Sender<()>>>>,
    ids: Arc<Mutex<BTreeMap<RpcId, u32>>>,
}

impl Io {
    fn start() -> Self {
        use tracing_error::ErrorLayer;
        use tracing_subscriber::prelude::*;
        use tracing_subscriber::{fmt, EnvFilter};

        let filter_layer = EnvFilter::new("embassy_container_init=debug");
        let fmt_layer = fmt::layer().with_target(true);

        tracing_subscriber::registry()
            .with(filter_layer)
            .with(fmt_layer)
            .with(ErrorLayer::default())
            .init();
        color_eyre::install().unwrap();
        Self {
            commands: Default::default(),
            ids: Default::default(),
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
                    let mut child_and_rpc = match ChildAndRpc::new(id.clone(), cmd) {
                        Err(_e) => return,
                        Ok(a) => a,
                    };

                    if let Some(child_id) = child_and_rpc.child.id() {
                        io.ids.lock().await.insert(id.clone(), child_id);
                    }

                    let stdout = child_and_rpc.child
                        .stdout
                        .take()
                        .expect("child did not have a handle to stdout");
                    let stderr = child_and_rpc.child
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
                                waited = child_and_rpc
                                    .wait() => {
                                        io.clean_id(&waited).await;
                                        match &waited.status {
                                            DoneProgramStatus::Wait(Ok(st)) =>  return st.code(),
                                            DoneProgramStatus::Wait(Err(err)) => tracing::debug!("Child {id:?} got error: {err:?}"),
                                            DoneProgramStatus::Killed => tracing::debug!("Child {id:?} already killed?"),
                                        }

                                    },
                                _ = end_command_receiver => {
                                    let status = child_and_rpc.kill().await;
                                    io.clean_id(&status).await;
                                },
                            }
                            None
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
                    let code = spawned.await.ok().flatten();
                    yield JsonRpc::new(id, Output::Done(code));
                },
                Input::Kill() => {
                    io.trigger_end_command(id).await;
                }
                Input::Term() => {
                    io.term_by_rpc(&id).await;
                }
            }
        }
    }
    /// Used to get the string lines from the stdin
    fn inputs(&self) -> impl Stream<Item = String> {
        use std::io::BufRead;
        let (sender, receiver) = tokio::sync::mpsc::channel(100);
        tokio::task::spawn_blocking(move || {
            let stdin = std::io::stdin();
            for line in stdin.lock().lines().flatten() {
                tracing::trace!("Line = {}", line);
                sender.blocking_send(line).unwrap();
            }
        });
        tokio_stream::wrappers::ReceiverStream::new(receiver)
    }

    ///Convert a stream of string to stdout
    async fn output(&self, outputs: impl Stream<Item = String>) {
        pin_mut!(outputs);
        while let Some(output) = outputs.next().await {
            tracing::info!("{}", output);
            println!("{}", output);
        }
    }

    /// Helper for the command fn
    /// Part of a pair for the signal map, that indicates that we should kill the command
    async fn trigger_end_command(&self, id: RpcId) {
        if let Some(command) = self.commands.lock().await.remove(&id) {
            if command.send(()).is_err() {
                tracing::trace!("Command {id:?} could not be ended, possible error or was done");
            }
        }
    }

    /// Helper for the command fn
    /// Part of a pair for the signal map, that indicates that we should kill the command
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

    /// Used during cleaning up a procress
    async fn clean_id(
        &self,
        done_program: &DoneProgram,
    ) -> (Option<u32>, Option<oneshot::Sender<()>>) {
        (
            self.ids.lock().await.remove(&done_program.id),
            self.commands.lock().await.remove(&done_program.id),
        )
    }

    /// Given the rpcid, will try and term the running command
    async fn term_by_rpc(&self, rpc: &RpcId) {
        let output = match self.remove_cmd_id(rpc).await {
            Some(id) => {
                let mut cmd = tokio::process::Command::new("kill");
                cmd.arg(format!("{id}"));
                cmd.output().await
            }
            None => return,
        };
        match output {
            Ok(_) => (),
            Err(err) => {
                tracing::error!("Could not kill rpc {rpc:?}");
                tracing::debug!("{err}");
            }
        }
    }

    /// Used as a cleanup
    async fn term_all(self) {
        let ids: Vec<_> = self.ids.lock().await.keys().cloned().collect();
        for id in ids {
            self.term_by_rpc(&id).await;
        }
    }

    async fn remove_cmd_id(&self, rpc: &RpcId) -> Option<u32> {
        self.ids.lock().await.remove(rpc)
    }
}
#[tokio::main]
async fn main() {
    use futures::StreamExt;
    use tokio::signal::unix::{signal, SignalKind};
    let mut sigint = signal(SignalKind::interrupt()).unwrap();
    let mut sigterm = signal(SignalKind::terminate()).unwrap();
    let mut sigquit = signal(SignalKind::quit()).unwrap();
    let mut sighangup = signal(SignalKind::hangup()).unwrap();
    let io = Io::start();
    let outputs = io
        .inputs()
        .filter_map(|x| async move { InputJsonRpc::maybe_parse(&x) })
        .flat_map_unordered(MAX_COMMANDS, |x| io.command(x).boxed())
        .filter_map(|x| async move { x.maybe_serialize() });

    select! {
        _ = io.output(outputs) => {
            tracing::debug!("Done with inputs/outputs")
        },
        _ = sigint.recv() => {
            tracing::debug!("Sigint")
        },
        _ = sigterm.recv() => {
            tracing::debug!("Sig Term")
        },
        _ = sigquit.recv() => {
            tracing::debug!("Sigquit")
        },
        _ = sighangup.recv() => {
            tracing::debug!("Sighangup")
        }
    }
    io.term_all().await;
    ::std::process::exit(0);
}
