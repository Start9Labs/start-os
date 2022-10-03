use std::process::Stdio;

use async_stream::stream;
use futures::{pin_mut, Stream, StreamExt};
use tokio::io::AsyncBufReadExt;
use tokio::{io::BufReader, process::Command};
use tracing::instrument;

use embassy_container_init::{Input, InputJsonRpc, JsonRpc, Output, OutputJsonRpc};

const MAX_COMMANDS: usize = 10;

#[derive(Debug, Copy, Clone)]
struct Io;

impl Io {
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
        Self
    }

    #[instrument]
    fn command(&self, input: InputJsonRpc) -> impl Stream<Item = OutputJsonRpc> {
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

                    let spawned = tokio::spawn(async move {
                        let status = child
                            .wait()
                            .await
                            .expect("child process encountered an error");

                        tracing::trace!("child status was: {}", status);
                    });
                    while let Ok(Some(line)) = buff_out.next_line().await {
                        let id = id.clone();
                        let output = Output::Line(line);
                        let output = JsonRpc::new(id, output);
                        tracing::trace!("OutputJsonRpc {{ id, output_rpc }} = {:?}", output);
                        yield output;
                    }
                    while let Ok(Some(line)) = buff_err.next_line().await {
                        yield JsonRpc::new(id.clone(), Output::Error(line));
                    }
                    if let Err(e) = spawned.await {
                        tracing::error!("command join failed");
                        tracing::debug!("{:?}", e);
                    }
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
                // yield line;
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
    tracing::debug!("Debuggin!");
    let outputs = io
        .inputs()
        .filter_map(|x| async move { InputJsonRpc::maybe_parse(&x) })
        .flat_map_unordered(MAX_COMMANDS, |x| io.command(x).boxed())
        .filter_map(|x| async move { x.maybe_serialize() });

    io.output(outputs).await;
}
