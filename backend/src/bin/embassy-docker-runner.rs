use std::{process::Stdio, str::FromStr};

use async_stream::stream;
use color_eyre::Report;
use futures::{pin_mut, Stream, StreamExt};
use serde::{ser::SerializeMap, Deserialize, Serialize, Serializer};
use tokio::io::AsyncBufReadExt;
use tokio::{io::BufReader, process::Command};
use tracing::instrument;

use embassy::docker_runner;

const MAX_COMMANDS: usize = 10;

#[derive(Debug, Copy, Clone)]
struct Io;

impl Io {
    fn start() -> Self {
        use tracing_error::ErrorLayer;
        use tracing_subscriber::prelude::*;
        use tracing_subscriber::{fmt, EnvFilter};

        let filter_layer = EnvFilter::new("embassy_docker_runner=trace");
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
    fn command(&self, input: InputRpcId) -> impl Stream<Item = OutputRpcId> {
        stream! {
            let id = &input.id;
            let input_rpc = &input.input_rpc;
            match input_rpc {
                InputRpc::Two(Input::Command {
                            ref command,
                            ref args,
                }) => {
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
                        tracing::trace!("OutputRpcId {{ id, output_rpc }} = {:?}",OutputRpcId { id: id.clone(), output: output.clone() });
                        yield OutputRpcId { id, output };
                    }
                    while let Ok(Some(line)) = buff_err.next_line().await {
                        yield OutputRpcId {
                                id: input.id.clone(),
                                output: Output::Error(line),
                            };
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
        .filter_map(|x| async move { InputRpcId::maybe_parse(&x) })
        .flat_map_unordered(MAX_COMMANDS, |x| io.command(x).boxed())
        .filter_map(|x| async move { x.maybe_serialize() });

    io.output(outputs).await;
}

// Test parse input example command
// Test io command
#[test]
fn example_echo_line() {
    let input = r#"{"id": "test", "jsonrpc": "2.0", "method":"command", "params": {"command": "echo", "args": ["world I am here"]}}"#;
    let new_input = InputRpcId::maybe_parse(input);
    assert!(new_input.is_some());
}

#[test]
fn example_input_line() {
    let output = OutputRpcId {
        id: Some(RpcId::String("test".to_string())),
        output: Output::Line("world I am here".to_string()),
    };
    let output_str = output.maybe_serialize();
    assert!(output_str.is_some());
    assert_eq!(
        &output_str.unwrap().to_string(),
        r#"{"jsonrpc":"2.0","id":"test","result":{"Line":"world I am here"}}"#
    );
}
