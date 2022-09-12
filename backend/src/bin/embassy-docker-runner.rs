
use std::{collections::BTreeMap, str::FromStr, process::Stdio};

use async_stream::stream;
use color_eyre::{Report, eyre::eyre};
use futures::{Stream, StreamExt, TryStreamExt, pin_mut};
use serde_json::Value;
use serde::{Serialize, Deserialize};
use tokio::{process::Command, io::BufReader};
use tracing::instrument;use tokio::io::AsyncBufReadExt;

const MAX_COMMANDS:usize = 10;


#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(untagged)]
enum RpcId {
    Int(i64),
    Float(f64),
    String(String),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
struct OutputRpcId {
    id: Option<RpcId>,
    #[serde(flatten)]
    output_rpc: OutputRpc,
}
impl OutputRpcId {
    #[instrument]
    fn maybe_serialize(&self) -> Option<String> {
        match serde_json::to_string(self) {
            Ok(x) => Some(x),
            Err(e) => {
                tracing::warn!("Could not stringify and skipping");
                tracing::debug!("{:?}", e);
                None
            },
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(tag = "jsonrpc", rename_all="camelCase")]
enum OutputRpc {    
    #[serde(rename = "2.0")]
    Two(Output),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(tag = "method", rename_all="camelCase")]
enum Output {
    Line(String),
    Error(String)
}



#[derive(Debug, Serialize, Deserialize, Clone)]
struct InputRpcId {
    id: Option<RpcId>,
    #[serde(flatten)]
    input_rpc: InputRpc,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(tag = "jsonrpc", rename_all="camelCase")]
enum InputRpc {    
    #[serde(rename = "2.0")]
    Two(Input),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(tag = "method", rename_all="camelCase")]
enum Input {
    Command{
        command: String,
        args: Vec<String>
    }
}

impl FromStr for InputRpcId {
    type Err = Report;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let input: Self = serde_json::from_str(s)?;
        Ok(input)
    }
}
impl InputRpcId {
    fn maybe_parse(s: &str) -> Option<Self> {
        match serde_json::from_str::<Self>(s) {
            Ok(a) => Some(a),
            Err(e) => {
                tracing::warn!("Could not parse and skipping: {}", s);
                tracing::debug!("{:?}", e);
                None
            },
        }
    }
}

#[derive(Debug, Copy, Clone)]
struct Io;

impl Io {
    #[instrument]
    fn command(&self, InputRpcId {id, input_rpc }: InputRpcId) -> impl Stream<Item = OutputRpcId>+ Unpin {
        stream! {
    
            match input_rpc {
                InputRpc::Two(Input::Command {
                    command, args
                }) => {
                    let mut cmd = Command::new(command).args(args);

                    cmd.stdout(Stdio::piped());
                    cmd.stderr(Stdio::piped());
                    let mut child = match cmd.spawn() {
                        Err(e) => return,
                        Ok(a) => a,
                        };
        
                    let stdout = child.stdout.take()
                        .expect("child did not have a handle to stdout");
                    let stderr = child.stderr.take()
                        .expect("child did not have a handle to stdout");
        
                    let mut buff_out = BufReader::new(stdout).lines();
                    let mut buff_err = BufReader::new(stderr).lines();
        
                    let spawned = tokio::spawn(async move {
                        let status = child.wait().await
                            .expect("child process encountered an error");
        
                        println!("child status was: {}", status);
                    });
                    while let Ok(Some(line)) = buff_out.next_line().await {
                        yield OutputRpcId {
                            id: id.clone(),
                            output_rpc: OutputRpc::Two(Output::Line(line))
                        };
                    }
                    while let Ok(Some(line)) = buff_err.next_line().await {
                        yield OutputRpcId {
                            id: id.clone(),
                            output_rpc: OutputRpc::Two(Output::Error(line))
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
        stream! {
            let stdin = std::io::stdin();
            for line in stdin.lock().lines() {
                yield line;
            }
        }
    }

    async fn output(&self, outputs: impl Stream<Item = String>) {
        let mut lock = std::io::stdout().lock();
        pin_mut!(outputs);
        while let Some(output) = outputs.next().await {
            writeln!(lock, "{}", output);
        }
    }
}

#[tokio::main]
async fn main() {
    let io = Io;
    let outputs = io.inputs()
        .filter_map(|x| async move {InputRpcId::maybe_parse(&x)}).flat_map_unordered(MAX_COMMANDS, |x| async move{
        io.command(x).boxed()
}).
filter_map(|x| async move {x.maybe_serialize()}).boxed();

    
    io.output(outputs).await;
    
}

// Test parse input example command
// Test io command

