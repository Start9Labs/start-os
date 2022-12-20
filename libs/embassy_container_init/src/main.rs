use std::collections::BTreeMap;
use std::net::SocketAddr;
use std::ops::DerefMut;
use std::os::unix::process::ExitStatusExt;
use std::process::Stdio;
use std::sync::Arc;

use axum::{
    extract::{self, State},
    http::StatusCode,
    response::{IntoResponse, Response},
    routing::{ self, put, post},
    Router,
    handler::{Layered}
};
use embassy_container_init::{
    OutputParams, OutputStrategy, ProcessGroupId, ProcessId, ReadLineStderrParams,
    ReadLineStdoutParams, RunCommandParams, SendSignalParams, SignalGroupParams, PORT,
};
use futures::StreamExt;
use helpers::NonDetachingJoinHandle;
use nix::errno::Errno;
use nix::sys::signal::Signal;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::process::{Child, ChildStderr, ChildStdout, Command};
use tokio::select;
use tokio::sync::{watch, Mutex};
use yajrc::{Id, RpcError};

/// Outputs embedded in the JSONRpc output of the executable.
#[derive(Debug, Clone, Serialize)]
#[serde(untagged)]
enum Output {
    Command(ProcessId),
    ReadLineStdout(String),
    ReadLineStderr(String),
    Output(String),
    Signal,
    SignalGroup,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "method", content = "params", rename_all = "kebab-case")]
enum Input {
    /// Run a new command, with the args
    Command(RunCommandParams),
    // /// Get a line of stdout from the command
    // ReadLineStdout(ReadLineStdoutParams),
    // /// Get a line of stderr from the command
    // ReadLineStderr(ReadLineStderrParams),
    /// Get output of command
    Output(OutputParams),
    /// Send the sigterm to the process
    Signal(SendSignalParams),
    /// Signal a group of processes
    SignalGroup(SignalGroupParams),
}

#[derive(Deserialize)]
struct IncomingRpc {
    id: Id,
    #[serde(flatten)]
    input: Input,
}

struct ChildInfo {
    gid: Option<ProcessGroupId>,
    child: Arc<Mutex<Option<Child>>>,
    output: Option<InheritOutput>,
}

struct InheritOutput {
    _thread: NonDetachingJoinHandle<()>,
    stdout: watch::Receiver<String>,
    stderr: watch::Receiver<String>,
}

#[derive(Clone)]
struct Handler {
    children: Arc<Mutex<BTreeMap<ProcessId, ChildInfo>>>,
}
impl Handler {
    fn new() -> Self {
        Handler {
            children: Arc::new(Mutex::new(BTreeMap::new())),
        }
    }
    async fn handle(&self, req: Input) -> Result<Output, RpcError> {
        Ok(match req {
            Input::Command(RunCommandParams {
                gid,
                command,
                args,
                output,
            }) => Output::Command(self.command(gid, command, args, output).await?),
            // Input::ReadLineStdout(ReadLineStdoutParams { pid }) => {
            //     Output::ReadLineStdout(self.read_line_stdout(pid).await?)
            // }
            // Input::ReadLineStderr(ReadLineStderrParams { pid }) => {
            //     Output::ReadLineStderr(self.read_line_stderr(pid).await?)
            // }
            Input::Output(OutputParams { pid }) => Output::Output(self.output(pid).await?),
            Input::Signal(SendSignalParams { pid, signal }) => {
                self.signal(pid, signal).await?;
                Output::Signal
            }
            Input::SignalGroup(SignalGroupParams { gid, signal }) => {
                self.signal_group(gid, signal).await?;
                Output::SignalGroup
            }
        })
    }

    async fn command(
        &self,
        gid: Option<ProcessGroupId>,
        command: String,
        args: Vec<String>,
        output: OutputStrategy,
    ) -> Result<ProcessId, RpcError> {
        let mut cmd = Command::new(command);
        cmd.args(args);
        cmd.kill_on_drop(true);
        cmd.stdout(Stdio::piped());
        cmd.stderr(Stdio::piped());
        let mut child = cmd.spawn().map_err(|e| {
            let mut err = yajrc::INTERNAL_ERROR.clone();
            err.data = Some(json!(e.to_string()));
            err
        })?;
        let pid = ProcessId(child.id().ok_or_else(|| {
            let mut err = yajrc::INTERNAL_ERROR.clone();
            err.data = Some(json!("Child has no pid"));
            err
        })?);
        let output = match output {
            OutputStrategy::Inherit => {
                let (stdout_send, stdout) = watch::channel(String::new());
                let (stderr_send, stderr) = watch::channel(String::new());
                if let (Some(child_stdout), Some(child_stderr)) =
                    (child.stdout.take(), child.stderr.take())
                {
                    Some(InheritOutput {
                        _thread: tokio::spawn(async move {
                            tokio::join!(
                                async {
                                    if let Err(e) = async {
                                        let mut lines = BufReader::new(child_stdout).lines();
                                        while let Some(line) = lines.next_line().await? {
                                            tracing::info!("({}): {}", pid.0, line);
                                            let _ = stdout_send.send(line);
                                        }
                                        Ok::<_, std::io::Error>(())
                                    }
                                    .await
                                    {
                                        tracing::error!(
                                            "Error reading stdout of pid {}: {}",
                                            pid.0,
                                            e
                                        );
                                    }
                                },
                                async {
                                    if let Err(e) = async {
                                        let mut lines = BufReader::new(child_stderr).lines();
                                        while let Some(line) = lines.next_line().await? {
                                            tracing::warn!("({}): {}", pid.0, line);
                                            let _ = stderr_send.send(line);
                                        }
                                        Ok::<_, std::io::Error>(())
                                    }
                                    .await
                                    {
                                        tracing::error!(
                                            "Error reading stdout of pid {}: {}",
                                            pid.0,
                                            e
                                        );
                                    }
                                }
                            );
                        })
                        .into(),
                        stdout,
                        stderr,
                    })
                } else {
                    None
                }
            }
            OutputStrategy::Collect => None,
        };
        self.children.lock().await.insert(
            pid,
            ChildInfo {
                gid,
                child: Arc::new(Mutex::new(Some(child))),
                output,
            },
        );
        Ok(pid)
    }

    async fn output(&self, pid: ProcessId) -> Result<String, RpcError> {
        let not_found = || {
            let mut err = yajrc::INTERNAL_ERROR.clone();
            err.data = Some(json!(format!("Child with pid {} not found", pid.0)));
            err
        };
        let mut child = {
            self.children
                .lock()
                .await
                .get(&pid)
                .ok_or_else(not_found)?
                .child
                .clone()
        }
        .lock_owned()
        .await;
        let Some(child) = child.take() else {
           return  Err(not_found());
        };
        let output = child.wait_with_output().await?;
        if !output.status.success() {
            return Err(RpcError {
                code: output
                    .status
                    .code()
                    .or_else(|| output.status.signal().map(|s| 128 + s))
                    .unwrap_or(0),
                message: "Command failed".into(),
                data: Some(json!(String::from_utf8(if output.stderr.is_empty() {
                    output.stdout
                } else {
                    output.stderr
                })
                .map_err(|_| yajrc::PARSE_ERROR)?)),
            });
        }
        return Ok(String::from_utf8(output.stdout).map_err(|_| yajrc::PARSE_ERROR)?);
    }

    async fn signal(&self, pid: ProcessId, signal: u32) -> Result<(), RpcError> {
        let not_found = || {
            let mut err = yajrc::INTERNAL_ERROR.clone();
            err.data = Some(json!(format!("Child with pid {} not found", pid.0)));
            err
        };

        Self::killall(pid, Signal::try_from(signal as i32)?)?;

        if signal == 9 {
            self.children
                .lock()
                .await
                .remove(&pid)
                .ok_or_else(not_found)?;
        }
        Ok(())
    }

    async fn signal_group(&self, gid: ProcessGroupId, signal: u32) -> Result<(), RpcError> {
        let mut to_kill = Vec::new();
        {
            let mut children_ref = self.children.lock().await;
            let children = std::mem::take(children_ref.deref_mut());
            for (pid, child_info) in children {
                if child_info.gid == Some(gid) {
                    to_kill.push(pid);
                } else {
                    children_ref.insert(pid, child_info);
                }
            }
        }
        for pid in to_kill {
            tracing::info!("Killing pid {}", pid.0);
            Self::killall(pid, Signal::try_from(signal as i32)?)?;
        }

        Ok(())
    }

    fn killall(pid: ProcessId, signal: Signal) -> Result<(), RpcError> {
        for proc in procfs::process::all_processes()? {
            let stat = proc?.stat()?;
            if ProcessId::from(stat.ppid) == pid {
                Self::killall(stat.pid.into(), signal)?;
            }
        }
        if let Err(e) = nix::sys::signal::kill(pid.into(), Some(signal)) {
            if e != Errno::ESRCH {
                tracing::error!("Failed to kill pid {}: {}", pid.0, e);
            }
        }
        Ok(())
    }

    async fn graceful_exit(self) {
        let kill_all = futures::stream::iter(
            std::mem::take(self.children.lock().await.deref_mut()).into_iter(),
        )
        .for_each_concurrent(None, |(pid, child)| async move {
            let _ = Self::killall(pid, Signal::SIGTERM);
            if let Some(child) = child.child.lock().await.take() {
                let _ = child.wait_with_output().await;
            }
        });
        kill_all.await
    }
}
#[tokio::main]
async fn main() {
    use tokio::signal::unix::{signal, SignalKind};
    let mut sigint = signal(SignalKind::interrupt()).unwrap();
    let mut sigterm = signal(SignalKind::terminate()).unwrap();
    let mut sigquit = signal(SignalKind::quit()).unwrap();
    let mut sighangup = signal(SignalKind::hangup()).unwrap();

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

    let handler = Handler::new();
    let handler_thread = main_server(handler.clone());

    select! {
        res = handler_thread => {
            tracing::debug!("Server is done?")
        },
        _ = sigint.recv() => {
            tracing::debug!("SIGINT");
        },
        _ = sigterm.recv() => {
            tracing::debug!("SIGTERM");
        },
        _ = sigquit.recv() => {
            tracing::debug!("SIGQUIT");
        },
        _ = sighangup.recv() => {
            tracing::debug!("SIGHUP");
        }
    }
    handler.graceful_exit().await;
    ::std::process::exit(0)
}

async fn main_server(handler: Handler) {
    let app = Router::new()
        .route("/", post(main_route))
        .with_state(handler);

    let addr = SocketAddr::from(([0, 0, 0, 0], PORT));
    println!("listening on {}", addr);
    axum::Server::bind(&addr)
        .serve(app.into_make_service())
        .await
        .unwrap();
}

async fn main_route(
    State(handler): axum::extract::State<Handler>,
    extract::Json(payload): axum::extract::Json<IncomingRpc>,
) -> impl IntoResponse {
    let id = payload.id;
    axum::Json(match handler.handle(payload.input).await {
        Ok(output) => json!({ "id": id, "jsonrpc": "2.0", "result": output }),
        Err(err) => json!({ "id": id, "jsonrpc": "2.0", "error": err }),
    })
}
