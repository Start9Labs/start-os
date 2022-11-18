use std::collections::BTreeMap;
use std::ops::DerefMut;
use std::os::unix::process::ExitStatusExt;
use std::process::Stdio;
use std::sync::Arc;

use embassy_container_init::{
    KillGroupParams, OutputParams, ProcessGroupId, ProcessId, ReadLineStderrParams,
    ReadLineStdoutParams, RunCommandParams, SendSignalParams,
};
use futures::StreamExt;
use serde::{Deserialize, Serialize};
use serde_json::json;
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::process::{Child, ChildStderr, ChildStdout, Command};
use tokio::select;
use tokio::sync::Mutex;
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
    KillGroup,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "method", content = "params", rename_all = "kebab-case")]
enum Input {
    /// Run a new command, with the args
    Command(RunCommandParams),
    /// Get a line of stdout from the command
    ReadLineStdout(ReadLineStdoutParams),
    /// Get a line of stderr from the command
    ReadLineStderr(ReadLineStderrParams),
    /// Get output of command
    Output(OutputParams),
    /// Send the sigterm to the process
    Signal(SendSignalParams),
    /// Kill a group of processes
    KillGroup(KillGroupParams),
}

#[derive(Deserialize)]
struct IncomingRpc {
    id: Id,
    #[serde(flatten)]
    input: Input,
}

#[derive(Clone)]
struct Handler {
    children: Arc<
        Mutex<
            BTreeMap<
                ProcessId,
                (
                    Option<ProcessGroupId>,
                    Arc<
                        Mutex<
                            Option<(
                                Child,
                                Option<BufReader<ChildStdout>>,
                                Option<BufReader<ChildStderr>>,
                            )>,
                        >,
                    >,
                ),
            >,
        >,
    >,
}
impl Handler {
    fn new() -> Self {
        Handler {
            children: Arc::new(Mutex::new(BTreeMap::new())),
        }
    }
    async fn handle(&self, req: Input) -> Result<Output, RpcError> {
        Ok(match req {
            Input::Command(RunCommandParams { gid, command, args }) => {
                Output::Command(self.command(gid, command, args).await?)
            }
            Input::ReadLineStdout(ReadLineStdoutParams { pid }) => {
                Output::ReadLineStdout(self.read_line_stdout(pid).await?)
            }
            Input::ReadLineStderr(ReadLineStderrParams { pid }) => {
                Output::ReadLineStderr(self.read_line_stderr(pid).await?)
            }
            Input::Output(OutputParams { pid }) => Output::Output(self.output(pid).await?),
            Input::Signal(SendSignalParams { pid, signal }) => {
                self.signal(pid, signal).await?;
                Output::Signal
            }
            Input::KillGroup(KillGroupParams { gid }) => {
                self.kill_group(gid).await?;
                Output::KillGroup
            }
        })
    }

    async fn command(
        &self,
        gid: Option<ProcessGroupId>,
        command: String,
        args: Vec<String>,
    ) -> Result<ProcessId, RpcError> {
        let mut cmd = Command::new(command);
        cmd.args(args);
        cmd.kill_on_drop(true);
        cmd.stdout(Stdio::piped());
        cmd.stderr(Stdio::piped());
        let child = cmd.spawn().map_err(|e| {
            let mut err = yajrc::INTERNAL_ERROR.clone();
            err.data = Some(json!(e.to_string()));
            err
        })?;
        let pid = ProcessId(child.id().ok_or_else(|| {
            let mut err = yajrc::INTERNAL_ERROR.clone();
            err.data = Some(json!("Child has no pid"));
            err
        })?);
        self.children
            .lock()
            .await
            .insert(pid, (gid, Arc::new(Mutex::new(Some((child, None, None))))));
        Ok(pid)
    }

    async fn read_line_stdout(&self, pid: ProcessId) -> Result<String, RpcError> {
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
                .1
                .clone()
        }
        .lock_owned()
        .await;
        if let Some(mut child) = child.as_mut() {
            if let Some(stdout) = child.0.stdout.take() {
                child.1 = Some(BufReader::new(stdout));
            }
            if let Some(stdout) = child.1.as_mut() {
                let mut res = String::new();
                stdout.read_line(&mut res).await.map_err(|e| {
                    let mut err = yajrc::INTERNAL_ERROR.clone();
                    err.data = Some(json!(e.to_string()));
                    err
                })?;
                Ok(res)
            } else {
                let mut err = yajrc::INTERNAL_ERROR.clone();
                err.data = Some(json!("Child has no stdout handle"));
                Err(err)
            }
        } else {
            Err(not_found())
        }
    }

    async fn read_line_stderr(&self, pid: ProcessId) -> Result<String, RpcError> {
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
                .1
                .clone()
        }
        .lock_owned()
        .await;
        if let Some(mut child) = child.as_mut() {
            if let Some(stderr) = child.0.stderr.take() {
                child.2 = Some(BufReader::new(stderr));
            }
            if let Some(stderr) = child.2.as_mut() {
                let mut res = String::new();
                stderr.read_line(&mut res).await.map_err(|e| {
                    let mut err = yajrc::INTERNAL_ERROR.clone();
                    err.data = Some(json!(e.to_string()));
                    err
                })?;
                Ok(res)
            } else {
                let mut err = yajrc::INTERNAL_ERROR.clone();
                err.data = Some(json!("Child has no stderr handle"));
                Err(err)
            }
        } else {
            Err(not_found())
        }
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
                .remove(&pid)
                .ok_or_else(not_found)?
                .1
        }
        .lock_owned()
        .await;
        if let Some(child) = child.take() {
            let output = child.0.wait_with_output().await?;
            if output.status.success() {
                Ok(String::from_utf8(output.stdout).map_err(|_| yajrc::PARSE_ERROR)?)
            } else {
                Err(RpcError {
                    code: output
                        .status
                        .code()
                        .or_else(|| output.status.signal().map(|s| 128 + s))
                        .unwrap_or(0),
                    message: "Command failed".into(),
                    data: Some(json!(
                        String::from_utf8(output.stderr).map_err(|_| yajrc::PARSE_ERROR)?
                    )),
                })
            }
        } else {
            Err(not_found())
        }
    }

    async fn signal(&self, pid: ProcessId, signal: u32) -> Result<(), RpcError> {
        let not_found = || {
            let mut err = yajrc::INTERNAL_ERROR.clone();
            err.data = Some(json!(format!("Child with pid {} not found", pid.0)));
            err
        };
        let child = {
            self.children
                .lock()
                .await
                .get(&pid)
                .ok_or_else(not_found)?
                .1
                .clone()
        }
        .lock_owned()
        .await;
        if !child.is_some() {
            return Err(not_found());
        }

        let _ = Command::new("kill")
            .arg("-s")
            .arg(signal.to_string())
            .arg(pid.0.to_string())
            .output()
            .await;

        if signal == 9 {
            self.children
                .lock()
                .await
                .remove(&pid)
                .ok_or_else(not_found)?;
        }
        Ok(())
    }

    async fn kill_group(&self, gid: ProcessGroupId) -> Result<(), RpcError> {
        let mut to_kill = Vec::new();
        {
            let mut children_ref = self.children.lock().await;
            let children = std::mem::take(children_ref.deref_mut());
            for (pid, (child_gid, child)) in children {
                if child_gid == Some(gid) {
                    to_kill.push(pid);
                } else {
                    children_ref.insert(pid, (child_gid, child));
                }
            }
        }
        for pid in to_kill {
            let _ = Command::new("kill")
                .arg("-9")
                .arg(pid.0.to_string())
                .output()
                .await;
        }

        Ok(())
    }

    async fn graceful_exit(self) {
        let kill_all = futures::stream::iter(
            std::mem::take(self.children.lock().await.deref_mut()).into_iter(),
        )
        .for_each_concurrent(None, |(pid, child)| async move {
            let _ = Command::new("kill").arg(pid.0.to_string()).output().await;
            if let Some(child) = child.1.lock().await.take() {
                let _ = child.0.wait_with_output().await;
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

    let handler = Handler::new();
    let mut lines = BufReader::new(tokio::io::stdin()).lines();
    let handler_thread = async {
        while let Some(line) = lines.next_line().await? {
            let local_hdlr = handler.clone();
            tokio::spawn(async move {
                if let Err(e) = async {
                    let req = serde_json::from_str::<IncomingRpc>(&line)?;
                    match local_hdlr.handle(req.input).await {
                        Ok(output) => {
                            println!(
                                "{}",
                                json!({ "id": req.id, "jsonrpc": "2.0", "result": output })
                            )
                        }
                        Err(e) => {
                            println!("{}", json!({ "id": req.id, "jsonrpc": "2.0", "error": e }))
                        }
                    }
                    Ok::<_, serde_json::Error>(())
                }
                .await
                {
                    tracing::error!("Error parsing RPC request: {}", e);
                    tracing::debug!("{:?}", e);
                }
            });
        }
        Ok::<_, std::io::Error>(())
    };

    select! {
        res = handler_thread => {
            match res {
                Ok(()) => tracing::debug!("Done with inputs/outputs"),
                Err(e) => {
                    tracing::error!("Error reading RPC input: {}", e);
                    tracing::debug!("{:?}", e);
                }
            }
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
