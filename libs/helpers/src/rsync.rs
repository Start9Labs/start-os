use color_eyre::eyre::eyre;
use std::path::Path;

use crate::{const_true, ByteReplacementReader, NonDetachingJoinHandle};
use models::{Error, ErrorKind};
use tokio::io::{AsyncBufReadExt, AsyncReadExt, BufReader};
use tokio::process::{Child, Command};
use tokio::sync::watch;
use tokio_stream::wrappers::WatchStream;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct RsyncOptions {
    #[serde(default = "const_true")]
    pub delete: bool,
    #[serde(default = "const_true")]
    pub force: bool,
    #[serde(default)]
    pub ignore_existing: bool,
    #[serde(default)]
    pub exclude: Vec<String>,
}
impl Default for RsyncOptions {
    fn default() -> Self {
        Self {
            delete: true,
            force: true,
            ignore_existing: false,
            exclude: Vec::new(),
        }
    }
}

pub struct Rsync {
    pub command: Child,
    _progress_task: NonDetachingJoinHandle<Result<(), Error>>,
    stderr: NonDetachingJoinHandle<Result<String, Error>>,
    pub progress: WatchStream<f64>,
}
impl Rsync {
    pub fn new(
        src: impl AsRef<Path>,
        dst: impl AsRef<Path>,
        options: RsyncOptions,
    ) -> Result<Self, Error> {
        let mut cmd = Command::new("rsync");
        if options.delete {
            cmd.arg("--delete");
        }
        if options.force {
            cmd.arg("--force");
        }
        if options.ignore_existing {
            cmd.arg("--ignore-existing");
        }
        for exclude in options.exclude {
            cmd.arg(format!("--exclude={}", exclude));
        }
        let mut command = cmd
            .arg("-acAXH")
            .arg("--info=progress2")
            .arg(src.as_ref())
            .arg(dst.as_ref())
            .kill_on_drop(true)
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .spawn()?;
        let cmd_stdout = match command.stdout.take() {
            None => {
                return Err(Error::new(
                    eyre!("rsync command stdout is none"),
                    ErrorKind::Filesystem,
                ))
            }
            Some(a) => a,
        };
        let mut cmd_stderr = match command.stderr.take() {
            None => {
                return Err(Error::new(
                    eyre!("rsync command stderr is none"),
                    ErrorKind::Filesystem,
                ))
            }
            Some(a) => a,
        };
        let (send, recv) = watch::channel(0.0);
        let stderr = tokio::spawn(async move {
            let mut res = String::new();
            cmd_stderr.read_to_string(&mut res).await?;
            Ok(res)
        })
        .into();
        let progress_task = tokio::spawn(async move {
            let mut lines = BufReader::new(ByteReplacementReader {
                replace: b'\r',
                with: b'\n',
                inner: cmd_stdout,
            })
            .lines();
            while let Some(line) = lines.next_line().await? {
                if let Some(percentage) = line
                    .split_ascii_whitespace()
                    .find_map(|col| col.strip_suffix("%"))
                {
                    if let Err(err) = send.send(percentage.parse::<f64>()? / 100.0) {
                        return Err(Error::new(
                            eyre!("rsync progress send error: {}", err),
                            ErrorKind::Filesystem,
                        ));
                    }
                }
            }
            Ok(())
        })
        .into();
        Ok(Rsync {
            command,
            _progress_task: progress_task,
            stderr,
            progress: WatchStream::new(recv),
        })
    }
    pub async fn wait(mut self) -> Result<(), Error> {
        let status = self.command.wait().await?;
        let stderr = match self.stderr.await {
            Err(err) => {
                return Err(Error::new(
                    eyre!("rsync stderr error: {}", err),
                    ErrorKind::Filesystem,
                ))
            }
            Ok(a) => a?,
        };
        if status.success() {
            tracing::info!("rsync: {}", stderr);
        } else {
            return Err(Error::new(
                eyre!(
                    "rsync error: {} {} ",
                    status.code().map(|x| x.to_string()).unwrap_or_default(),
                    stderr
                ),
                ErrorKind::Filesystem,
            ));
        }
        Ok(())
    }
}
