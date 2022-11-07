use color_eyre::eyre::eyre;
use std::path::Path;

use helpers::NonDetachingJoinHandle;
use tokio::io::{AsyncBufReadExt, AsyncReadExt, BufReader};
use tokio::process::{Child, Command};
use tokio::sync::watch;
use tokio_stream::wrappers::WatchStream;

use crate::util::io::ByteReplacementReader;
use crate::{Error, ErrorKind};

pub struct RsyncOptions {
    pub delete: bool,
    pub force: bool,
    pub ignore_existing: bool,
}
impl Default for RsyncOptions {
    fn default() -> Self {
        Self {
            delete: true,
            force: true,
            ignore_existing: false,
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
        let mut command = cmd
            .arg("-ac")
            .arg("--info=progress2")
            .arg(src.as_ref())
            .arg(dst.as_ref())
            .kill_on_drop(true)
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .spawn()?;
        let cmd_stdout = command.stdout.take().unwrap();
        let mut cmd_stderr = command.stderr.take().unwrap();
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
                    send.send(percentage.parse::<f64>()? / 100.0).unwrap();
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
        let stderr = self.stderr.await.unwrap()?;
        if status.success() {
            tracing::info!("rsync: {}", stderr);
        } else {
            return Err(Error::new(
                eyre!("rsync error: {}", stderr),
                ErrorKind::Filesystem,
            ));
        }
        Ok(())
    }
}
