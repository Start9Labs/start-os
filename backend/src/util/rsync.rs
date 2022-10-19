use std::path::Path;

use helpers::NonDetachingJoinHandle;
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::process::{Child, Command};
use tokio::sync::watch;
use tokio_stream::wrappers::WatchStream;

use crate::util::io::ByteReplacementReader;
use crate::Error;

pub struct Rsync {
    pub command: Child,
    _progress_task: NonDetachingJoinHandle<Result<(), Error>>,
    pub progress: WatchStream<f64>,
}
impl Rsync {
    pub fn new(src: impl AsRef<Path>, dst: impl AsRef<Path>, delete: bool) -> Result<Self, Error> {
        let mut cmd = Command::new("rsync");
        if delete {
            cmd.arg("--delete");
        }
        let mut command = cmd
            .arg("-a")
            .arg("--no-devices")
            .arg("--info=progress2")
            .arg(src.as_ref())
            .arg(dst.as_ref())
            .kill_on_drop(true)
            .stdout(std::process::Stdio::piped())
            .spawn()?;
        let cmd_stdout = command.stdout.take().unwrap();
        let (send, recv) = watch::channel(0.0);
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
            progress: WatchStream::new(recv),
        })
    }
    // pub fn
}
