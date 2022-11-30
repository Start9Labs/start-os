use std::path::Path;

use color_eyre::eyre::eyre;
use futures::StreamExt;
use models::{Error, ErrorKind};
use tokio::io::{AsyncBufReadExt, AsyncReadExt, BufReader};
use tokio::process::{Child, Command};
use tokio::sync::watch;
use tokio_stream::wrappers::WatchStream;

use crate::{const_true, ByteReplacementReader, NonDetachingJoinHandle};

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
    pub async fn new(
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
            .arg("--no-inc-recursive")
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
                if let Some(percentage) = parse_percentage(&line) {
                    if percentage > 0.0 {
                        if let Err(err) = send.send(percentage / 100.0) {
                            return Err(Error::new(
                                eyre!("rsync progress send error: {}", err),
                                ErrorKind::Filesystem,
                            ));
                        }
                    }
                }
            }
            Ok(())
        })
        .into();
        let mut progress = WatchStream::new(recv);
        progress.next().await;
        Ok(Rsync {
            command,
            _progress_task: progress_task,
            stderr,
            progress,
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

fn parse_percentage(line: &str) -> Option<f64> {
    if let Some(percentage) = line
        .split_ascii_whitespace()
        .find_map(|col| col.strip_suffix("%"))
    {
        return percentage.parse().ok();
    }
    None
}

#[test]
fn test_parse() {
    let input = "          1.07G 57%   95.20MB/s    0:00:10 (xfr#1, to-chk=0/2)";
    assert_eq!(Some(57.0), parse_percentage(input));
}

#[tokio::test]
async fn test_rsync() {
    use futures::StreamExt;
    use tokio::fs;
    let mut seen_zero = false;
    let mut seen_in_between = false;
    let mut seen_hundred = false;
    fs::remove_dir_all("/tmp/test_rsync")
        .await
        .unwrap_or_default();
    fs::create_dir_all("/tmp/test_rsync/a").await.unwrap();
    fs::create_dir_all("/tmp/test_rsync/b").await.unwrap();
    for i in 0..100 {
        tokio::io::copy(
            &mut fs::File::open("/dev/urandom").await.unwrap().take(100_000),
            &mut fs::File::create(format!("/tmp/test_rsync/a/sample.{i}.bin"))
                .await
                .unwrap(),
        )
        .await
        .unwrap();
    }
    let mut rsync = Rsync::new(
        "/tmp/test_rsync/a/",
        "/tmp/test_rsync/b/",
        Default::default(),
    )
    .await
    .unwrap();
    while let Some(progress) = rsync.progress.next().await {
        if progress <= 0.05 {
            seen_zero = true;
        } else if progress > 0.05 && progress < 1.0 {
            seen_in_between = true
        } else {
            seen_hundred = true;
        }
    }
    rsync.wait().await.unwrap();
    assert!(seen_zero, "seen zero");
    assert!(seen_in_between, "seen in between 0 and 100");
    assert!(seen_hundred, "seen 100");
}
