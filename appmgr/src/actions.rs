use std::os::unix::process::ExitStatusExt;
use std::process::Stdio;

use linear_map::set::LinearSet;
use tokio::io::{AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt, Error as IoError};
use yajrc::RpcError;

use crate::apps::DockerStatus;

pub const STATUS_NOT_ALLOWED: i32 = -2;
pub const INVALID_COMMAND: i32 = -3;

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct Action {
    pub id: String,
    pub name: String,
    pub description: String,
    #[serde(default)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub warning: Option<String>,
    pub allowed_statuses: LinearSet<DockerStatus>,
    pub command: Vec<String>,
}

async fn tee<R: AsyncRead + Unpin, W: AsyncWrite + Unpin>(
    mut r: R,
    mut w: W,
) -> Result<Vec<u8>, IoError> {
    let mut res = Vec::new();
    let mut buf = vec![0; 2048];
    let mut bytes;
    while {
        bytes = r.read(&mut buf).await?;
        bytes != 0
    } {
        res.extend_from_slice(&buf[..bytes]);
        w.write_all(&buf[..bytes]).await?;
    }
    w.flush().await?;
    Ok(res)
}

impl Action {
    pub async fn perform(&self, app_id: &str) -> Result<String, RpcError> {
        let man = crate::apps::manifest(app_id)
            .await
            .map_err(failure::Error::from)
            .map_err(failure::Error::compat)?;
        let status = crate::apps::status(app_id, true)
            .await
            .map_err(failure::Error::from)
            .map_err(failure::Error::compat)?
            .status;
        if !self.allowed_statuses.contains(&status) {
            return Err(RpcError {
                code: STATUS_NOT_ALLOWED,
                message: format!(
                    "{} is in status {:?} which is not allowed by {}",
                    app_id, status, self.id
                ),
                data: None,
            });
        }
        let mut cmd = if status == DockerStatus::Running {
            let mut cmd = tokio::process::Command::new("docker");
            cmd.arg("exec").arg(&app_id).args(&self.command);
            cmd
        } else {
            let mut cmd = tokio::process::Command::new("docker");
            let entrypoint = self.command.get(0).ok_or_else(|| RpcError {
                code: INVALID_COMMAND,
                message: "Command Cannot Be Empty".to_owned(),
                data: None,
            })?;
            cmd.arg("run")
                .arg("--rm")
                .arg("--name")
                .arg(format!("{}_{}", app_id, self.id))
                .arg("--mount")
                .arg(format!(
                    "type=bind,src={}/{},dst={}",
                    crate::VOLUMES,
                    app_id,
                    man.mount.display()
                ))
                .arg("--entrypoint")
                .arg(entrypoint)
                .arg(format!("start9/{}", app_id))
                .args(&self.command[1..]);
            // TODO: 0.3.0: net, tor, shm
            cmd
        };
        cmd.stdout(Stdio::piped());
        cmd.stderr(Stdio::piped());
        let mut child = cmd.spawn()?;

        let (stdout, stderr) = futures::try_join!(
            tee(child.stdout.take().unwrap(), tokio::io::sink()),
            tee(child.stderr.take().unwrap(), tokio::io::sink())
        )?;

        let status = child.wait().await?;
        if status.success() {
            String::from_utf8(stdout).map_err(From::from)
        } else {
            Err(RpcError {
                code: status
                    .code()
                    .unwrap_or_else(|| status.signal().unwrap_or(0) + 128),
                message: String::from_utf8(stderr)?,
                data: None,
            })
        }
    }
}
