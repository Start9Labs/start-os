use std::ffi::OsStr;
use std::path::Path;

use color_eyre::eyre::eyre;
use tokio::process::Command;
use tracing::instrument;

use crate::{Error, ResultExt};

#[derive(Debug, Clone, Copy)]
pub struct RequiresReboot(pub bool);
impl std::ops::BitOrAssign for RequiresReboot {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0
    }
}

#[derive(Debug, Clone, Copy)]
pub enum RepairStrategy {
    Preen,
    Aggressive,
}
impl RepairStrategy {
    pub async fn e2fsck(
        &self,
        logicalname: impl AsRef<Path> + std::fmt::Debug,
    ) -> Result<RequiresReboot, Error> {
        match self {
            RepairStrategy::Preen => e2fsck_preen(logicalname).await,
            RepairStrategy::Aggressive => e2fsck_aggressive(logicalname).await,
        }
    }
}

#[instrument]
pub async fn e2fsck_preen(
    logicalname: impl AsRef<Path> + std::fmt::Debug,
) -> Result<RequiresReboot, Error> {
    e2fsck_runner(Command::new("e2fsck").arg("-p"), logicalname).await
}

#[instrument]
pub async fn e2fsck_aggressive(
    logicalname: impl AsRef<Path> + std::fmt::Debug,
) -> Result<RequiresReboot, Error> {
    e2fsck_runner(
        Command::new("e2fsck").arg("-y").arg("-z").arg(
            Path::new("/embassy-os")
                .join(
                    logicalname
                        .as_ref()
                        .file_name()
                        .unwrap_or(OsStr::new("unknown")),
                )
                .with_extension("e2undo"),
        ),
        logicalname,
    )
    .await
}

async fn e2fsck_runner(
    e2fsck_cmd: &mut Command,
    logicalname: impl AsRef<Path> + std::fmt::Debug,
) -> Result<RequiresReboot, Error> {
    let e2fsck_out = e2fsck_cmd.arg(logicalname.as_ref()).output().await?;
    let e2fsck_stderr = String::from_utf8(e2fsck_out.stderr)?;
    let code = e2fsck_out.status.code().ok_or_else(|| {
        Error::new(
            eyre!("e2fsck: process terminated by signal"),
            crate::ErrorKind::DiskManagement,
        )
    })?;
    if code & 4 != 0 {
        tracing::error!(
            "some filesystem errors NOT corrected on {}:\n{}",
            logicalname.as_ref().display(),
            e2fsck_stderr,
        );
    } else if code & 1 != 0 {
        tracing::warn!(
            "filesystem errors corrected on {}:\n{}",
            logicalname.as_ref().display(),
            e2fsck_stderr,
        );
    }
    if code < 8 {
        if code & 2 != 0 {
            tracing::warn!("reboot required");
            Ok(RequiresReboot(true))
        } else {
            Ok(RequiresReboot(false))
        }
    } else {
        Err(Error::new(
            eyre!("e2fsck: {}", e2fsck_stderr),
            crate::ErrorKind::DiskManagement,
        ))
    }
}
