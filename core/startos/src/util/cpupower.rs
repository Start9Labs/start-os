use std::borrow::Cow;
use std::collections::BTreeSet;

use imbl::OrdMap;
use tokio::process::Command;

use crate::prelude::*;
use crate::util::Invoke;

pub const GOVERNOR_PERFORMANCE: Governor = Governor(Cow::Borrowed("performance"));

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Governor(Cow<'static, str>);
impl std::fmt::Display for Governor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}
impl std::ops::Deref for Governor {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
impl std::borrow::Borrow<str> for Governor {
    fn borrow(&self) -> &str {
        &**self
    }
}

pub async fn get_available_governors() -> Result<BTreeSet<Governor>, Error> {
    let raw = String::from_utf8(
        Command::new("cpupower")
            .arg("frequency-info")
            .arg("-g")
            .invoke(ErrorKind::CpuSettings)
            .await?,
    )?;
    let mut for_cpu: OrdMap<u32, BTreeSet<Governor>> = OrdMap::new();
    let mut current_cpu = None;
    for line in raw.lines() {
        if line.starts_with("analyzing") {
            current_cpu = Some(
                sscanf::sscanf!(line, "analyzing CPU {u32}:")
                    .map_err(|e| eyre!("{e}"))
                    .with_kind(ErrorKind::ParseSysInfo)?,
            );
        } else if let Some(rest) = line
            .trim()
            .strip_prefix("available cpufreq governors:")
            .map(|s| s.trim())
        {
            if rest != "Not Available" {
                for_cpu
                    .entry(current_cpu.ok_or_else(|| {
                        Error::new(
                            eyre!("governors listed before cpu"),
                            ErrorKind::ParseSysInfo,
                        )
                    })?)
                    .or_default()
                    .extend(
                        rest.split_ascii_whitespace()
                            .map(|g| Governor(Cow::Owned(g.to_owned()))),
                    );
            }
        }
    }
    Ok(for_cpu
        .into_iter()
        .fold(None, |acc: Option<BTreeSet<Governor>>, (_, x)| {
            if let Some(acc) = acc {
                Some(acc.intersection(&x).cloned().collect())
            } else {
                Some(x)
            }
        })
        .unwrap_or_default()) // include only governors available for ALL cpus
}

pub async fn current_governor() -> Result<Option<Governor>, Error> {
    let Some(raw) = Command::new("cpupower")
        .arg("frequency-info")
        .arg("-p")
        .invoke(ErrorKind::CpuSettings)
        .await
        .and_then(|s| Ok(Some(String::from_utf8(s)?)))
        .or_else(|e| {
            if e.source
                .to_string()
                .contains("Unable to determine current policy")
            {
                Ok(None)
            } else {
                Err(e)
            }
        })?
    else {
        return Ok(None);
    };

    for line in raw.lines() {
        if let Some(governor) = line
            .trim()
            .strip_prefix("The governor \"")
            .and_then(|s| s.strip_suffix("\" may decide which speed to use"))
        {
            return Ok(Some(Governor(Cow::Owned(governor.to_owned()))));
        }
    }
    Err(Error::new(
        eyre!("Failed to parse cpupower output:\n{raw}"),
        ErrorKind::ParseSysInfo,
    ))
}

pub async fn set_governor(governor: &Governor) -> Result<(), Error> {
    Command::new("cpupower")
        .arg("frequency-set")
        .arg("-g")
        .arg(&*governor.0)
        .invoke(ErrorKind::CpuSettings)
        .await?;
    Ok(())
}
