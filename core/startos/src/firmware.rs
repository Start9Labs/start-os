use std::collections::BTreeSet;
use std::path::Path;

use async_compression::tokio::bufread::GzipDecoder;
use clap::ArgMatches;
use rpc_toolkit::command;
use serde::{Deserialize, Serialize};
use tokio::fs::File;
use tokio::io::BufReader;
use tokio::process::Command;

use crate::disk::fsck::RequiresReboot;
use crate::prelude::*;
use crate::util::Invoke;
use crate::PLATFORM;

/// Part of the Firmware, look there for more about
#[derive(Clone, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct VersionMatcher {
    /// Strip this prefix on the version matcher
    semver_prefix: Option<String>,
    /// Match the semver to this range
    semver_range: Option<semver::VersionReq>,
    /// Strip this suffix on the version matcher
    semver_suffix: Option<String>,
}

/// Inside a file that is firmware.json, we
/// wanted a structure that could help decide what to do
/// for each of the firmware versions
#[derive(Clone, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct Firmware {
    id: String,
    /// This is the platform(s) the firmware was built for
    platform: BTreeSet<String>,
    /// This usally comes from the dmidecode
    system_product_name: Option<String>,
    /// The version comes from dmidecode, then we decide if it matches
    bios_version: Option<VersionMatcher>,
    /// the hash of the firmware rom.gz
    shasum: String,
}

fn display_firmware_update_result(arg: RequiresReboot, _: &ArgMatches) {
    if arg.0 {
        println!("Firmware successfully updated! Reboot to apply changes.");
    } else {
        println!("No firmware update available.");
    }
}

/// We wanted to make sure during every init
/// that the firmware was the correct and updated for
/// systems like the Pure System that a new firmware
/// was released and the updates where pushed through the pure os.
#[command(rename = "update-firmware", display(display_firmware_update_result))]
pub async fn update_firmware() -> Result<RequiresReboot, Error> {
    let system_product_name = String::from_utf8(
        Command::new("dmidecode")
            .arg("-s")
            .arg("system-product-name")
            .invoke(ErrorKind::Firmware)
            .await?,
    )?
    .trim()
    .to_owned();
    let bios_version = String::from_utf8(
        Command::new("dmidecode")
            .arg("-s")
            .arg("bios-version")
            .invoke(ErrorKind::Firmware)
            .await?,
    )?
    .trim()
    .to_owned();
    if system_product_name.is_empty() || bios_version.is_empty() {
        return Ok(RequiresReboot(false));
    }

    let firmware_dir = Path::new("/usr/lib/startos/firmware");

    for firmware in serde_json::from_str::<Vec<Firmware>>(
        &tokio::fs::read_to_string("/usr/lib/startos/firmware.json").await?,
    )
    .with_kind(ErrorKind::Deserialization)?
    {
        let id = firmware.id;
        let matches_product_name = firmware
            .system_product_name
            .map_or(true, |spn| spn == system_product_name);
        let matches_bios_version = firmware
            .bios_version
            .map_or(Some(true), |bv| {
                let mut semver_str = bios_version.as_str();
                if let Some(prefix) = &bv.semver_prefix {
                    semver_str = semver_str.strip_prefix(prefix)?;
                }
                if let Some(suffix) = &bv.semver_suffix {
                    semver_str = semver_str.strip_suffix(suffix)?;
                }
                let semver = semver_str
                    .split(".")
                    .filter_map(|v| v.parse().ok())
                    .chain(std::iter::repeat(0))
                    .take(3)
                    .collect::<Vec<_>>();
                let semver = semver::Version::new(semver[0], semver[1], semver[2]);
                Some(
                    bv.semver_range
                        .as_ref()
                        .map_or(true, |r| r.matches(&semver)),
                )
            })
            .unwrap_or(false);
        if firmware.platform.contains(&*PLATFORM) && matches_product_name && matches_bios_version {
            let filename = format!("{id}.rom.gz");
            let firmware_path = firmware_dir.join(&filename);
            Command::new("sha256sum")
                .arg("-c")
                .input(Some(&mut std::io::Cursor::new(format!(
                    "{} {}",
                    firmware.shasum,
                    firmware_path.display()
                ))))
                .invoke(ErrorKind::Filesystem)
                .await?;
            let mut rdr = if tokio::fs::metadata(&firmware_path).await.is_ok() {
                GzipDecoder::new(BufReader::new(File::open(&firmware_path).await?))
            } else {
                return Err(Error::new(
                    eyre!("Firmware {id}.rom.gz not found in {firmware_dir:?}"),
                    ErrorKind::NotFound,
                ));
            };
            Command::new("flashrom")
                .arg("-p")
                .arg("internal")
                .arg("-w-")
                .input(Some(&mut rdr))
                .invoke(ErrorKind::Firmware)
                .await?;
            return Ok(RequiresReboot(true));
        }
    }

    Ok(RequiresReboot(false))
}
