use std::path::Path;

use async_compression::tokio::bufread::GzipDecoder;
use tokio::fs::File;
use tokio::io::{AsyncRead, BufReader};
use tokio::process::Command;

use crate::disk::fsck::RequiresReboot;
use crate::prelude::*;
use crate::util::Invoke;

pub async fn update_firmware() -> Result<RequiresReboot, Error> {
    let product_name = String::from_utf8(
        Command::new("dmidecode")
            .arg("-s")
            .arg("system-product-name")
            .invoke(ErrorKind::Firmware)
            .await?,
    )?
    .trim()
    .to_owned();
    if product_name.is_empty() {
        return Ok(RequiresReboot(false));
    }
    let firmware_dir = Path::new("/usr/lib/startos/firmware").join(&product_name);
    if tokio::fs::metadata(&firmware_dir).await.is_ok() {
        let current_firmware = String::from_utf8(
            Command::new("dmidecode")
                .arg("-s")
                .arg("bios-version")
                .invoke(ErrorKind::Firmware)
                .await?,
        )?
        .trim()
        .to_owned();
        if tokio::fs::metadata(firmware_dir.join(format!("{current_firmware}.rom.gz")))
            .await
            .is_err()
            && tokio::fs::metadata(firmware_dir.join(format!("{current_firmware}.rom")))
                .await
                .is_err()
        {
            let mut firmware_read_dir = tokio::fs::read_dir(&firmware_dir).await?;
            while let Some(entry) = firmware_read_dir.next_entry().await? {
                let filename = entry.file_name().to_string_lossy().into_owned();
                let rdr: Option<Box<dyn AsyncRead + Unpin + Send>> =
                    if filename.ends_with(".rom.gz") {
                        Some(Box::new(GzipDecoder::new(BufReader::new(
                            File::open(entry.path()).await?,
                        ))))
                    } else if filename.ends_with(".rom") {
                        Some(Box::new(File::open(entry.path()).await?))
                    } else {
                        None
                    };
                if let Some(mut rdr) = rdr {
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
        }
    }
    Ok(RequiresReboot(false))
}
