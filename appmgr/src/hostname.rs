use digest::Digest;
use tokio::process::Command;
use tracing::instrument;

use crate::util::Invoke;
use crate::{Error, ErrorKind, ResultExt};

pub const PRODUCT_KEY_PATH: &'static str = "/embassy-os/product_key.txt";

#[instrument]
pub async fn get_hostname() -> Result<String, Error> {
    Ok(format!("embassy-{}", get_id().await?))
}

#[instrument]
pub async fn get_current_hostname() -> Result<String, Error> {
    let out = Command::new("hostname")
        .invoke(ErrorKind::ParseSysInfo)
        .await?;
    let out_string = String::from_utf8(out)?;
    Ok(out_string.trim().to_owned())
}

#[instrument]
pub async fn set_hostname(hostname: &str) -> Result<(), Error> {
    let _out = Command::new("hostnamectl")
        .arg("set-hostname")
        .arg(hostname)
        .invoke(ErrorKind::ParseSysInfo)
        .await?;
    Ok(())
}

#[instrument]
pub async fn get_product_key() -> Result<String, Error> {
    let out = tokio::fs::read_to_string(PRODUCT_KEY_PATH)
        .await
        .with_ctx(|_| (crate::ErrorKind::Filesystem, PRODUCT_KEY_PATH))?;
    Ok(out.trim().to_owned())
}

#[instrument]
pub async fn get_id() -> Result<String, Error> {
    let key = get_product_key().await?;
    let mut hasher = sha2::Sha256::new();
    hasher.update(key.as_bytes());
    let res = hasher.finalize();
    Ok(hex::encode(&res[0..4]))
}

// cat /embassy-os/product_key.txt | shasum -a 256 | head -c 8 | awk '{print "embassy-"$1}' | xargs hostnamectl set-hostname && systemctl restart avahi-daemon
#[instrument]
pub async fn sync_hostname() -> Result<(), Error> {
    set_hostname(&format!("embassy-{}", get_id().await?)).await?;
    Command::new("systemctl")
        .arg("restart")
        .arg("avahi-daemon")
        .invoke(crate::ErrorKind::Network)
        .await?;
    Ok(())
}
