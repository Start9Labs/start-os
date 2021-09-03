use digest::Digest;
use tokio::process::Command;

use crate::util::Invoke;
use crate::{Error, ErrorKind};

pub async fn get_hostname() -> Result<String, Error> {
    let out = Command::new("hostname")
        .invoke(ErrorKind::ParseSysInfo)
        .await?;
    let out_string = String::from_utf8(out)?;
    Ok(out_string.trim().to_owned())
}

pub async fn set_hostname(hostname: &str) -> Result<(), Error> {
    let _out = Command::new("hostnamectl")
        .arg("set-hostname")
        .arg(hostname)
        .invoke(ErrorKind::ParseSysInfo)
        .await?;
    Ok(())
}

pub async fn get_product_key() -> Result<String, Error> {
    let out = tokio::fs::read_to_string("/boot/embassy-os/product_key.txt").await?;
    Ok(out.trim().to_owned())
}

pub async fn get_id() -> Result<String, Error> {
    let key = get_product_key().await?;
    let mut hasher = sha2::Sha256::new();
    hasher.update(key.as_bytes());
    let res = hasher.finalize();
    Ok(hex::encode(&res[0..4]))
}

// cat /boot/embassy-os/product_key.txt | shasum -a 256 | head -c 8 | awk '{print "start9-"$1}' | xargs hostnamectl set-hostname
pub async fn sync_hostname() -> Result<(), Error> {
    set_hostname(&format!("start9-{}", get_id().await?)).await?;
    Ok(())
}
