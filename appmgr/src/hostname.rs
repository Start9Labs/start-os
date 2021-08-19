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
    let out = tokio::fs::read_to_string("/boot/product_key.txt").await?;
    Ok(out.trim().to_owned())
}

pub async fn sync_hostname() -> Result<(), Error> {
    let key = get_product_key().await?;
    let mut hasher = sha2::Sha256::new();
    hasher.update(key.as_bytes());
    let res = hasher.finalize();
    set_hostname(&format!("start9-{}", hex::encode(&res[0..4]))).await?;
    Ok(())
}
