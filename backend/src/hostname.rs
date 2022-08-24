use digest::Digest;
use patch_db::DbHandle;
use tokio::process::Command;
use tracing::instrument;

use crate::util::Invoke;
use crate::{Error, ErrorKind};
#[derive(Clone, serde::Deserialize, serde::Serialize, Debug)]
pub struct HostName(String);

impl AsRef<str> for HostName {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl HostName {
    pub fn lan_address(&self) -> String {
        format!("https://{}.local", self.0)
    }
}

#[instrument]
pub async fn get_current_hostname() -> Result<HostName, Error> {
    let out = Command::new("hostname")
        .invoke(ErrorKind::ParseSysInfo)
        .await?;
    let out_string = String::from_utf8(out)?;
    Ok(HostName(out_string.trim().to_owned()))
}

#[instrument]
pub async fn set_hostname(hostname: &HostName) -> Result<(), Error> {
    let hostname: &str = hostname.as_ref();
    let _out = Command::new("hostnamectl")
        .arg("set-hostname")
        .arg(hostname)
        .invoke(ErrorKind::ParseSysInfo)
        .await?;
    Ok(())
}

pub fn derive_id(key: &str) -> String {
    let mut hasher = sha2::Sha256::new();
    hasher.update(key.as_bytes());
    let res = hasher.finalize();
    hex::encode(&res[0..4])
}

#[instrument(skip(handle))]
pub async fn get_id<Db: DbHandle>(handle: &mut Db) -> Result<String, Error> {
    let id = crate::db::DatabaseModel::new()
        .server_info()
        .id()
        .get(handle, false)
        .await?;
    Ok(id.to_string())
}

pub async fn get_hostname<Db: DbHandle>(handle: &mut Db) -> Result<HostName, Error> {
    if let Ok(hostname) = crate::db::DatabaseModel::new()
        .server_info()
        .hostname()
        .get(handle, false)
        .await
    {
        return Ok(HostName(hostname.to_string()));
    }
    let id = get_id(handle).await?;
    if id.contains("-") {
        return Ok(HostName(id));
    }
    return Ok(HostName(format!("embassy-{}", id)));
}
#[instrument(skip(handle))]
pub async fn sync_hostname<Db: DbHandle>(mut handle: Db) -> Result<(), Error> {
    set_hostname(&get_hostname(&mut handle).await?).await?;
    Command::new("systemctl")
        .arg("restart")
        .arg("avahi-daemon")
        .invoke(crate::ErrorKind::Network)
        .await?;
    Ok(())
}
