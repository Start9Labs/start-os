use digest::Digest;
use patch_db::DbHandle;
use tokio::process::Command;
use tracing::instrument;
use rand::{thread_rng, Rng};

use crate::util::Invoke;
use crate::{Error, ErrorKind};
#[derive(Clone, serde::Deserialize, serde::Serialize, Debug)]
pub struct HostName(pub String);

lazy_static::lazy_static! {
    static ref ADJECTIVES: Vec<String> = include_str!("./assets/adjectives.txt").lines().map(|x| x.to_string()).collect();
    static ref NOUNS: Vec<String> = include_str!("./assets/nouns.txt").lines().map(|x| x.to_string()).collect();
}
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

pub fn generate_hostname() -> HostName {
    let mut rng = thread_rng();
    let adjective = &ADJECTIVES[rng.gen_range(0..ADJECTIVES.len())];
    let noun = &NOUNS[rng.gen_range(0..NOUNS.len())];
    HostName(format!("{adjective}-{noun}"))
}


pub fn generate_id() -> String {
    let id = uuid::Uuid::new_v4();
    id.to_string()
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
    let hostname: &String = &hostname.0;
    tracing::debug!("BLUJ: Hostname = {}", hostname);
    let _out = Command::new("hostnamectl")
        .arg("set-hostname")
        .arg(hostname)
        .invoke(ErrorKind::ParseSysInfo)
        .await?;
    Ok(())
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
        if let Some(hostname) = hostname.to_owned() {
            return Ok(HostName(hostname));
        }
    }
    let id = get_id(handle).await?;
    if id.len() != 8 {
        return Ok(generate_hostname());
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
