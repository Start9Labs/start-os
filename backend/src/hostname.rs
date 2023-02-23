use rand::{thread_rng, Rng};
use tokio::process::Command;
use tracing::instrument;

use crate::account::AccountInfo;
use crate::prelude::*;
use crate::util::Invoke;
#[derive(Clone, serde::Deserialize, serde::Serialize, Debug)]
pub struct Hostname(pub String);

lazy_static::lazy_static! {
    static ref ADJECTIVES: Vec<String> = include_str!("./assets/adjectives.txt").lines().map(|x| x.to_string()).collect();
    static ref NOUNS: Vec<String> = include_str!("./assets/nouns.txt").lines().map(|x| x.to_string()).collect();
}
impl AsRef<str> for Hostname {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl Hostname {
    pub fn lan_address(&self) -> String {
        format!("https://{}.local", self.0)
    }

    pub fn local_domain_name(&self) -> String {
        format!("{}.local", self.0)
    }
    pub fn no_dot_host_name(&self) -> String {
        self.0.to_owned()
    }
}

pub fn generate_hostname() -> Hostname {
    let mut rng = thread_rng();
    let adjective = &ADJECTIVES[rng.gen_range(0..ADJECTIVES.len())];
    let noun = &NOUNS[rng.gen_range(0..NOUNS.len())];
    Hostname(format!("{adjective}-{noun}"))
}

pub fn generate_id() -> String {
    let id = uuid::Uuid::new_v4();
    id.to_string()
}

#[instrument]
pub async fn get_current_hostname() -> Result<Hostname, Error> {
    let out = Command::new("hostname")
        .invoke(ErrorKind::ParseSysInfo)
        .await?;
    let out_string = String::from_utf8(out)?;
    Ok(Hostname(out_string.trim().to_owned()))
}

#[instrument]
pub async fn set_hostname(hostname: &Hostname) -> Result<(), Error> {
    let hostname: &String = &hostname.0;
    let _out = Command::new("hostnamectl")
        .arg("set-hostname")
        .arg(hostname)
        .invoke(ErrorKind::ParseSysInfo)
        .await?;
    Ok(())
}

#[instrument]
pub async fn sync_hostname(account: &AccountInfo) -> Result<(), Error> {
    set_hostname(&account.hostname).await?;
    Command::new("systemctl")
        .arg("restart")
        .arg("avahi-daemon")
        .invoke(ErrorKind::Network)
        .await?;
    Ok(())
}
