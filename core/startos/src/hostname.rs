use imbl_value::InternedString;
use lazy_format::lazy_format;
use rand::{thread_rng, Rng};
use tokio::process::Command;
use tracing::instrument;

use crate::util::Invoke;
use crate::{Error, ErrorKind};
#[derive(Clone, Debug, Default, serde::Deserialize, serde::Serialize)]
pub struct Hostname(pub InternedString);

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
    pub fn lan_address(&self) -> InternedString {
        InternedString::from_display(&lazy_format!("https://{}.local", self.0))
    }

    pub fn local_domain_name(&self) -> InternedString {
        InternedString::from_display(&lazy_format!("{}.local", self.0))
    }

    pub fn no_dot_host_name(&self) -> InternedString {
        self.0.clone()
    }
}

pub fn generate_hostname() -> Hostname {
    let mut rng = thread_rng();
    let adjective = &ADJECTIVES[rng.gen_range(0..ADJECTIVES.len())];
    let noun = &NOUNS[rng.gen_range(0..NOUNS.len())];
    Hostname(InternedString::from_display(&lazy_format!(
        "{adjective}-{noun}"
    )))
}

pub fn generate_id() -> String {
    let id = uuid::Uuid::new_v4();
    id.to_string()
}

#[instrument(skip_all)]
pub async fn get_current_hostname() -> Result<Hostname, Error> {
    let out = Command::new("hostname")
        .invoke(ErrorKind::ParseSysInfo)
        .await?;
    let out_string = String::from_utf8(out)?;
    Ok(Hostname(out_string.trim().into()))
}

#[instrument(skip_all)]
pub async fn set_hostname(hostname: &Hostname) -> Result<(), Error> {
    let hostname = &*hostname.0;
    Command::new("hostnamectl")
        .arg("--static")
        .arg("set-hostname")
        .arg(hostname)
        .invoke(ErrorKind::ParseSysInfo)
        .await?;
    Command::new("sed")
        .arg("-i")
        .arg(format!(
            "s/\\(\\s\\)localhost\\( {hostname}\\)\\?/\\1localhost {hostname}/g"
        ))
        .arg("/etc/hosts")
        .invoke(ErrorKind::ParseSysInfo)
        .await?;
    Ok(())
}

#[instrument(skip_all)]
pub async fn sync_hostname(hostname: &Hostname) -> Result<(), Error> {
    set_hostname(hostname).await?;
    Command::new("systemctl")
        .arg("restart")
        .arg("avahi-daemon")
        .invoke(crate::ErrorKind::Network)
        .await?;
    Ok(())
}
