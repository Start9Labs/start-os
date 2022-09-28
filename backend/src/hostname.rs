use patch_db::DbHandle;
use rand::{thread_rng, Rng};
use tokio::process::Command;
use tracing::instrument;

use crate::util::Invoke;
use crate::{Error, ErrorKind};
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
}

pub fn generate_hostname() -> Hostname {
    let mut rng = thread_rng();
    let adjective = &ADJECTIVES[rng.gen_range(0..ADJECTIVES.len())];
    let noun = &NOUNS[rng.gen_range(0..NOUNS.len())];
    Hostname(format!("embassy-{adjective}-{noun}"))
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

#[instrument(skip(handle, receipts))]
pub async fn get_id<Db: DbHandle>(
    handle: &mut Db,
    receipts: &HostNameReceipt,
) -> Result<String, Error> {
    let id = receipts.id.get(handle).await?;
    Ok(id)
}

pub async fn get_hostname<Db: DbHandle>(
    handle: &mut Db,
    receipts: &HostNameReceipt,
) -> Result<Hostname, Error> {
    if let Ok(hostname) = receipts.hostname.get(handle).await {
        if let Some(hostname) = hostname.to_owned() {
            return Ok(Hostname(hostname));
        }
    }
    let id = get_id(handle, receipts).await?;
    if id.len() != 8 {
        return Ok(generate_hostname());
    }
    return Ok(Hostname(format!("embassy-{}", id)));
}

pub async fn ensure_hostname_is_set<Db: DbHandle>(
    handle: &mut Db,
    receipts: &HostNameReceipt,
) -> Result<(), Error> {
    let hostname = get_hostname(handle, &receipts).await?;
    receipts.hostname.set(handle, Some(hostname.0)).await?;
    Ok(())
}

#[derive(Clone)]
pub struct HostNameReceipt {
    hostname: patch_db::LockReceipt<Option<String>, ()>,
    pub id: patch_db::LockReceipt<String, ()>,
}

impl HostNameReceipt {
    pub async fn new<'a>(db: &'a mut impl DbHandle) -> Result<Self, Error> {
        let mut locks = Vec::new();

        let setup = Self::setup(&mut locks);
        setup(&db.lock_all(locks).await?)
    }

    pub fn setup(
        locks: &mut Vec<patch_db::LockTargetId>,
    ) -> impl FnOnce(&patch_db::Verifier) -> Result<Self, Error> {
        use patch_db::LockType;
        let hostname = crate::db::DatabaseModel::new()
            .server_info()
            .hostname()
            .make_locker(LockType::Write)
            .add_to_keys(locks);
        let id = crate::db::DatabaseModel::new()
            .server_info()
            .id()
            .make_locker(LockType::Write)
            .add_to_keys(locks);
        move |skeleton_key| {
            Ok(Self {
                hostname: hostname.verify(skeleton_key)?,
                id: id.verify(skeleton_key)?,
            })
        }
    }
}

#[instrument(skip(handle, receipts))]
pub async fn sync_hostname<Db: DbHandle>(
    handle: &mut Db,
    receipts: &HostNameReceipt,
) -> Result<(), Error> {
    set_hostname(&get_hostname(handle, receipts).await?).await?;
    Command::new("systemctl")
        .arg("restart")
        .arg("avahi-daemon")
        .invoke(crate::ErrorKind::Network)
        .await?;
    Ok(())
}
