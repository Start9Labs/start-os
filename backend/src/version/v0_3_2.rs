use emver::VersionRange;

use super::v0_3_0::V0_3_0_COMPAT;
use super::*;

const V0_3_2: emver::Version = emver::Version::new(0, 3, 2, 0);

lazy_static::lazy_static! {
    static ref DEFAULT_UI: serde_json::Value =serde_json::json!({
        "name": null,
        "auto-check-updates": true,
        "pkg-order": [],
        "ack-welcome": "0.3.2",
        "marketplace": {
          "selected-id": null,
          "known-hosts": {}
        },
        "dev": {},
        "gaming": {
          "snake": {
            "high-score": 0
          }
        },
        "ack-instructions": {}
      });

}

#[derive(Clone, Debug)]
pub struct Version;
#[async_trait]
impl VersionT for Version {
    type Previous = v0_3_1_2::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> emver::Version {
        V0_3_2
    }
    fn compat(&self) -> &'static VersionRange {
        &*V0_3_0_COMPAT
    }
    async fn up<Db: DbHandle>(&self, db: &mut Db, _secrets: &PgPool) -> Result<(), Error> {
        let hostname = legacy::hostname::get_hostname(db).await?;
        crate::db::DatabaseModel::new()
            .server_info()
            .hostname()
            .put(db, &Some(hostname.0))
            .await?;
        crate::db::DatabaseModel::new()
            .server_info()
            .id()
            .put(db, &legacy::hostname::generate_id())
            .await?;

        legacy::hostname::sync_hostname(db).await?;
        Ok(())
    }
    async fn down<Db: DbHandle>(&self, _db: &mut Db, _secrets: &PgPool) -> Result<(), Error> {
        Ok(())
    }
}

mod legacy {
    pub mod hostname {
        use patch_db::DbHandle;
        use rand::{thread_rng, Rng};
        use tokio::process::Command;
        use tracing::instrument;

        use crate::util::Invoke;
        use crate::{Error, ErrorKind};
        #[derive(Clone, serde::Deserialize, serde::Serialize, Debug)]
        pub struct Hostname(pub String);

        lazy_static::lazy_static! {
            static ref ADJECTIVES: Vec<String> = include_str!("../assets/adjectives.txt").lines().map(|x| x.to_string()).collect();
            static ref NOUNS: Vec<String> = include_str!("../assets/nouns.txt").lines().map(|x| x.to_string()).collect();
        }
        impl AsRef<str> for Hostname {
            fn as_ref(&self) -> &str {
                &self.0
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

        #[instrument(skip_all)]
        pub async fn get_current_hostname() -> Result<Hostname, Error> {
            let out = Command::new("hostname")
                .invoke(ErrorKind::ParseSysInfo)
                .await?;
            let out_string = String::from_utf8(out)?;
            Ok(Hostname(out_string.trim().to_owned()))
        }

        #[instrument(skip_all)]
        pub async fn set_hostname(hostname: &Hostname) -> Result<(), Error> {
            let hostname: &String = &hostname.0;
            let _out = Command::new("hostnamectl")
                .arg("set-hostname")
                .arg(hostname)
                .invoke(ErrorKind::ParseSysInfo)
                .await?;
            Ok(())
        }

        #[instrument(skip_all)]
        pub async fn get_id<Db: DbHandle>(handle: &mut Db) -> Result<String, Error> {
            let id = crate::db::DatabaseModel::new()
                .server_info()
                .id()
                .get(handle)
                .await?;
            Ok(id.to_string())
        }

        pub async fn get_hostname<Db: DbHandle>(handle: &mut Db) -> Result<Hostname, Error> {
            if let Ok(hostname) = crate::db::DatabaseModel::new()
                .server_info()
                .hostname()
                .get(handle)
                .await
            {
                if let Some(hostname) = hostname.to_owned() {
                    return Ok(Hostname(hostname));
                }
            }
            let id = get_id(handle).await?;
            if id.len() != 8 {
                return Ok(generate_hostname());
            }
            return Ok(Hostname(format!("embassy-{}", id)));
        }
        #[instrument(skip_all)]
        pub async fn sync_hostname<Db: DbHandle>(handle: &mut Db) -> Result<(), Error> {
            set_hostname(&get_hostname(handle).await?).await?;
            Command::new("systemctl")
                .arg("restart")
                .arg("avahi-daemon")
                .invoke(crate::ErrorKind::Network)
                .await?;
            Ok(())
        }
    }
}
