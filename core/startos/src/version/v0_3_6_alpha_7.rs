use std::path::Path;
use std::{collections::BTreeMap, future::Future};

use exver::{PreReleaseSegment, VersionRange};
use imbl_value::{json, InternedString};
use itertools::Itertools;
use openssl::{
    pkey::{PKey, Private},
    x509::X509,
};
use patch_db::ModelExt;
use sqlx::postgres::PgConnectOptions;
use sqlx::PgPool;
use tokio::process::Command;
use torut::onion::TorSecretKeyV3;

use super::v0_3_5::V0_3_0_COMPAT;
use super::{v0_3_6_alpha_6, VersionT};
use crate::{
    auth::Sessions, backup::target::cifs::CifsTargets, notifications::Notifications,
    ssh::SshPubKey, util::Invoke,
};
use crate::{db::model::Database, util::serde::PemEncoding};
use crate::{disk::mount::util::unmount, ssh::SshKeys};
use crate::{net::forward::AvailablePorts, prelude::*};

lazy_static::lazy_static! {
    static ref V0_3_6_alpha_7: exver::Version = exver::Version::new(
        [0, 3, 6],
        [PreReleaseSegment::String("alpha".into()), 7.into()]
    );
}

#[tracing::instrument(skip_all)]
async fn init_postgres(datadir: impl AsRef<Path>) -> Result<PgPool, Error> {
    let db_dir = datadir.as_ref().join("main/postgresql");
    if tokio::process::Command::new("mountpoint")
        .arg("/var/lib/postgresql")
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .await?
        .success()
    {
        unmount("/var/lib/postgresql", true).await?;
    }
    let exists = tokio::fs::metadata(&db_dir).await.is_ok();
    if !exists {
        Command::new("cp")
            .arg("-ra")
            .arg("/var/lib/postgresql")
            .arg(&db_dir)
            .invoke(crate::ErrorKind::Filesystem)
            .await?;
    }
    Command::new("chown")
        .arg("-R")
        .arg("postgres:postgres")
        .arg(&db_dir)
        .invoke(crate::ErrorKind::Database)
        .await?;

    let mut pg_paths = tokio::fs::read_dir("/usr/lib/postgresql").await?;
    let mut pg_version = None;
    while let Some(pg_path) = pg_paths.next_entry().await? {
        let pg_path_version = pg_path
            .file_name()
            .to_str()
            .map(|v| v.parse())
            .transpose()?
            .unwrap_or(0);
        if pg_path_version > pg_version.unwrap_or(0) {
            pg_version = Some(pg_path_version)
        }
    }
    let pg_version = pg_version.ok_or_else(|| {
        Error::new(
            eyre!("could not determine postgresql version"),
            crate::ErrorKind::Database,
        )
    })?;

    crate::disk::mount::util::bind(&db_dir, "/var/lib/postgresql", false).await?;

    let pg_version_string = pg_version.to_string();
    let pg_version_path = db_dir.join(&pg_version_string);
    if exists
    // maybe migrate
    {
        let incomplete_path = db_dir.join(format!("{pg_version}.migration.incomplete"));
        if tokio::fs::metadata(&incomplete_path).await.is_ok() // previous migration was incomplete
        && tokio::fs::metadata(&pg_version_path).await.is_ok()
        {
            tokio::fs::remove_dir_all(&pg_version_path).await?;
        }
        if tokio::fs::metadata(&pg_version_path).await.is_err()
        // need to migrate
        {
            let conf_dir = Path::new("/etc/postgresql").join(pg_version.to_string());
            let conf_dir_tmp = {
                let mut tmp = conf_dir.clone();
                tmp.set_extension("tmp");
                tmp
            };
            if tokio::fs::metadata(&conf_dir).await.is_ok() {
                Command::new("mv")
                    .arg(&conf_dir)
                    .arg(&conf_dir_tmp)
                    .invoke(ErrorKind::Filesystem)
                    .await?;
            }
            let mut old_version = pg_version;
            while old_version > 13
            /* oldest pg version included in startos */
            {
                old_version -= 1;
                let old_datadir = db_dir.join(old_version.to_string());
                if tokio::fs::metadata(&old_datadir).await.is_ok() {
                    tokio::fs::File::create(&incomplete_path)
                        .await?
                        .sync_all()
                        .await?;
                    Command::new("pg_upgradecluster")
                        .arg(old_version.to_string())
                        .arg("main")
                        .invoke(crate::ErrorKind::Database)
                        .await?;
                    break;
                }
            }
            if tokio::fs::metadata(&conf_dir).await.is_ok() {
                if tokio::fs::metadata(&conf_dir).await.is_ok() {
                    tokio::fs::remove_dir_all(&conf_dir).await?;
                }
                Command::new("mv")
                    .arg(&conf_dir_tmp)
                    .arg(&conf_dir)
                    .invoke(ErrorKind::Filesystem)
                    .await?;
            }
            tokio::fs::remove_file(&incomplete_path).await?;
        }
        if tokio::fs::metadata(&incomplete_path).await.is_ok() {
            unreachable!() // paranoia
        }
    }

    Command::new("systemctl")
        .arg("start")
        .arg(format!("postgresql@{pg_version}-main.service"))
        .invoke(crate::ErrorKind::Database)
        .await?;
    if !exists {
        Command::new("sudo")
            .arg("-u")
            .arg("postgres")
            .arg("createuser")
            .arg("root")
            .invoke(crate::ErrorKind::Database)
            .await?;
        Command::new("sudo")
            .arg("-u")
            .arg("postgres")
            .arg("createdb")
            .arg("secrets")
            .arg("-O")
            .arg("root")
            .invoke(crate::ErrorKind::Database)
            .await?;
    }

    let secret_store =
        PgPool::connect_with(PgConnectOptions::new().database("secrets").username("root")).await?;
    sqlx::migrate!()
        .run(&secret_store)
        .await
        .with_kind(crate::ErrorKind::Database)?;
    Ok(secret_store)
}

struct Account {
    password: String,
    tor_key: TorSecretKeyV3,
    server_id: String,
    hostname: String,
    network_key: String,
    root_ca_key_pem: PKey<Private>,
    root_ca_cert_pem: X509,
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Version;

impl VersionT for Version {
    type Previous = v0_3_6_alpha_6::Version;
    type PreUpRes = (Account, SshKeys, CifsTargets, Notifications);
    fn semver(self) -> exver::Version {
        V0_3_6_alpha_7.clone()
    }
    fn compat(self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    async fn pre_up(self) -> Result<Self::PreUpRes, Error> {
        // provides `try_get`
        use sqlx::Row;
        // TODO What is this supposed to be for the datadir?
        let pg = init_postgres("/var/lib/startos").await?;
        let account_query = sqlx::query(r#"SELECT * FROM account"#)
            .fetch_one(&pg)
            .await?;
        let account = {
            Account {
                password: account_query.try_get("password")?,
                tor_key: todo!(), //TorSecretKeyV3::try_from(account_query.try_get("tor_key")?)?,
                server_id: account_query.try_get("server_id")?,
                hostname: account_query.try_get("hostname")?,
                network_key: account_query.try_get("network_key")?,
                root_ca_key_pem: todo!(), //serde_json::from_str(account_query.try_get("root_ca_key_pem")?)?,
                root_ca_cert_pem: X509::from_pem(
                    account_query
                        .try_get::<String, _>("root_ca_cert_pem")?
                        .as_bytes(),
                )?,
            }
        };

        let ssh_query = sqlx::query(r#"SELECT * FROM ssh_keys"#)
            .fetch_all(&pg)
            .await?;
        let ssh_keys: SshKeys = {
            let keys = ssh_query.into_iter().fold(
                Ok::<_, Error>(BTreeMap::<InternedString, WithTimeData<SshPubKey>>::new()),
                |ssh_keys, row| {
                    let mut ssh_keys = ssh_keys?;
                    let time = serde_json::from_str(row.try_get("created_at")?)
                        .with_kind(ErrorKind::Database)?;
                    let value: SshPubKey = serde_json::from_str(row.try_get("openssh_pubkey")?)
                        .with_kind(ErrorKind::Database)?;
                    let data = WithTimeData {
                        created_at: time,
                        updated_at: time,
                        value,
                    };
                    ssh_keys.insert(row.try_get::<String, _>("fingerprint")?.into(), data);
                    Ok(ssh_keys)
                },
            )?;
            SshKeys::from(keys)
        };

        Ok((account, ssh_keys, todo!(), todo!()))
    }
    fn up(
        self,
        db: &mut Value,
        (account, ssh_keys, cifs, notifications): Self::PreUpRes,
    ) -> Result<impl Future<Output = Result<(), Error>> + Send + 'static, Error> {
        let wifi = json!({
            "infterface": db["server-info"]["wifi"]["interface"],
            "ssids": db["server-info"]["wifi"]["ssids"],
            "selected": db["server-info"]["wifi"]["selected"],
            "last_region": db["server-info"]["wifi"]["last-region"],
        });

        let ip_info = {
            let mut ip_info = json!({});
            let empty = Default::default();
            for (k, v) in db["server-info"]["ip-info"].as_object().unwrap_or(&empty) {
                let k: &str = k.as_ref();
                ip_info[k] = json!({
                    "ipv4Range": v["ipv4-range"],
                    "ipv6Range": v["ipv6-range"],
                    "ipv4": v["ipv4"],
                    "ipv6": v["ipv6"],
                });
            }
            ip_info
        };

        let status_info = json!({
            "backupProgress": db["server-info"]["status-info"]["backup-progress"],
            "updated": db["server-info"]["status-info"]["updated"],
            "updateProgress": db["server-info"]["status-info"]["update-progress"],
            "shuttingDown": db["server-info"]["status-info"]["shutting-down"],
            "restarting": db["server-info"]["status-info"]["restarting"],
        });
        let server_info = {
            let mut server_info = json!({
                "arch": db["server-info"]["arch"],
                "platform": db["server-info"]["platform"],
                "id": db["server-info"]["id"],
                "hostname": db["server-info"]["hostname"],
                "version": db["server-info"]["version"],
                "lastBackup": db["server-info"]["last-backup"],
                "eosVersionCompat": db["server-info"]["eos-version-compat"],
                "lanAddress": db["server-info"]["lan-address"],
            });

            // Maybe we do this like the Public::init does
            server_info["onionAddress"] = db["server-info"]["onion-address"].clone();
            server_info["torAddress"] = db["server-info"]["tor-address"].clone();
            server_info["ipInfo"] = ip_info;
            server_info["statusInfo"] = status_info;
            server_info["wifi"] = wifi;
            server_info["unreadNotificationCount"] =
                db["server-info"]["unread-notification-count"].clone();
            server_info["passwordHash"] = db["server-info"]["password-hash"].clone();
            server_info["pubkey"] = db["server-info"]["pubkey"].clone();
            server_info["caFingerprint"] = db["server-info"]["ca-fingerprint"].clone();
            server_info["ntpSynced"] = db["server-info"]["ntp-synced"].clone();
            server_info["zram"] = db["server-info"]["zram"].clone();
            server_info["governor"] = db["server-info"]["governor"].clone();
            // This one should always be empty, doesn't exist in the previous. And the smtp is all single word key
            server_info["smtp"] = db["server-info"]["smtp"].clone();
            server_info
        };

        let public = json!({
            "serverInfo": server_info,
            "packageData": json!({}),
            "ui": db["ui"],
        });

        let private = {
            let mut value = json!({});
            // keystore.onion from tor
            // keystore.local new with information from secrets.account
            value["keystore"] = todo!();
            value["password"] = imbl_value::to_value(&account.password)?;
            value["compatS9pkKey"] =
                imbl_value::to_value(&crate::db::model::private::generate_compat_key())?;
            // ssh_privkey nex
            // ssh_pubkeys from the ssh
            value["sshPrivkey"] = todo!();
            value["sshPubkeys"] = todo!();
            value["ssh"] = imbl_value::to_value(&ssh_keys)?;
            value["availablePorts"] = imbl_value::to_value(&AvailablePorts::new())?;
            value["sessions"] = imbl_value::to_value(&Sessions::new())?;
            value["notifications"] = imbl_value::to_value(&notifications)?;
            value["cifs"] = imbl_value::to_value(&cifs)?;
            value["packageStores"] = json!({});
            value
        };
        let next: Value = json!({
            "public": public,
            "private": private,
        });

        *db = next;

        Ok(async {
            // reinstall packages
            Ok(())
        })
    }
    fn down(
        self,
        _db: &mut Value,
    ) -> Result<impl Future<Output = Result<(), Error>> + Send + 'static, Error> {
        Ok(async {
            Err(Error::new(
                eyre!("downgrades prohibited"),
                ErrorKind::InvalidRequest,
            ))
        })
    }

    fn commit(self, db: &mut Value) -> Result<(), Error> {
        todo!()
    }
}
