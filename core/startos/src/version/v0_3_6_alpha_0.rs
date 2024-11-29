use std::collections::BTreeMap;
use std::future::Future;
use std::path::Path;

use chrono::{DateTime, Utc};
use ed25519_dalek::SigningKey;
use exver::{PreReleaseSegment, VersionRange};
use imbl_value::{json, InternedString};
use itertools::Itertools;
use models::PackageId;
use openssl::pkey::{PKey, Private};
use openssl::x509::X509;
use patch_db::ModelExt;
use sqlx::postgres::PgConnectOptions;
use sqlx::{PgPool, Row};
use ssh_key::Fingerprint;
use tokio::process::Command;
use torut::onion::TorSecretKeyV3;

use super::v0_3_5::V0_3_0_COMPAT;
use super::{v0_3_5_2, VersionT};
use crate::account::AccountInfo;
use crate::auth::Sessions;
use crate::backup::target::cifs::CifsTargets;
use crate::context::RpcContext;
use crate::db::model::Database;
use crate::disk::mount::filesystem::cifs::Cifs;
use crate::disk::mount::util::unmount;
use crate::hostname::Hostname;
use crate::net::forward::AvailablePorts;
use crate::net::keys::KeyStore;
use crate::net::ssl::CertStore;
use crate::net::tor;
use crate::net::tor::OnionStore;
use crate::notifications::{Notification, Notifications};
use crate::prelude::*;
use crate::s9pk::merkle_archive::source::multi_cursor_file::MultiCursorFile;
use crate::ssh::{SshKeys, SshPubKey};
use crate::util::crypto::ed25519_expand_key;
use crate::util::serde::{Pem, PemEncoding};
use crate::util::Invoke;

lazy_static::lazy_static! {
    static ref V0_3_6_alpha_0: exver::Version = exver::Version::new(
        [0, 3, 6],
        [PreReleaseSegment::String("alpha".into()), 0.into()]
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
    dbg!("Init Postgres Done");
    Ok(secret_store)
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Version;

impl VersionT for Version {
    type Previous = v0_3_5_2::Version;
    type PreUpRes = (AccountInfo, SshKeys, CifsTargets, Notifications);
    fn semver(self) -> exver::Version {
        V0_3_6_alpha_0.clone()
    }
    fn compat(self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    async fn pre_up(self) -> Result<Self::PreUpRes, Error> {
        let pg = init_postgres("/embassy-data").await?;
        let account = previous_account_info(&pg).await?;

        let ssh_keys = previous_ssh_keys(&pg).await?;

        let cifs = previous_cifs(&pg).await?;

        let notifications = previous_notifications(pg).await?;

        Ok((account, ssh_keys, cifs, notifications))
    }
    fn up(
        self,
        db: &mut Value,
        (account, ssh_keys, cifs, notifications): Self::PreUpRes,
    ) -> Result<(), Error> {
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
                "versionCompat": db["server-info"]["eos-version-compat"],
                "lastBackup": db["server-info"]["last-backup"],
                "lanAddress": db["server-info"]["lan-address"],
            });

            server_info["postInitMigrationTodos"] = json!([]);
            let tor_address: String = from_value(db["server-info"]["tor-address"].clone())?;
            // Maybe we do this like the Public::init does
            server_info["torAddress"] = json!(tor_address);
            server_info["onionAddress"] = json!(tor_address
                .replace("https://", "")
                .replace("http://", "")
                .replace(".onion/", ""));
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
            value["keyStore"] = to_value(&KeyStore::new(&account)?)?;
            value["password"] = to_value(&account.password)?;
            value["compatS9pkKey"] = to_value(&crate::db::model::private::generate_compat_key())?;
            value["sshPrivkey"] = to_value(Pem::new_ref(&account.ssh_key))?;
            value["sshPubkeys"] = to_value(&ssh_keys)?;
            value["availablePorts"] = to_value(&AvailablePorts::new())?;
            value["sessions"] = to_value(&Sessions::new())?;
            value["notifications"] = to_value(&notifications)?;
            value["cifs"] = to_value(&cifs)?;
            value["packageStores"] = json!({});
            value
        };
        let next: Value = json!({
            "public": public,
            "private": private,
        });

        dbg!("Should be done with the up");
        *db = next;
        Ok(())
    }
    fn down(self, _db: &mut Value) -> Result<(), Error> {
        Err(Error::new(
            eyre!("downgrades prohibited"),
            ErrorKind::InvalidRequest,
        ))
    }

    #[instrument(skip(self, ctx))]
    /// MUST be idempotent, and is run after *all* db migrations
    async fn post_up(self, ctx: &RpcContext) -> Result<(), Error> {
        let path = Path::new("/embassy-data/package-data/archive/");
        if !path.is_dir() {
            return Err(Error::new(
                eyre!(
                    "expected path ({}) to be a directory",
                    path.to_string_lossy()
                ),
                ErrorKind::Filesystem,
            ));
        }
        // Should be the name of the package
        let mut paths = tokio::fs::read_dir(path).await?;
        while let Some(path) = paths.next_entry().await? {
            let path = path.path();
            if !path.is_dir() {
                continue;
            }
            // Should be the version of the package
            let mut paths = tokio::fs::read_dir(path).await?;
            while let Some(path) = paths.next_entry().await? {
                let path = path.path();
                if !path.is_dir() {
                    continue;
                }

                // Should be s9pk
                let mut paths = tokio::fs::read_dir(path).await?;
                while let Some(path) = paths.next_entry().await? {
                    let path = path.path();
                    if path.is_dir() {
                        continue;
                    }

                    let package_s9pk = tokio::fs::File::open(path).await?;
                    let file = MultiCursorFile::open(&package_s9pk).await?;

                    let key = ctx.db.peek().await.into_private().into_compat_s9pk_key();
                    ctx.services
                        .install(
                            ctx.clone(),
                            || crate::s9pk::load(file.clone(), || Ok(key.de()?.0), None),
                            None::<crate::util::Never>,
                            None,
                        )
                        .await?
                        .await?
                        .await?;
                }
            }
        }
        Ok(())
    }
}

async fn previous_notifications(pg: sqlx::Pool<sqlx::Postgres>) -> Result<Notifications, Error> {
    let notification_cursor = sqlx::query(r#"SELECT * FROM notifications"#)
        .fetch_all(&pg)
        .await?;
    let notifications = {
        let mut notifications = Notifications::default();
        for row in notification_cursor {
            let package_id = serde_json::from_str::<PackageId>(
                row.try_get("package_id")
                    .with_ctx(|_| (ErrorKind::Database, "package_id"))?,
            )
            .ok();

            let created_at = row
                .try_get("created_at")
                .with_ctx(|_| (ErrorKind::Database, "created_at"))?;
            let code = row
                .try_get::<i64, _>("code")
                .with_ctx(|_| (ErrorKind::Database, "code"))? as u32;
            let id = row
                .try_get::<i64, _>("id")
                .with_ctx(|_| (ErrorKind::Database, "id"))? as u32;
            let level = serde_json::from_str(
                row.try_get("level")
                    .with_ctx(|_| (ErrorKind::Database, "level"))?,
            )
            .with_kind(ErrorKind::Database)
            .with_ctx(|_| (ErrorKind::Database, "level: serde_json "))?;
            let title = row
                .try_get("title")
                .with_ctx(|_| (ErrorKind::Database, "title"))?;
            let message = row
                .try_get("message")
                .with_ctx(|_| (ErrorKind::Database, "message"))?;
            let data = serde_json::from_str(
                row.try_get("data")
                    .with_ctx(|_| (ErrorKind::Database, "data"))?,
            )
            .unwrap_or_default();

            notifications.0.insert(
                id,
                Notification {
                    package_id,
                    created_at,
                    code,
                    level,
                    title,
                    message,
                    data,
                },
            );
        }
        notifications
    };
    Ok(notifications)
}

#[tracing::instrument(skip_all)]
async fn previous_cifs(pg: &sqlx::Pool<sqlx::Postgres>) -> Result<CifsTargets, Error> {
    let cifs = sqlx::query(r#"SELECT * FROM cifs_shares"#)
        .fetch_all(pg)
        .await?
        .into_iter()
        .map(|row| {
            let id: i64 = row.try_get("id")?;
            Ok::<_, Error>((
                id,
                Cifs {
                    hostname: row
                        .try_get("hostname")
                        .with_ctx(|_| (ErrorKind::Database, "hostname"))?,
                    path: serde_json::from_str(row.try_get("path")?)
                        .with_kind(ErrorKind::Database)
                        .with_ctx(|_| (ErrorKind::Database, "path"))?,
                    username: row
                        .try_get("username")
                        .with_ctx(|_| (ErrorKind::Database, "username"))?,
                    password: row
                        .try_get("password")
                        .with_ctx(|_| (ErrorKind::Database, "password"))?,
                },
            ))
        })
        .fold(Ok::<_, Error>(CifsTargets::default()), |cifs, data| {
            let mut cifs = cifs?;
            let (id, cif_value) = data?;
            cifs.0.insert(id as u32, cif_value);
            Ok(cifs)
        })?;
    Ok(cifs)
}

#[tracing::instrument(skip_all)]
async fn previous_account_info(pg: &sqlx::Pool<sqlx::Postgres>) -> Result<AccountInfo, Error> {
    let account_query = sqlx::query(r#"SELECT * FROM account"#)
        .fetch_one(pg)
        .await?;
    let account = {
        AccountInfo {
            password: account_query
                .try_get("password")
                .with_ctx(|_| (ErrorKind::Database, "password"))?,
            tor_key: TorSecretKeyV3::try_from(
                if let Some(bytes) = account_query
                    .try_get::<Option<Vec<u8>>, _>("tor_key")
                    .with_ctx(|_| (ErrorKind::Database, "tor_key"))?
                {
                    <[u8; 64]>::try_from(bytes)
                        .map_err(|e| {
                            Error::new(
                                eyre!("expected vec of len 64, got len {}", e.len()),
                                ErrorKind::ParseDbField,
                            )
                        })
                        .with_ctx(|_| (ErrorKind::Database, "password.u8 64"))?
                } else {
                    ed25519_expand_key(
                        &<[u8; 32]>::try_from(account_query.try_get::<Vec<u8>, _>("network_key")?)
                            .map_err(|e| {
                                Error::new(
                                    eyre!("expected vec of len 32, got len {}", e.len()),
                                    ErrorKind::ParseDbField,
                                )
                            })
                            .with_ctx(|_| (ErrorKind::Database, "password.u8 32"))?,
                    )
                },
            )?,
            server_id: account_query
                .try_get("server_id")
                .with_ctx(|_| (ErrorKind::Database, "server_id"))?,
            hostname: Hostname(
                account_query
                    .try_get::<String, _>("hostname")
                    .with_ctx(|_| (ErrorKind::Database, "hostname"))?
                    .into(),
            ),
            root_ca_key: PKey::private_key_from_pem(
                &account_query
                    .try_get::<String, _>("root_ca_key_pem")
                    .with_ctx(|_| (ErrorKind::Database, "root_ca_key_pem"))?
                    .as_bytes(),
            )
            .with_ctx(|_| (ErrorKind::Database, "private_key_from_pem"))?,
            root_ca_cert: X509::from_pem(
                account_query
                    .try_get::<String, _>("root_ca_cert_pem")
                    .with_ctx(|_| (ErrorKind::Database, "root_ca_cert_pem"))?
                    .as_bytes(),
            )
            .with_ctx(|_| (ErrorKind::Database, "X509::from_pem"))?,
            compat_s9pk_key: SigningKey::generate(&mut rand::thread_rng()),
            ssh_key: ssh_key::PrivateKey::random(
                &mut rand::thread_rng(),
                ssh_key::Algorithm::Ed25519,
            )
            .with_ctx(|_| (ErrorKind::Database, "X509::ssh_key::PrivateKey::random"))?,
        }
    };
    Ok(account)
}
#[tracing::instrument(skip_all)]
async fn previous_ssh_keys(pg: &sqlx::Pool<sqlx::Postgres>) -> Result<SshKeys, Error> {
    let ssh_query = sqlx::query(r#"SELECT * FROM ssh_keys"#)
        .fetch_all(pg)
        .await?;
    let ssh_keys: SshKeys = {
        let keys = ssh_query.into_iter().fold(
            Ok::<_, Error>(BTreeMap::<InternedString, WithTimeData<SshPubKey>>::new()),
            |ssh_keys, row| {
                let mut ssh_keys = ssh_keys?;
                let time = row
                    .try_get::<String, _>("created_at")
                    .map_err(Error::from)
                    .and_then(|x| x.parse::<DateTime<Utc>>().with_kind(ErrorKind::Database))
                    .with_ctx(|_| (ErrorKind::Database, "openssh_pubkey::created_at"))?;
                let value: SshPubKey = row
                    .try_get::<String, _>("openssh_pubkey")
                    .map_err(Error::from)
                    .and_then(|x| x.parse().map(SshPubKey).with_kind(ErrorKind::Database))
                    .with_ctx(|_| (ErrorKind::Database, "openssh_pubkey"))?;
                let data = WithTimeData {
                    created_at: time,
                    updated_at: time,
                    value,
                };
                let fingerprint = row
                    .try_get::<String, _>("fingerprint")
                    .with_ctx(|_| (ErrorKind::Database, "fingerprint"))?;
                ssh_keys.insert(fingerprint.into(), data);
                Ok(ssh_keys)
            },
        )?;
        SshKeys::from(keys)
    };
    Ok(ssh_keys)
}
