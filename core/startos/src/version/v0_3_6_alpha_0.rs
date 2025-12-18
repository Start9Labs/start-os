use std::collections::{BTreeMap, BTreeSet};
use std::ffi::OsStr;
use std::path::Path;

use chrono::{DateTime, Utc};
use const_format::formatcp;
use ed25519_dalek::SigningKey;
use exver::{PreReleaseSegment, VersionRange};
use imbl_value::{InternedString, json};
use crate::{HostId, Id, PackageId, ReplayId};
use openssl::pkey::PKey;
use openssl::x509::X509;
use sqlx::postgres::PgConnectOptions;
use sqlx::{PgPool, Row};
use tokio::process::Command;

use super::v0_3_5::V0_3_0_COMPAT;
use super::{VersionT, v0_3_5_2};
use crate::account::AccountInfo;
use crate::auth::Sessions;
use crate::backup::target::cifs::CifsTargets;
use crate::context::RpcContext;
use crate::disk::mount::filesystem::cifs::Cifs;
use crate::disk::mount::util::unmount;
use crate::hostname::Hostname;
use crate::net::forward::AvailablePorts;
use crate::net::host::Host;
use crate::net::keys::KeyStore;
use crate::net::tor::{OnionAddress, TorSecretKey};
use crate::notifications::Notifications;
use crate::prelude::*;
use crate::s9pk::merkle_archive::source::multi_cursor_file::MultiCursorFile;
use crate::ssh::{SshKeys, SshPubKey};
use crate::util::Invoke;
use crate::util::crypto::ed25519_expand_key;
use crate::util::serde::Pem;
use crate::{DATA_DIR, PACKAGE_DATA};

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

    let secret_store = if let Ok(s) = PgPool::connect_with(
        PgConnectOptions::new()
            .database("secrets")
            .username("root")
            .port(5432)
            .socket("/var/run/postgresql"),
    )
    .await
    {
        s
    } else {
        PgPool::connect_with(
            PgConnectOptions::new()
                .database("secrets")
                .username("root")
                .port(5433)
                .socket("/var/run/postgresql"),
        )
        .await
        .with_kind(ErrorKind::Database)?
    };
    Ok(secret_store)
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Version;

impl VersionT for Version {
    type Previous = v0_3_5_2::Version;
    type PreUpRes = (
        AccountInfo,
        SshKeys,
        CifsTargets,
        BTreeMap<PackageId, BTreeMap<HostId, TorSecretKey>>,
    );
    fn semver(self) -> exver::Version {
        V0_3_6_alpha_0.clone()
    }
    fn compat(self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    async fn pre_up(self) -> Result<Self::PreUpRes, Error> {
        let pg = init_postgres(DATA_DIR).await?;
        let account = previous_account_info(&pg).await?;

        let ssh_keys = previous_ssh_keys(&pg).await?;

        let cifs = previous_cifs(&pg).await?;

        let tor_keys = previous_tor_keys(&pg).await?;

        Ok((account, ssh_keys, cifs, tor_keys))
    }
    fn up(
        self,
        db: &mut Value,
        (account, ssh_keys, cifs, tor_keys): Self::PreUpRes,
    ) -> Result<Value, Error> {
        let prev_package_data = db["package-data"].clone();

        let wifi = json!({
            "interface": db["server-info"]["wifi"]["interface"],
            "ssids": db["server-info"]["wifi"]["ssids"],
            "selected": db["server-info"]["wifi"]["selected"],
            "lastRegion": db["server-info"]["wifi"]["last-region"],
        });

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

            server_info["postInitMigrationTodos"] = json!({});
            let tor_address: String = from_value(db["server-info"]["tor-address"].clone())?;
            // Maybe we do this like the Public::init does
            server_info["torAddress"] = json!(tor_address);
            server_info["onionAddress"] = json!(
                tor_address
                    .replace("https://", "")
                    .replace("http://", "")
                    .replace(".onion/", "")
            );
            server_info["networkInterfaces"] = json!({});
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

        let mut keystore = KeyStore::new(&account)?;
        for key in tor_keys.values().flat_map(|v| v.values()) {
            assert!(key.is_valid());
            keystore.onion.insert(key.clone());
        }

        let private = {
            let mut value = json!({});
            value["keyStore"] = crate::dbg!(to_value(&keystore)?);
            value["password"] = to_value(&account.password)?;
            value["compatS9pkKey"] =
                to_value(&crate::db::model::private::generate_developer_key())?;
            value["sshPrivkey"] = to_value(Pem::new_ref(&account.ssh_key))?;
            value["sshPubkeys"] = to_value(&ssh_keys)?;
            value["availablePorts"] = to_value(&AvailablePorts::new())?;
            value["sessions"] = to_value(&Sessions::new())?;
            value["notifications"] = to_value(&Notifications::new())?;
            value["cifs"] = to_value(&cifs)?;
            value["packageStores"] = json!({});
            value
        };
        let next: Value = json!({
            "public": public,
            "private": private,
        });

        *db = next;

        Ok(prev_package_data)
    }
    fn down(self, _db: &mut Value) -> Result<(), Error> {
        Err(Error::new(
            eyre!("downgrades prohibited"),
            ErrorKind::InvalidRequest,
        ))
    }

    #[instrument(skip(self, ctx))]
    /// MUST be idempotent, and is run after *all* db migrations
    async fn post_up(self, ctx: &RpcContext, input: Value) -> Result<(), Error> {
        let path = Path::new(formatcp!("{PACKAGE_DATA}/archive/"));
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
            let Ok(id) = path.file_name().to_string_lossy().parse::<PackageId>() else {
                continue;
            };
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
                    if path.extension() != Some(OsStr::new("s9pk")) {
                        continue;
                    }

                    let configured = if !input.is_null() {
                        let Some(configured) = input
                            .get(&*id)
                            .and_then(|pde| pde.get("installed"))
                            .and_then(|i| i.get("status"))
                            .and_then(|s| s.get("configured"))
                            .and_then(|c| c.as_bool())
                        else {
                            continue;
                        };
                        configured
                    } else {
                        false
                    };

                    let onions = input[&*id]["installed"]["interface-addresses"]
                        .as_object()
                        .into_iter()
                        .flatten()
                        .filter_map(|(id, addrs)| {
                            addrs["tor-address"].as_str().map(|addr| {
                                Ok((
                                    HostId::from(Id::try_from(id.clone())?),
                                    addr.parse::<OnionAddress>()?,
                                ))
                            })
                        })
                        .collect::<Result<BTreeMap<_, _>, Error>>()?;

                    if let Err(e) = async {
                        let package_s9pk = tokio::fs::File::open(path).await?;
                        let file = MultiCursorFile::open(&package_s9pk).await?;

                        let key = ctx.db.peek().await.into_private().into_developer_key();
                        ctx.services
                            .install(
                                ctx.clone(),
                                || crate::s9pk::load(file.clone(), || Ok(key.de()?.0), None),
                                None,
                                None::<crate::util::Never>,
                                None,
                            )
                            .await?
                            .await?
                            .await?;

                        let to_sync = ctx
                            .db
                            .mutate(|db| {
                                let mut to_sync = BTreeSet::new();

                                let package = db
                                    .as_public_mut()
                                    .as_package_data_mut()
                                    .as_idx_mut(&id)
                                    .or_not_found(&id)?;
                                if configured {
                                    package
                                        .as_tasks_mut()
                                        .remove(&ReplayId::from("needs-config"))?;
                                }
                                for (id, onion) in onions {
                                    package
                                        .as_hosts_mut()
                                        .upsert(&id, || Ok(Host::new()))?
                                        .as_onions_mut()
                                        .mutate(|o| {
                                            o.clear();
                                            o.insert(onion);
                                            Ok(())
                                        })?;
                                    to_sync.insert(id);
                                }
                                Ok(to_sync)
                            })
                            .await
                            .result?;

                        if let Some(service) = &*ctx.services.get(&id).await {
                            for host_id in to_sync {
                                service.sync_host(host_id.clone()).await?;
                            }
                        }

                        Ok::<_, Error>(())
                    }
                    .await
                    {
                        tracing::error!("Error reinstalling {id}: {e}");
                        tracing::debug!("{e:?}");
                    }
                }
            }
        }
        Ok(())
    }
}

#[tracing::instrument(skip_all)]
async fn previous_cifs(pg: &sqlx::Pool<sqlx::Postgres>) -> Result<CifsTargets, Error> {
    let cifs = sqlx::query(r#"SELECT * FROM cifs_shares"#)
        .fetch_all(pg)
        .await
        .with_kind(ErrorKind::Database)?
        .into_iter()
        .map(|row| {
            let id: i32 = row.try_get("id").with_kind(ErrorKind::Database)?;
            Ok::<_, Error>((
                id,
                Cifs {
                    hostname: row
                        .try_get("hostname")
                        .with_ctx(|_| (ErrorKind::Database, "hostname"))?,
                    path: row
                        .try_get::<String, _>("path")
                        .with_ctx(|_| (ErrorKind::Database, "path"))?
                        .into(),
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
        .await
        .with_kind(ErrorKind::Database)?;
    let account = {
        AccountInfo {
            password: account_query
                .try_get("password")
                .with_ctx(|_| (ErrorKind::Database, "password"))?,
            tor_keys: vec![TorSecretKey::from_bytes(
                if let Some(bytes) = account_query
                    .try_get::<Option<Vec<u8>>, _>("tor_key")
                    .with_ctx(|_| (ErrorKind::Database, "tor_key"))?
                {
                    <[u8; 64]>::try_from(bytes).map_err(|e| {
                        Error::new(
                            eyre!("expected vec of len 64, got len {}", e.len()),
                            ErrorKind::ParseDbField,
                        )
                    })?
                } else {
                    ed25519_expand_key(
                        &<[u8; 32]>::try_from(
                            account_query
                                .try_get::<Vec<u8>, _>("network_key")
                                .with_kind(ErrorKind::Database)?,
                        )
                        .map_err(|e| {
                            Error::new(
                                eyre!("expected vec of len 32, got len {}", e.len()),
                                ErrorKind::ParseDbField,
                            )
                        })?,
                    )
                },
            )?],
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
            developer_key: SigningKey::generate(&mut ssh_key::rand_core::OsRng::default()),
            ssh_key: ssh_key::PrivateKey::random(
                &mut ssh_key::rand_core::OsRng::default(),
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
        .await
        .with_kind(ErrorKind::Database)?;
    let ssh_keys: SshKeys = {
        let keys = ssh_query.into_iter().fold(
            Ok::<_, Error>(BTreeMap::<InternedString, WithTimeData<SshPubKey>>::new()),
            |ssh_keys, row| {
                let mut ssh_keys = ssh_keys?;
                let time = row
                    .try_get::<String, _>("created_at")
                    .with_kind(ErrorKind::Database)
                    .and_then(|x| x.parse::<DateTime<Utc>>().with_kind(ErrorKind::Database))
                    .with_ctx(|_| (ErrorKind::Database, "openssh_pubkey::created_at"))?;
                let value: SshPubKey = row
                    .try_get::<String, _>("openssh_pubkey")
                    .with_kind(ErrorKind::Database)
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

#[tracing::instrument(skip_all)]
async fn previous_tor_keys(
    pg: &sqlx::Pool<sqlx::Postgres>,
) -> Result<BTreeMap<PackageId, BTreeMap<HostId, TorSecretKey>>, Error> {
    let mut res = BTreeMap::<PackageId, BTreeMap<HostId, TorSecretKey>>::new();
    let net_key_query = sqlx::query(r#"SELECT * FROM network_keys"#)
        .fetch_all(pg)
        .await
        .with_kind(ErrorKind::Database)?;

    for row in net_key_query {
        let package_id: PackageId = row
            .try_get::<String, _>("package")
            .with_ctx(|_| (ErrorKind::Database, "network_keys::package"))?
            .parse()?;
        let interface_id: HostId = row
            .try_get::<String, _>("interface")
            .with_ctx(|_| (ErrorKind::Database, "network_keys::interface"))?
            .parse()?;
        let key = TorSecretKey::from_bytes(ed25519_expand_key(
            &<[u8; 32]>::try_from(
                row.try_get::<Vec<u8>, _>("key")
                    .with_ctx(|_| (ErrorKind::Database, "network_keys::key"))?,
            )
            .map_err(|e| {
                Error::new(
                    eyre!("expected vec of len 32, got len {}", e.len()),
                    ErrorKind::ParseDbField,
                )
            })?,
        ))?;
        res.entry(package_id).or_default().insert(interface_id, key);
    }

    let tor_key_query = sqlx::query(r#"SELECT * FROM tor"#)
        .fetch_all(pg)
        .await
        .with_kind(ErrorKind::Database)?;

    for row in tor_key_query {
        let package_id: PackageId = row
            .try_get::<String, _>("package")
            .with_ctx(|_| (ErrorKind::Database, "tor::package"))?
            .parse()?;
        let interface_id: HostId = row
            .try_get::<String, _>("interface")
            .with_ctx(|_| (ErrorKind::Database, "tor::interface"))?
            .parse()?;
        let key = TorSecretKey::from_bytes(
            <[u8; 64]>::try_from(
                row.try_get::<Vec<u8>, _>("key")
                    .with_ctx(|_| (ErrorKind::Database, "tor::key"))?,
            )
            .map_err(|e| {
                Error::new(
                    eyre!("expected vec of len 64, got len {}", e.len()),
                    ErrorKind::ParseDbField,
                )
            })?,
        )?;
        res.entry(package_id).or_default().insert(interface_id, key);
    }

    Ok(res)
}
