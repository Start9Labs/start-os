use std::collections::BTreeSet;
use std::net::IpAddr;
use std::path::Path;
use std::time::{Duration, SystemTime};

use chrono::TimeDelta;
use clap::Parser;
use imbl_value::imbl::OrdMap;
use imbl_value::{json, InternedString, Value};
use itertools::Itertools;
use patch_db::value::index::Index;
use patch_db::{HasModel, ModelExt, PatchDb};
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{from_fn_async, CallRemote, Empty, HandlerArgs, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use startos::auth::{AuthKeys, LoginContext};
use startos::middleware::auth::local::LocalAuthContext;
use startos::middleware::auth::signature::{
    check_enrolled, url_host_str, LoginMetadata, NonceCache, SignatureAuthContext,
};
use startos::middleware::auth::DbContext;
use startos::prelude::{Model, TypedPatchDb};
use startos::rpc_continuations::OpenAuthedContinuations;
use startos::sign::AnyVerifyingKey;
use startos::util::sync::SyncMutex;
use tokio::io::AsyncWriteExt;
use tracing::instrument;

use crate::prelude::*;
use crate::utils::DeserializeStdin;
use crate::{captive, CliContext, CtrlContext, ServerContext};

pub const LOCAL_AUTH_COOKIE_PATH: &str = "/run/startwrt/rpc.authcookie";

const AUTH_DB_PATH: &str = "/etc/startwrt/auth.db";

/// Written by dnsmasq's ntp hotplug once ntpd reports a valid time.
const TIME_VALID_PATH: &str = "/var/state/dnsmasqsec";

const IDLE_KEY_SWEEP: Duration = Duration::from_secs(60 * 60);

#[derive(Default, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct AuthDatabase {
    #[serde(default)]
    pub session_pubkeys: AuthKeys,
}
impl HasModel for AuthDatabase {
    type Model = Model<Self>;
}

trait AuthDatabaseModel {
    fn as_session_pubkeys(&self) -> &Model<AuthKeys>;
    fn as_session_pubkeys_mut(&mut self) -> &mut Model<AuthKeys>;
}
impl AuthDatabaseModel for Model<AuthDatabase> {
    fn as_session_pubkeys(&self) -> &Model<AuthKeys> {
        self.transmute_ref(|v| {
            SESSION_PUBKEYS
                .index_into(v)
                .unwrap_or(&patch_db::value::NULL)
        })
    }
    fn as_session_pubkeys_mut(&mut self) -> &mut Model<AuthKeys> {
        self.transmute_mut(|v| SESSION_PUBKEYS.index_or_insert(v))
    }
}
const SESSION_PUBKEYS: &str = "sessionPubkeys";

/// The enabled DDNS hostname as of the ddns config's modification time.
#[derive(Default)]
struct DdnsIdentity {
    modified: Option<SystemTime>,
    hostname: Option<InternedString>,
}

pub struct AuthState {
    db: TypedPatchDb<AuthDatabase>,
    nonce_cache: SyncMutex<NonceCache>,
    open_authed_continuations: OpenAuthedContinuations<Option<InternedString>>,
    ddns: SyncMutex<DdnsIdentity>,
}

impl AuthState {
    /// Enrolls nothing and persists nothing.
    pub fn in_memory() -> Self {
        Self::new(TypedPatchDb::load_unchecked(PatchDb::in_memory()))
    }

    pub async fn open() -> Result<Self, Error> {
        if let Some(dir) = Path::new(AUTH_DB_PATH).parent() {
            tokio::fs::create_dir_all(dir).await?;
        }
        let db = PatchDb::open(AUTH_DB_PATH).await.map_err(|e| {
            Error::new(
                eyre!("Failed to open {AUTH_DB_PATH}: {e}"),
                ErrorKind::Filesystem,
            )
        })?;
        Ok(Self::new(
            TypedPatchDb::load_or_init(db, || async { Ok(AuthDatabase::default()) }).await?,
        ))
    }

    fn new(db: TypedPatchDb<AuthDatabase>) -> Self {
        Self {
            db,
            nonce_cache: SyncMutex::new(NonceCache::default()),
            open_authed_continuations: OpenAuthedContinuations::new(),
            ddns: SyncMutex::new(DdnsIdentity::default()),
        }
    }
}

impl ServerContext {
    /// Re-reads the ddns config only when its modification time changes.
    async fn ddns_identity(&self) -> Option<InternedString> {
        let path = self.uci_root().join("ddns");
        let modified = tokio::fs::metadata(&path)
            .await
            .ok()
            .and_then(|meta| meta.modified().ok());
        let cached = self
            .auth
            .ddns
            .peek(|cache| (cache.modified == modified).then(|| cache.hostname.clone()));
        if let Some(hostname) = cached {
            return hostname;
        }
        let hostname = crate::wan::ddns_get(self.clone())
            .await
            .ok()
            .filter(|ddns| ddns.enabled)
            .and_then(|ddns| ddns.hostname)
            .map(InternedString::from);
        self.auth.ddns.mutate(|cache| {
            *cache = DdnsIdentity {
                modified,
                hostname: hostname.clone(),
            }
        });
        hostname
    }
}

impl AsRef<OpenAuthedContinuations<Option<InternedString>>> for ServerContext {
    fn as_ref(&self) -> &OpenAuthedContinuations<Option<InternedString>> {
        &self.auth.open_authed_continuations
    }
}

impl DbContext for ServerContext {
    type Database = AuthDatabase;
    fn db(&self) -> &TypedPatchDb<Self::Database> {
        &self.auth.db
    }
}

impl SignatureAuthContext for ServerContext {
    type AdditionalMetadata = LoginMetadata;
    type CheckPubkeyRes = Option<InternedString>;
    /// Routine requests never write to flash.
    const LAST_ACTIVE_RESOLUTION: TimeDelta = TimeDelta::hours(1);
    fn mutate_nonce_cache<F: FnOnce(&mut NonceCache) -> T, T>(&self, f: F) -> T {
        self.auth.nonce_cache.mutate(f)
    }
    async fn clock_synced(&self) -> bool {
        tokio::fs::metadata(TIME_VALID_PATH).await.is_ok()
    }
    fn open_authed_continuations(&self) -> &OpenAuthedContinuations<Option<InternedString>> {
        &self.auth.open_authed_continuations
    }
    fn remove_enrolled_keys(
        db: &mut Model<Self::Database>,
        keys: &BTreeSet<InternedString>,
    ) -> Result<(), startos::Error> {
        let auth_keys = db.as_session_pubkeys_mut();
        for key in keys {
            auth_keys.remove(key)?;
        }
        Ok(())
    }
    async fn sig_context(
        &self,
        _: &http::Extensions,
    ) -> impl IntoIterator<Item = Result<impl AsRef<str> + Send, startos::Error>> + Send {
        signing_identities(interface_addresses(), self.ddns_identity().await)
            .into_iter()
            .map(Ok)
    }
    fn check_pubkey(
        &self,
        db: &Model<Self::Database>,
        pubkey: Option<&AnyVerifyingKey>,
        metadata: Self::AdditionalMetadata,
    ) -> Result<Self::CheckPubkeyRes, startos::Error> {
        check_enrolled(pubkey, metadata.login, |key| {
            Ok(db.as_session_pubkeys().de()?.0.contains_key(&**key))
        })
    }
    async fn post_auth_hook(
        &self,
        key: Self::CheckPubkeyRes,
        _: &rpc_toolkit::RpcRequest,
    ) -> Result<(), startos::Error> {
        if let Some(key) = key {
            startos::auth::touch_last_active(self, &key).await?;
        }
        Ok(())
    }
}

impl LocalAuthContext for ServerContext {
    const LOCAL_AUTH_COOKIE_PATH: &str = LOCAL_AUTH_COOKIE_PATH;
    const LOCAL_AUTH_COOKIE_OWNERSHIP: &str = "root:root";
}

impl LoginContext for ServerContext {
    fn access_auth_keys(db: &mut Model<Self::Database>) -> &mut Model<AuthKeys> {
        db.as_session_pubkeys_mut()
    }
    fn check_password(_: &Model<Self::Database>, password: &str) -> Result<(), startos::Error> {
        check_password_sync(password).map_err(|e| {
            crate::activity::log("auth", "login", false, "Login failed", Some(&e.to_string()));
            e.into()
        })
    }
    async fn post_login_hook(&self, _: &str) -> Result<(), startos::Error> {
        crate::activity::log("auth", "login", true, "Logged in", None);
        Ok(())
    }
}

/// Unenrolls idle keys for the daemon's lifetime.
pub async fn reap_idle_keys(ctx: ServerContext) {
    loop {
        if let Err(e) = startos::auth::reap_idle_keys(&ctx).await {
            tracing::error!("idle key sweep failed: {e}");
        }
        tokio::time::sleep(IDLE_KEY_SWEEP).await;
    }
}

/// Every name and address a browser may sign for.
fn signing_identities(
    addresses: impl IntoIterator<Item = IpAddr>,
    ddns: Option<InternedString>,
) -> BTreeSet<InternedString> {
    std::iter::once(InternedString::intern(crate::ssl::ROUTER_HOSTNAME))
        .chain(addresses.into_iter().map(url_host_str))
        .chain(ddns.map(|hostname| InternedString::intern(hostname.to_ascii_lowercase())))
        .collect()
}

/// Excludes loopback, which start-core grants to loopback peers alone, and
/// link-local IPv6, which a URL can only name with a zone.
fn interface_addresses() -> Vec<IpAddr> {
    let Ok(addrs) = nix::ifaddrs::getifaddrs() else {
        return Vec::new();
    };
    addrs
        .filter_map(|ifaddr| {
            let addr = ifaddr.address?;
            if let Some(v4) = addr.as_sockaddr_in() {
                Some(IpAddr::V4(v4.ip()))
            } else {
                addr.as_sockaddr_in6()
                    .map(|v6| v6.ip())
                    .filter(|ip| !ip.is_unicast_link_local())
                    .map(IpAddr::V6)
            }
        })
        .filter(|ip| !ip.is_unspecified() && !ip.is_loopback())
        .collect()
}

/// `None` when the account has no password or is locked.
fn get_shadow_hash(username: &str) -> Result<Option<String>, Error> {
    let shadow_content = std::fs::read_to_string("/etc/shadow").map_err(|e| {
        Error::new(
            eyre!("Failed to read /etc/shadow: {}", e),
            ErrorKind::Filesystem,
        )
    })?;

    for line in shadow_content.lines() {
        let parts: Vec<&str> = line.split(':').collect();
        if parts.len() >= 2 && parts[0] == username {
            let hash = parts[1];
            if hash.is_empty() || hash == "*" || hash == "!" || hash == "x" {
                return Ok(None);
            }
            return Ok(Some(hash.to_string()));
        }
    }

    Ok(None)
}

const MIN_PASSWORD_LEN: usize = 12;

fn validate_password_length(password: &str) -> Result<(), Error> {
    if password.len() < MIN_PASSWORD_LEN {
        return Err(Error::new(
            eyre!("password must be at least {MIN_PASSWORD_LEN} characters"),
            ErrorKind::InvalidRequest,
        ));
    }
    Ok(())
}

/// `STARTWRT_DEV_PASSWORD` replaces the shadow check for development without root.
fn check_password_sync(password: &str) -> Result<(), Error> {
    if let Ok(dev_password) = std::env::var("STARTWRT_DEV_PASSWORD") {
        tracing::warn!("DEV MODE: Using development password authentication");
        if password == dev_password {
            return Ok(());
        }
        return Err(Error::new(
            eyre!("Incorrect password"),
            ErrorKind::IncorrectPassword,
        ));
    }

    match get_shadow_hash("root")? {
        // Root has no password until setup sets one; a factory reset clears it.
        None => Ok(()),
        Some(hash) => {
            if pwhash::unix::verify(password, &hash) {
                Ok(())
            } else {
                Err(Error::new(
                    eyre!("Incorrect password"),
                    ErrorKind::IncorrectPassword,
                ))
            }
        }
    }
}

pub async fn check_password(password: &str) -> Result<(), Error> {
    let password = password.to_owned();
    tokio::task::spawn_blocking(move || check_password_sync(&password)).await?
}

#[derive(Debug, Clone, Deserialize, Serialize, Parser, visit_rs::VisitFields)]
#[serde(rename_all = "camelCase")]
pub struct VerifyPasswordParams {
    pub password: String,
}
rpc_toolkit::reflect_ts!(VerifyPasswordParams);

#[instrument(skip_all)]
pub async fn verify_password_impl(
    _ctx: ServerContext,
    VerifyPasswordParams { password }: VerifyPasswordParams,
) -> Result<(), Error> {
    check_password(&password).await
}

#[derive(Debug, Clone, Deserialize, Serialize, Parser, visit_rs::VisitFields)]
#[serde(rename_all = "camelCase")]
pub struct ResetPasswordParams {
    pub old_password: String,
    pub new_password: String,
}
rpc_toolkit::reflect_ts!(ResetPasswordParams);

async fn update_shadow_hash(username: &str, new_hash: &str) -> Result<(), Error> {
    const SHADOW_PATH: &str = "/etc/shadow";
    let shadow_path = Path::new(SHADOW_PATH);

    let shadow_content = tokio::fs::read_to_string(SHADOW_PATH).await.map_err(|e| {
        Error::new(
            eyre!("Failed to read /etc/shadow: {e}"),
            ErrorKind::Filesystem,
        )
    })?;

    let mut found = false;
    let new_content = shadow_content
        .lines()
        .map(|line| {
            if line.starts_with(&format!("{username}:")) {
                found = true;
                let mut parts: Vec<&str> = line.split(':').collect();
                if parts.len() >= 2 {
                    parts[1] = new_hash;
                }
                parts.join(":")
            } else {
                line.to_string()
            }
        })
        .collect::<Vec<_>>()
        .join("\n");

    if !found {
        return Err(Error::new(
            eyre!("User '{username}' not found in /etc/shadow"),
            ErrorKind::NotFound,
        ));
    }

    let new_content = if new_content.ends_with('\n') {
        new_content
    } else {
        format!("{new_content}\n")
    };

    use std::os::unix::fs::PermissionsExt;
    let mut file = startos::util::io::AtomicFile::new(shadow_path, None::<&Path>)
        .await
        .map_err(Error::from)?;
    file.set_permissions(std::fs::Permissions::from_mode(0o600))
        .await
        .map_err(|e| {
            Error::new(
                eyre!("Failed to set shadow file permissions: {e}"),
                ErrorKind::Filesystem,
            )
        })?;
    file.write_all(new_content.as_bytes()).await.map_err(|e| {
        Error::new(
            eyre!("Failed to write shadow file: {e}"),
            ErrorKind::Filesystem,
        )
    })?;
    file.save().await.map_err(Error::from)?;

    Ok(())
}

#[instrument(skip_all)]
pub async fn reset_password_impl(
    _ctx: ServerContext,
    ResetPasswordParams {
        old_password,
        new_password,
    }: ResetPasswordParams,
) -> Result<(), Error> {
    check_password(&old_password).await?;
    validate_password_length(&new_password)?;

    let new_hash = pwhash::sha512_crypt::hash(&new_password).map_err(|e| {
        Error::new(
            eyre!("Failed to hash password: {e}"),
            ErrorKind::PasswordHashGeneration,
        )
    })?;

    let result = update_shadow_hash("root", &new_hash).await;
    match &result {
        Ok(()) => crate::activity::log(
            "auth",
            "password-changed",
            true,
            "Changed admin password",
            None,
        ),
        Err(e) => crate::activity::log(
            "auth",
            "password-changed",
            false,
            "Failed to change admin password",
            Some(&e.to_string()),
        ),
    }
    result
}

#[instrument(skip_all)]
async fn cli_reset_password(
    HandlerArgs {
        context: ctx,
        parent_method,
        method,
        ..
    }: HandlerArgs<CliContext>,
) -> Result<(), RpcError> {
    let old_password = rpassword::prompt_password("Current Password: ")?;

    ctx.call_remote(
        "auth.verify-password",
        OrdMap::new(),
        json!({ "password": old_password }),
        Empty {},
    )
    .await?;

    let new_password = rpassword::prompt_password("New Password: ")?;
    let confirm = rpassword::prompt_password("Confirm Password: ")?;

    if new_password != confirm {
        return Err(Error::new(eyre!("Passwords do not match"), ErrorKind::InvalidRequest).into());
    }

    ctx.call_remote(
        &parent_method.into_iter().chain(method).join("."),
        OrdMap::new(),
        json!({
            "oldPassword": old_password,
            "newPassword": new_password,
        }),
        Empty {},
    )
    .await?;

    println!("Password changed successfully.");
    Ok(())
}

#[derive(Debug, Clone, Deserialize, Serialize, visit_rs::VisitFields)]
#[serde(rename_all = "camelCase")]
pub struct CheckInitializedRes {
    pub initialized: bool,
}
rpc_toolkit::reflect_ts!(CheckInitializedRes);

#[instrument(skip_all)]
pub async fn check_initialized_impl(_ctx: ServerContext) -> Result<CheckInitializedRes, Error> {
    let has_password = tokio::task::spawn_blocking(|| get_shadow_hash("root")).await??;
    Ok(CheckInitializedRes {
        initialized: has_password.is_some(),
    })
}

#[derive(Debug, Clone, Deserialize, Serialize, Parser, visit_rs::VisitFields)]
#[serde(rename_all = "camelCase")]
pub struct SetInitialPasswordParams {
    pub password: String,
    /// IANA timezone name, e.g. "America/New_York". An unknown name is skipped.
    #[serde(default)]
    pub timezone: Option<String>,
}
rpc_toolkit::reflect_ts!(SetInitialPasswordParams);

#[instrument(skip_all)]
pub async fn set_initial_password_impl(
    ctx: ServerContext,
    SetInitialPasswordParams { password, timezone }: SetInitialPasswordParams,
) -> Result<(), Error> {
    if tokio::task::spawn_blocking(|| get_shadow_hash("root"))
        .await??
        .is_some()
    {
        return Err(Error::new(
            eyre!("admin password is already set"),
            ErrorKind::InvalidRequest,
        ));
    }

    validate_password_length(&password)?;

    let new_hash = pwhash::sha512_crypt::hash(&password).map_err(|e| {
        Error::new(
            eyre!("failed to hash password: {e}"),
            ErrorKind::PasswordHashGeneration,
        )
    })?;
    update_shadow_hash("root", &new_hash).await?;

    if let Some(timezone) = timezone {
        let params = crate::system::SetTimezoneParams { timezone };
        if let Err(e) = crate::system::set_timezone(ctx, DeserializeStdin(params)).await {
            tracing::warn!("failed to set the setup timezone: {e}");
        }
    }

    captive::disable_captive_portal().await.map_err(|e| {
        tracing::error!("failed to disable captive portal: {e}");
        e
    })?;

    Ok(())
}

pub fn auth<C: CtrlContext>() -> ParentHandler<C> {
    startos::auth::auth::<C, ServerContext, CliContext>()
        .subcommand(
            "verify-password",
            from_fn_async(verify_password_impl).no_cli(),
        )
        .subcommand("set-password", from_fn_async(reset_password_impl).no_cli())
        .subcommand(
            "set-password",
            from_fn_async(cli_reset_password)
                .no_display()
                .with_about("Reset root password"),
        )
        .subcommand(
            "check-initialized",
            from_fn_async(check_initialized_impl)
                .with_metadata("authenticated", Value::Bool(false))
                .no_cli(),
        )
        .subcommand(
            "set-initial-password",
            from_fn_async(set_initial_password_impl)
                .with_metadata("authenticated", Value::Bool(false))
                .no_cli(),
        )
}

#[cfg(test)]
mod tests {
    use chrono::Utc;

    use super::*;

    const LAST_ACTIVE_RESOLUTION: TimeDelta =
        <ServerContext as SignatureAuthContext>::LAST_ACTIVE_RESOLUTION;

    #[test]
    fn validate_password_length_rejects_short() {
        assert!(validate_password_length("shortpass12").is_err());
        assert!(validate_password_length("").is_err());
    }

    #[test]
    fn validate_password_length_accepts_min_and_above() {
        assert!(validate_password_length("exactly12chr").is_ok());
        assert!(validate_password_length("a-longer-passphrase").is_ok());
    }

    #[test]
    fn signing_identities_cover_router_name_addresses_and_ddns() {
        let identities = signing_identities(
            ["192.168.0.1".parse().unwrap(), "fd00::1".parse().unwrap()],
            Some("home.example.com".into()),
        );
        let expected: BTreeSet<InternedString> =
            ["router.lan", "192.168.0.1", "[fd00::1]", "home.example.com"]
                .into_iter()
                .map(InternedString::intern)
                .collect();
        assert_eq!(identities, expected);
    }

    #[test]
    fn ddns_signing_identity_matches_browser_hostname_case() {
        let identities = signing_identities([], Some("Home.Example.COM".into()));
        assert!(identities.contains("home.example.com"));
    }

    #[tokio::test]
    async fn idle_sweep_spares_refreshed_key() {
        let ctx = ServerContext::default();
        let expired = Utc::now() - startos::auth::MAX_IDLE - TimeDelta::minutes(1);
        let keys = ["idle", "refreshed"]
            .into_iter()
            .map(|key| {
                (
                    InternedString::intern(key),
                    startos::auth::Session {
                        last_active: expired,
                        ..Default::default()
                    },
                )
            })
            .collect();
        ctx.auth
            .db
            .put(
                &patch_db::json_ptr::ROOT,
                &AuthDatabase {
                    session_pubkeys: AuthKeys(keys),
                },
            )
            .await
            .unwrap();
        startos::auth::touch_last_active(&ctx, &InternedString::intern("refreshed"))
            .await
            .unwrap();
        let continuations = &ctx.auth.open_authed_continuations;
        ctx.auth
            .db
            .mutate(|db| startos::auth::reap_idle_keys_in::<ServerContext>(continuations, None, db))
            .await
            .result
            .unwrap();
        let remaining = ctx.auth.db.peek().await.as_session_pubkeys().de().unwrap();
        assert_eq!(remaining.0.len(), 1);
        assert!(remaining.0.contains_key("refreshed"));
    }

    #[tokio::test]
    async fn last_active_persists_only_once_stale() {
        let ctx = ServerContext::default();
        ctx.auth
            .db
            .put(&patch_db::json_ptr::ROOT, &AuthDatabase::default())
            .await
            .unwrap();
        let key = InternedString::intern("key");
        let enrolled = Utc::now() - TimeDelta::minutes(5);
        ctx.auth
            .db
            .mutate(|db| {
                db.as_session_pubkeys_mut().insert(
                    &key,
                    &startos::auth::Session {
                        logged_in: enrolled,
                        last_active: enrolled,
                        ..Default::default()
                    },
                )
            })
            .await
            .result
            .unwrap();
        let request = rpc_toolkit::RpcRequest {
            id: None,
            method: rpc_toolkit::yajrc::GenericRpcMethod::new("x".into()),
            params: Value::Null,
        };
        let last_active = |ctx: &ServerContext| {
            let ctx = ctx.clone();
            let key = key.clone();
            async move {
                ctx.auth
                    .db
                    .peek()
                    .await
                    .as_session_pubkeys()
                    .de()
                    .unwrap()
                    .0[&key]
                    .last_active
            }
        };

        ctx.post_auth_hook(Some(key.clone()), &request)
            .await
            .unwrap();
        assert_eq!(last_active(&ctx).await, enrolled);

        let stale = Utc::now() - LAST_ACTIVE_RESOLUTION - TimeDelta::minutes(1);
        ctx.auth
            .db
            .mutate(|db| {
                db.as_session_pubkeys_mut().mutate(|keys| {
                    keys.0.get_mut(&key).unwrap().last_active = stale;
                    Ok(())
                })
            })
            .await
            .result
            .unwrap();
        ctx.post_auth_hook(Some(key.clone()), &request)
            .await
            .unwrap();
        assert!(last_active(&ctx).await > stale + LAST_ACTIVE_RESOLUTION);
    }
}
