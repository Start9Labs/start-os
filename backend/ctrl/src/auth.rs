use std::collections::BTreeMap;
use std::path::Path;
use std::sync::LazyLock;

use tokio::io::AsyncWriteExt;

use chrono::{DateTime, TimeDelta, Utc};
use clap::Parser;
use imbl_value::imbl::OrdMap;
use imbl_value::{json, Value};
use itertools::Itertools;
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{
    from_fn_async, CallRemote, Context, Empty, HandlerArgs, HandlerExt, ParentHandler,
};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use tracing::instrument;

use crate::captive;
use crate::error::Error;
use crate::{CliContext, ServerContext};

const DEFAULT_SESSION_FILE_PATH: &str = "/var/run/startwrt/sessions.json";
const SESSION_EXPIRY_DAYS: i64 = 1;

static SESSION_FILE_PATH: LazyLock<String> = LazyLock::new(|| {
    std::env::var("STARTWRT_SESSION_PATH").unwrap_or_else(|_| DEFAULT_SESSION_FILE_PATH.to_string())
});

pub mod error_code {
    pub const INCORRECT_PASSWORD: i32 = 7;
    pub const AUTHORIZATION: i32 = 34;
    pub const UNINITIALIZED: i32 = 40;
}

#[derive(Debug, Clone)]
pub struct HashSessionToken {
    hashed: String,
    token: String,
}

impl HashSessionToken {
    pub fn new() -> Self {
        Self::from_token(
            base32::encode(
                base32::Alphabet::Rfc4648 { padding: false },
                &rand::random::<[u8; 16]>(),
            )
            .to_lowercase(),
        )
    }

    pub fn from_token(token: String) -> Self {
        let hashed = Self::hash(&token);
        Self { hashed, token }
    }

    pub fn to_login_res(&self) -> LoginRes {
        LoginRes {
            session: self.token.clone(),
        }
    }

    pub fn hashed(&self) -> &str {
        &self.hashed
    }

    fn hash(token: &str) -> String {
        let mut hasher = Sha256::new();
        hasher.update(token.as_bytes());
        base32::encode(
            base32::Alphabet::Rfc4648 { padding: false },
            hasher.finalize().as_slice(),
        )
        .to_lowercase()
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Session {
    pub logged_in: DateTime<Utc>,
    pub last_active: DateTime<Utc>,
    pub user_agent: Option<String>,
}

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct LoginRes {
    pub session: String,
}

#[derive(Debug, Clone, Deserialize, Serialize, Parser)]
#[serde(rename_all = "camelCase")]
pub struct LoginParams {
    pub password: String,
    #[serde(default)]
    pub user_agent: Option<String>,
}

/// Read and parse the shadow file to get the password hash for a user
async fn get_shadow_hash(username: &str) -> Result<Option<String>, Error> {
    let shadow_content = tokio::fs::read_to_string("/etc/shadow")
        .await
        .map_err(|e| Error::other(format!("Failed to read /etc/shadow: {}", e)))?;

    for line in shadow_content.lines() {
        let parts: Vec<&str> = line.split(':').collect();
        if parts.len() >= 2 && parts[0] == username {
            let hash = parts[1];
            // Empty hash, *, or ! means no password set or account disabled
            if hash.is_empty() || hash == "*" || hash == "!" || hash == "x" {
                return Ok(None);
            }
            return Ok(Some(hash.to_string()));
        }
    }

    Ok(None)
}

/// Check password against /etc/shadow (or dev password if STARTWRT_DEV_PASSWORD is set)
pub async fn check_password(password: &str) -> Result<(), Error> {
    // Dev mode: check against env var password (for development without root access)
    if let Ok(dev_password) = std::env::var("STARTWRT_DEV_PASSWORD") {
        tracing::warn!("DEV MODE: Using development password authentication");
        if password == dev_password {
            return Ok(());
        }
        return Err(Error::other("Incorrect password"));
    }

    let hash = get_shadow_hash("root").await?;

    match hash {
        None => {
            /*
                start-wrt will be initialized with a password which will persist across factory resets.

                However in the case of a reflash, the root user will have no password;
                Which is the default for openwrt
            */
            println!("No password has been set. Please set a password.");
            Ok(())
        }
        Some(hash) => {
            if pwhash::unix::verify(password, &hash) {
                Ok(())
            } else {
                Err(Error::other("Incorrect password"))
            }
        }
    }
}

/// Map of session token hashes to session data
type Sessions = BTreeMap<String, Session>;

/// In-memory session store, lazy-initialized from disk on first access.
/// Authoritative after startup — disk is only written on login/logout for durability.
static SESSION_STORE: LazyLock<tokio::sync::RwLock<Sessions>> = LazyLock::new(|| {
    let sessions = std::fs::read_to_string(&*SESSION_FILE_PATH)
        .ok()
        .and_then(|c| serde_json::from_str(&c).ok())
        .unwrap_or_default();
    tokio::sync::RwLock::new(sessions)
});

/// Persist the in-memory session store to disk (atomic temp + rename).
/// Fire-and-forget — called from login/logout only.
async fn persist_sessions() {
    let sessions = SESSION_STORE.read().await;
    let content = match serde_json::to_string_pretty(&*sessions) {
        Ok(c) => c,
        Err(e) => {
            tracing::warn!("Failed to serialize sessions: {e}");
            return;
        }
    };
    drop(sessions);

    let path = Path::new(&*SESSION_FILE_PATH);
    let parent = match path.parent() {
        Some(p) => p,
        None => {
            tracing::warn!("Invalid session file path");
            return;
        }
    };
    let file_name = match path.file_name().and_then(|f| f.to_str()) {
        Some(f) => f,
        None => {
            tracing::warn!("Invalid session file name");
            return;
        }
    };
    let tmp_path = parent.join(format!(".{file_name}.tmp"));

    if let Err(e) = tokio::fs::create_dir_all(parent).await {
        tracing::warn!("Failed to create session directory: {e}");
        return;
    }

    let write_result: Result<(), std::io::Error> = async {
        let mut file = tokio::fs::OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .mode(0o600)
            .open(&tmp_path)
            .await?;
        file.write_all(content.as_bytes()).await?;
        file.flush().await?;
        file.sync_all().await?;
        tokio::fs::rename(&tmp_path, path).await?;
        Ok(())
    }
    .await;

    if let Err(e) = write_result {
        tracing::warn!("Failed to persist sessions to disk: {e}");
    }
}

async fn add_session(token_hash: &str, session: Session) -> Result<(), Error> {
    {
        let mut sessions = SESSION_STORE.write().await;
        sessions.insert(token_hash.to_string(), session);
    }
    tokio::spawn(persist_sessions());
    Ok(())
}

async fn remove_session(token_hash: &str) -> Result<(), Error> {
    {
        let mut sessions = SESSION_STORE.write().await;
        sessions.remove(token_hash);
    }
    tokio::spawn(persist_sessions());
    Ok(())
}

/// Validate a session token hash, check expiration, and update last_active timestamp.
/// Operates entirely in-memory — no disk I/O.
pub async fn validate_session(token_hash: &str) -> Result<(), Error> {
    let mut sessions = SESSION_STORE.write().await;

    let session = sessions
        .get(token_hash)
        .ok_or_else(|| Error::other("Invalid session token"))?;

    // Check if session has expired
    if let Some(expiry) = TimeDelta::try_days(SESSION_EXPIRY_DAYS) {
        if Utc::now() - session.last_active > expiry {
            sessions.remove(token_hash);
            // Fire-and-forget persist for expired session cleanup
            let sessions_clone = sessions.clone();
            drop(sessions);
            tokio::spawn(async move {
                let _ = sessions_clone; // ensure removal is reflected if we persist later
                persist_sessions().await;
            });
            return Err(Error::other("Session expired"));
        }
    }

    // Update last_active timestamp — in-memory only, no disk write
    let updated_session = Session {
        last_active: Utc::now(),
        ..session.clone()
    };
    sessions.insert(token_hash.to_string(), updated_session);

    Ok(())
}

#[instrument(skip_all)]
pub async fn login_impl(
    _ctx: ServerContext,
    LoginParams {
        password,
        user_agent,
    }: LoginParams,
) -> Result<LoginRes, Error> {
    check_password(&password).await?;

    let hash_token = HashSessionToken::new();
    let session = Session {
        logged_in: Utc::now(),
        last_active: Utc::now(),
        user_agent,
    };

    add_session(hash_token.hashed(), session).await?;

    Ok(hash_token.to_login_res())
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "camelCase")]
pub struct LogoutParams {
    /// Session hash injected by auth middleware
    #[serde(rename = "sessionHash")]
    #[arg(skip)]
    pub session_hash: Option<String>,
}

#[instrument(skip_all)]
pub async fn logout(
    _ctx: ServerContext,
    LogoutParams { session_hash }: LogoutParams,
) -> Result<(), Error> {
    if let Some(hash) = session_hash {
        remove_session(&hash).await?;
    }
    Ok(())
}

#[derive(Debug, Clone, Deserialize, Serialize, Parser)]
#[serde(rename_all = "camelCase")]
pub struct VerifyPasswordParams {
    pub password: String,
}

#[instrument(skip_all)]
pub async fn verify_password_impl(
    _ctx: ServerContext,
    VerifyPasswordParams { password }: VerifyPasswordParams,
) -> Result<(), Error> {
    check_password(&password).await
}

#[derive(Debug, Clone, Deserialize, Serialize, Parser)]
#[serde(rename_all = "camelCase")]
pub struct ResetPasswordParams {
    pub old_password: String,
    pub new_password: String,
}

/// Update a user's password hash in /etc/shadow
async fn update_shadow_hash(username: &str, new_hash: &str) -> Result<(), Error> {
    const SHADOW_PATH: &str = "/etc/shadow";
    let shadow_path = Path::new(SHADOW_PATH);
    let tmp_path = shadow_path
        .parent()
        .unwrap_or(Path::new("/etc"))
        .join(".shadow.tmp");

    let shadow_content = tokio::fs::read_to_string(SHADOW_PATH)
        .await
        .map_err(|e| Error::other(format!("Failed to read /etc/shadow: {e}")))?;

    // Update the user's hash line
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
        return Err(Error::other(format!("User '{username}' not found in /etc/shadow")));
    }

    // Ensure file ends with newline
    let new_content = if new_content.ends_with('\n') {
        new_content
    } else {
        format!("{new_content}\n")
    };

    // Write atomically with restricted permissions (0600)
    let mut file = tokio::fs::OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .mode(0o600)
        .open(&tmp_path)
        .await
        .map_err(|e| Error::other(format!("Failed to create temp shadow file: {e}")))?;

    file.write_all(new_content.as_bytes())
        .await
        .map_err(|e| Error::other(format!("Failed to write shadow file: {e}")))?;

    file.flush()
        .await
        .map_err(|e| Error::other(format!("Failed to flush shadow file: {e}")))?;

    file.sync_all()
        .await
        .map_err(|e| Error::other(format!("Failed to sync shadow file: {e}")))?;

    tokio::fs::rename(&tmp_path, shadow_path)
        .await
        .map_err(|e| Error::other(format!("Failed to rename shadow file: {e}")))?;

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
    // Verify old password first
    check_password(&old_password).await?;

    // Generate new password hash using SHA-512 crypt
    let new_hash = pwhash::sha512_crypt::hash(&new_password)
        .map_err(|e| Error::other(format!("Failed to hash password: {e}")))?;

    // Update /etc/shadow directly
    update_shadow_hash("root", &new_hash).await?;

    Ok(())
}

#[instrument(skip_all)]
async fn cli_login(
    HandlerArgs {
        context: ctx,
        parent_method,
        method,
        ..
    }: HandlerArgs<CliContext>,
) -> Result<(), RpcError> {
    let password = rpassword::prompt_password("Password: ")?;

    ctx.call_remote(
        &parent_method.into_iter().chain(method).join("."),
        OrdMap::new(),
        json!({
            "password": password,
        }),
        Empty {},
    )
    .await?;

    println!("Login successful. Session saved.");
    Ok(())
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

    // Verify old password before prompting for new one
    ctx.call_remote("auth.verify-password", OrdMap::new(), json!({ "password": old_password }), Empty {})
        .await?;

    // Old password verified, now prompt for new password
    let new_password = rpassword::prompt_password("New Password: ")?;
    let confirm = rpassword::prompt_password("Confirm Password: ")?;

    if new_password != confirm {
        return Err(Error::other("Passwords do not match").into());
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

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct CheckInitializedRes {
    pub initialized: bool,
}

#[instrument(skip_all)]
pub async fn check_initialized_impl(
    _ctx: ServerContext,
) -> Result<CheckInitializedRes, Error> {
    let has_password = get_shadow_hash("root").await?.is_some();
    Ok(CheckInitializedRes {
        initialized: has_password,
    })
}

#[derive(Debug, Clone, Deserialize, Serialize, Parser)]
#[serde(rename_all = "camelCase")]
pub struct SetInitialPasswordParams {
    pub password: String,
}

#[instrument(skip_all)]
pub async fn set_initial_password_impl(
    _ctx: ServerContext,
    SetInitialPasswordParams { password }: SetInitialPasswordParams,
) -> Result<LoginRes, Error> {
    // Reject if password already set
    if get_shadow_hash("root").await?.is_some() {
        return Err(Error::other("admin password is already set"));
    }

    // Validate minimum length
    if password.len() < 12 {
        return Err(Error::other("password must be at least 12 characters"));
    }

    // Hash and write to shadow
    let new_hash = pwhash::sha512_crypt::hash(&password)
        .map_err(|e| Error::other(format!("failed to hash password: {e}")))?;
    update_shadow_hash("root", &new_hash).await?;

    // Disable captive portal now that password is set
    tokio::task::spawn_blocking(|| captive::disable_captive_portal())
        .await
        .map_err(|e| Error::other(format!("captive portal task panicked: {e}")))?
        .map_err(|e| {
            tracing::error!("failed to disable captive portal: {e}");
            e
        })?;

    // Create login session so user is immediately authenticated
    let hash_token = HashSessionToken::new();
    let session = Session {
        logged_in: Utc::now(),
        last_active: Utc::now(),
        user_agent: None,
    };
    add_session(hash_token.hashed(), session).await?;

    Ok(hash_token.to_login_res())
}

pub fn auth<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        // RPC/HTTP login endpoint (hidden from CLI)
        .subcommand(
            "login",
            from_fn_async(login_impl)
                .with_metadata("login", Value::Bool(true))
                .no_cli(),
        )
        // CLI login endpoint (prompts for password, calls RPC)
        .subcommand(
            "login",
            from_fn_async(cli_login)
                .no_display()
                .with_about("Log in a new auth session"),
        )
        // Logout endpoint - works for both RPC and CLI
        .subcommand(
            "logout",
            from_fn_async(logout)
                .with_metadata("get_session", Value::Bool(true))
                .no_display()
                .with_about("Log out of current auth session")
                .with_call_remote::<CliContext>(),
        )
        // Verify password endpoint (RPC only, used internally by CLI)
        .subcommand(
            "verify-password",
            from_fn_async(verify_password_impl).no_cli(),
        )
        // RPC/HTTP set-password endpoint (hidden from CLI)
        .subcommand(
            "set-password",
            from_fn_async(reset_password_impl).no_cli(),
        )
        // CLI set-password endpoint (prompts for passwords, calls RPC)
        .subcommand(
            "set-password",
            from_fn_async(cli_reset_password)
                .no_display()
                .with_about("Reset root password"),
        )
        // Check if device has been initialized (password set) — bypasses auth
        .subcommand(
            "check-initialized",
            from_fn_async(check_initialized_impl)
                .with_metadata("no_auth", Value::Bool(true))
                .no_cli(),
        )
        // Set initial password (first-time setup) — bypasses auth
        .subcommand(
            "set-initial-password",
            from_fn_async(set_initial_password_impl)
                .with_metadata("login", Value::Bool(true))
                .no_cli(),
        )
}
