use std::collections::BTreeMap;
use std::fs::{self, File};
use std::io::Write;
use std::path::Path;

use chrono::{DateTime, TimeDelta, Utc};
use clap::Parser;
use imbl_value::{json, Value};
use itertools::Itertools;
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{from_fn_async, CallRemote, Context, Empty, HandlerArgs, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use tracing::instrument;

use crate::error::Error;
use crate::{CliContext, ServerContext};

const DEFAULT_SESSION_FILE_PATH: &str = "/var/run/startwrt/sessions.json";
const SESSION_EXPIRY_DAYS: i64 = 1;

fn session_file_path() -> String {
    std::env::var("STARTWRT_SESSION_PATH").unwrap_or_else(|_| DEFAULT_SESSION_FILE_PATH.to_string())
}

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
fn get_shadow_hash(username: &str) -> Result<Option<String>, Error> {
    let shadow_content = fs::read_to_string("/etc/shadow").map_err(|e| {
        Error::other(format!("Failed to read /etc/shadow: {}", e))
    })?;

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

/// Verify a password against the shadow file hash
fn verify_password(password: &str, hash: &str) -> Result<bool, Error> {
    // @TODO remove branches once a default algo is selected for start-wrt
    if hash.starts_with("$1$") {
        // MD5
        Ok(pwhash::md5_crypt::verify(password, hash))
    } else if hash.starts_with("$5$") {
        // SHA-256
        Ok(pwhash::sha256_crypt::verify(password, hash))
    } else if hash.starts_with("$6$") {
        // SHA-512
        Ok(pwhash::sha512_crypt::verify(password, hash))
    } else {
        Err(Error::other(format!(
            "Unsupported password hash format: {}",
            hash.chars().take(3).collect::<String>()
        )))
    }
}

/// Check password against /etc/shadow (or dev password if STARTWRT_DEV_PASSWORD is set)
pub fn check_password(password: &str) -> Result<(), Error> {
    // Dev mode: check against env var password (for development without root access)
    if let Ok(dev_password) = std::env::var("STARTWRT_DEV_PASSWORD") {
        tracing::warn!("DEV MODE: Using development password authentication");
        if password == dev_password {
            return Ok(());
        }
        return Err(Error::other("Incorrect password"));
    }

    let hash = get_shadow_hash("root")?;

    match hash {
        None => {
            /*
                start-wrt will be initialized with a password which will persist across "soft" factory resets.

                However in the case of a "hard" factory reset, in the case of a lost password, the root user will have no password;
                Which is the default for openwrt
            */
            println!("No password has been set. Please set a password.");
            Ok(())
        }
        Some(hash) => {
            let valid = verify_password(password, &hash)?;
            if !valid {
                Err(Error::other("Incorrect password"))
            } else {
                Ok(())
            }
        }
    }
}

/// Map of session token hashes to session data
type Sessions = BTreeMap<String, Session>;

fn load_sessions() -> Result<Sessions, Error> {
    let path_str = session_file_path();
    let path = Path::new(&path_str);

    if !path.exists() {
        return Ok(Sessions::new());
    }

    let content = fs::read_to_string(path).map_err(|e| {
        Error::other(format!("Failed to read sessions file: {}", e))
    })?;

    let sessions: Sessions = serde_json::from_str(&content).map_err(|e| {
        Error::other(format!("Failed to parse sessions file: {}", e))
    })?;

    Ok(sessions)
}

fn save_sessions(sessions: &Sessions) -> Result<(), Error> {
    let path_str = session_file_path();
    let path = Path::new(&path_str);

    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).map_err(|e| {
            Error::other(format!("Failed to create session directory: {}", e))
        })?;
    }

    let content = serde_json::to_string_pretty(sessions).map_err(|e| {
        Error::other(format!("Failed to serialize sessions: {}", e))
    })?;

    let mut file = File::create(path).map_err(|e| {
        Error::other(format!("Failed to create sessions file: {}", e))
    })?;

    file.write_all(content.as_bytes()).map_err(|e| {
        Error::other(format!("Failed to write sessions file: {}", e))
    })?;

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        fs::set_permissions(path, fs::Permissions::from_mode(0o600)).map_err(|e| {
            Error::other(format!("Failed to set sessions file permissions: {}", e))
        })?;
    }

    Ok(())
}

fn add_session(token_hash: &str, session: Session) -> Result<(), Error> {
    let mut sessions = load_sessions()?;
    sessions.insert(token_hash.to_string(), session);
    save_sessions(&sessions)
}

fn remove_session(token_hash: &str) -> Result<(), Error> {
    let mut sessions = load_sessions()?;
    sessions.remove(token_hash);
    save_sessions(&sessions)
}

/// Validate a session token hash, check expiration, and update last_active timestamp
pub fn validate_session(token_hash: &str) -> Result<(), Error> {
    let mut sessions = load_sessions()?;

    let session = sessions
        .get(token_hash)
        .ok_or_else(|| Error::other("Invalid session token"))?;

    // Check if session has expired
    if let Some(expiry) = TimeDelta::try_days(SESSION_EXPIRY_DAYS) {
        if Utc::now() - session.last_active > expiry {
            // Remove expired session
            sessions.remove(token_hash);
            save_sessions(&sessions)?;
            return Err(Error::other("Session expired"));
        }
    }

    // Update last_active timestamp
    let updated_session = Session {
        last_active: Utc::now(),
        ..session.clone()
    };
    sessions.insert(token_hash.to_string(), updated_session);
    save_sessions(&sessions)?;

    Ok(())
}

#[instrument(skip_all)]
pub async fn login_impl(
    _ctx: ServerContext,
    LoginParams { password, user_agent }: LoginParams,
) -> Result<LoginRes, Error> {
    check_password(&password)?;

    let hash_token = HashSessionToken::new();
    let session = Session {
        logged_in: Utc::now(),
        last_active: Utc::now(),
        user_agent,
    };

    add_session(hash_token.hashed(), session)?;

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
        remove_session(&hash)?;
    }
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
        json!({
            "password": password,
        }),
        Empty {},
    )
    .await?;

    println!("Login successful. Session saved.");
    Ok(())
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
}
