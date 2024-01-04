use std::collections::BTreeMap;
use std::marker::PhantomData;

use chrono::{DateTime, Utc};
use clap::{ArgMatches, Parser};
use color_eyre::eyre::eyre;
use imbl_value::{json, InternedString};
use josekit::jwk::Jwk;
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{command, from_fn_async, CallRemote, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use sqlx::{Executor, Postgres};
use tracing::instrument;

use crate::context::{CliContext, RpcContext};
use crate::middleware::auth::{
    AsLogoutSessionId, HasLoggedOutSessions, HashSessionToken, LoginRes,
};
use crate::prelude::*;
use crate::util::crypto::EncryptedWire;
use crate::util::serde::{display_serializable, IoFormat};
use crate::{ensure_code, Error, ResultExt};
#[derive(Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum PasswordType {
    EncryptedWire(EncryptedWire),
    String(String),
}
impl PasswordType {
    pub fn decrypt(self, current_secret: impl AsRef<Jwk>) -> Result<String, Error> {
        match self {
            PasswordType::String(x) => Ok(x),
            PasswordType::EncryptedWire(x) => x.decrypt(current_secret).ok_or_else(|| {
                Error::new(
                    color_eyre::eyre::eyre!("Couldn't decode password"),
                    crate::ErrorKind::Unknown,
                )
            }),
        }
    }
}
impl Default for PasswordType {
    fn default() -> Self {
        PasswordType::String(String::default())
    }
}
impl std::fmt::Debug for PasswordType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<REDACTED_PASSWORD>")?;
        Ok(())
    }
}

impl std::str::FromStr for PasswordType {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match serde_json::from_str(s) {
            Ok(a) => a,
            Err(_) => PasswordType::String(s.to_string()),
        })
    }
}
pub fn auth() -> ParentHandler {
    ParentHandler::new()
        .subcommand(
            "login",
            from_fn_async(login_impl)
                .metadata("login", Value::Boolean(true))
                .no_cli(),
        )
        .subcommand("login", from_fn_async(cli_login).no_display())
        .subcommand(
            "logout",
            from_fn_async(logout)
                .metadata("get-session", Value::Boolean(true))
                .with_remote_cli::<CliContext>(),
        )
        .subcommand("session", session())
        .subcommand(
            "reset-password",
            from_fn_async(reset_password_impl).no_cli(),
        )
        .subcommand(
            "reset-password",
            from_fn_async(cli_reset_password).no_display(),
        )
        .subcommand(
            "get-pubkey",
            from_fn_async(get_pubkey)
                .no_display()
                .metadata("authenticated", Value::Boolean(false))
                .with_remote_cli::<CliContext>(),
        )
}

pub fn cli_metadata() -> Value {
    serde_json::json!({
        "platforms": ["cli"],
    })
}

pub fn parse_metadata(_: &str, _: &ArgMatches) -> Result<Value, Error> {
    Ok(cli_metadata())
}

#[test]
fn gen_pwd() {
    println!(
        "{:?}",
        argon2::hash_encoded(
            b"testing1234",
            &rand::random::<[u8; 16]>()[..],
            &argon2::Config::rfc9106_low_mem()
        )
        .unwrap()
    )
}

#[instrument(skip_all)]
async fn cli_login(
    ctx: CliContext,
    password: Option<PasswordType>,
    metadata: Value,
) -> Result<(), RpcError> {
    let password = if let Some(password) = password {
        password.decrypt(&ctx)?
    } else {
        rpassword::prompt_password("Password: ")?
    };

    ctx.call_remote(
        "auth.login",
        json!({ "password": password, "metadata": metadata }),
    )
    .await?
    .result?;

    Ok(())
}

pub fn check_password(hash: &str, password: &str) -> Result<(), Error> {
    ensure_code!(
        argon2::verify_encoded(&hash, password.as_bytes()).map_err(|_| {
            Error::new(
                eyre!("Password Incorrect"),
                crate::ErrorKind::IncorrectPassword,
            )
        })?,
        crate::ErrorKind::IncorrectPassword,
        "Password Incorrect"
    );
    Ok(())
}

pub async fn check_password_against_db<Ex>(secrets: &mut Ex, password: &str) -> Result<(), Error>
where
    for<'a> &'a mut Ex: Executor<'a, Database = Postgres>,
{
    let pw_hash = sqlx::query!("SELECT password FROM account")
        .fetch_one(secrets)
        .await?
        .password;
    check_password(&pw_hash, password)?;
    Ok(())
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct LoginParams {
    password: Option<PasswordType>,
    #[arg(
        parse(parse_metadata),
        help = "RPC Only: This value cannot be overidden from the cli"
    )]
    #[serde(default = "cli_metadata")]
    metadata: Value,
}

#[instrument(skip_all)]
pub async fn login_impl(
    ctx: RpcContext,
    LoginParams { password, metadata }: LoginParams,
) -> Result<LoginRes, Error> {
    let password = password.unwrap_or_default().decrypt(&ctx)?;
    let mut handle = ctx.secret_store.acquire().await?;
    check_password_against_db(handle.as_mut(), &password).await?;

    let hash_token = HashSessionToken::new();
    let user_agent = metadata.get("user-agent").and_then(|h| h.to_str().ok());
    let metadata = serde_json::to_string(&metadata).with_kind(crate::ErrorKind::Database)?;
    let hash_token_hashed = hash_token.hashed();
    sqlx::query!(
        "INSERT INTO session (id, user_agent, metadata) VALUES ($1, $2, $3)",
        hash_token_hashed,
        user_agent,
        metadata,
    )
    .execute(handle.as_mut())
    .await?;

    Ok(hash_token.to_login_res())
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct LogoutParams {
    session: InternedString,
}

pub async fn logout(
    ctx: RpcContext,
    LogoutParams { session }: LogoutParams,
) -> Result<Option<HasLoggedOutSessions>, Error> {
    Ok(Some(
        HasLoggedOutSessions::new(vec![HashSessionToken::from_token(session)], &ctx).await?,
    ))
}

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct Session {
    logged_in: DateTime<Utc>,
    last_active: DateTime<Utc>,
    user_agent: Option<String>,
    metadata: Value,
}

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct SessionList {
    current: String,
    sessions: BTreeMap<String, Session>,
}

pub async fn session() -> ParentHandler {
    ParentHandler::new()
        .subcommand(
            "list",
            from_fn_async(list)
                .metadata("get-session", Value::Boolean(true))
                .with_custom_display_fn(|handle, result| {
                    Ok(display_sessions(handle.params, result))
                })
                .with_remote_cli::<CliContext>(),
        )
        .subcommand(
            "kill",
            from_fn_async(kill)
                .no_display()
                .with_remote_cli::<CliContext>(),
        )
}

fn display_sessions(params: ListParams, arg: SessionList) {
    use prettytable::*;

    if let Some(format) = params.format {
        return display_serializable(format, arg);
    }

    let mut table = Table::new();
    table.add_row(row![bc =>
        "ID",
        "LOGGED IN",
        "LAST ACTIVE",
        "USER AGENT",
        "METADATA",
    ]);
    for (id, session) in arg.sessions {
        let mut row = row![
            &id,
            &format!("{}", session.logged_in),
            &format!("{}", session.last_active),
            session.user_agent.as_deref().unwrap_or("N/A"),
            &format!("{}", session.metadata),
        ];
        if id == arg.current {
            row.iter_mut()
                .map(|c| c.style(Attr::ForegroundColor(color::GREEN)))
                .collect::<()>()
        }
        table.add_row(row);
    }
    table.print_tty(false).unwrap();
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct ListParams {
    #[arg(skip)]
    session: InternedString,
    #[arg(long = "format")]
    format: Option<IoFormat>,
}

// #[command(display(display_sessions))]
#[instrument(skip_all)]
pub async fn list(
    ctx: RpcContext,
    ListParams { session, .. }: ListParams,
) -> Result<SessionList, Error> {
    Ok(SessionList {
        current: HashSessionToken::from_token(session)?.as_hash(),
        sessions: sqlx::query!(
            "SELECT * FROM session WHERE logged_out IS NULL OR logged_out > CURRENT_TIMESTAMP"
        )
        .fetch_all(ctx.secret_store.acquire().await?.as_mut())
        .await?
        .into_iter()
        .map(|row| {
            Ok((
                row.id,
                Session {
                    logged_in: DateTime::from_utc(row.logged_in, Utc),
                    last_active: DateTime::from_utc(row.last_active, Utc),
                    user_agent: row.user_agent,
                    metadata: serde_json::from_str(&row.metadata)
                        .with_kind(crate::ErrorKind::Database)?,
                },
            ))
        })
        .collect::<Result<_, Error>>()?,
    })
}

fn parse_comma_separated(arg: &str, _: &ArgMatches) -> Result<Vec<String>, RpcError> {
    Ok(arg.split(",").map(|s| s.trim().to_owned()).collect())
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct KillSessionId(String);

impl AsLogoutSessionId for KillSessionId {
    fn as_logout_session_id(self) -> String {
        self.0
    }
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct KillParams {
    ids: Vec<String>,
}

#[instrument(skip_all)]
pub async fn kill(ctx: RpcContext, KillParams { ids }: KillParams) -> Result<(), Error> {
    HasLoggedOutSessions::new(ids.into_iter().map(KillSessionId), &ctx).await?;
    Ok(())
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct ResetPasswordParams {
    #[arg(rename = "old-password")]
    old_password: Option<PasswordType>,
    #[arg(rename = "new-password")]
    new_password: Option<PasswordType>,
}

#[instrument(skip_all)]
async fn cli_reset_password(
    ctx: CliContext,
    ResetPasswordParams {
        old_password,
        new_password,
    }: ResetPasswordParams,
) -> Result<(), RpcError> {
    let old_password = if let Some(old_password) = old_password {
        old_password.decrypt(&ctx)?
    } else {
        rpassword::prompt_password("Current Password: ")?
    };

    let new_password = if let Some(new_password) = new_password {
        new_password.decrypt(&ctx)?
    } else {
        let new_password = rpassword::prompt_password("New Password: ")?;
        if new_password != rpassword::prompt_password("Confirm: ")? {
            return Err(Error::new(
                eyre!("Passwords do not match"),
                crate::ErrorKind::IncorrectPassword,
            )
            .into());
        }
        new_password
    };

    ctx.call_remote(
        ctx,
        "auth.reset-password",
        serde_json::json!({ "old-password": old_password, "new-password": new_password }),
    )
    .await?
    .result?;

    Ok(())
}

#[instrument(skip_all)]
pub async fn reset_password_impl(
    ctx: RpcContext,
    ResetPasswordParams {
        old_password,
        new_password,
    }: ResetPasswordParams,
) -> Result<(), Error> {
    let old_password = old_password.unwrap_or_default().decrypt(&ctx)?;
    let new_password = new_password.unwrap_or_default().decrypt(&ctx)?;

    let mut account = ctx.account.write().await;
    if !argon2::verify_encoded(&account.password, old_password.as_bytes())
        .with_kind(crate::ErrorKind::IncorrectPassword)?
    {
        return Err(Error::new(
            eyre!("Incorrect Password"),
            crate::ErrorKind::IncorrectPassword,
        ));
    }
    account.set_password(&new_password)?;
    account.save(&ctx.secret_store).await?;
    let account_password = &account.password;
    ctx.db
        .mutate(|d| {
            d.as_server_info_mut()
                .as_password_hash_mut()
                .ser(account_password)
        })
        .await
}

#[instrument(skip_all)]
pub async fn get_pubkey(ctx: RpcContext) -> Result<Jwk, RpcError> {
    let secret = ctx.as_ref().clone();
    let pub_key = secret.to_public_key()?;
    Ok(pub_key)
}
