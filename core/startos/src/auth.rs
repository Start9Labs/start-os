use std::collections::BTreeMap;

use chrono::{DateTime, Utc};
use clap::Parser;
use color_eyre::eyre::eyre;
use imbl_value::{json, InternedString};
use itertools::Itertools;
use josekit::jwk::Jwk;
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{
    from_fn_async, CallRemote, Context, Empty, HandlerArgs, HandlerExt, ParentHandler,
};
use serde::{Deserialize, Serialize};
use tracing::instrument;
use ts_rs::TS;

use crate::context::{CliContext, RpcContext};
use crate::db::model::DatabaseModel;
use crate::middleware::auth::{
    AsLogoutSessionId, HasLoggedOutSessions, HashSessionToken, LoginRes,
};
use crate::prelude::*;
use crate::util::crypto::EncryptedWire;
use crate::util::serde::{display_serializable, HandlerExtSerde, WithIoFormat};
use crate::{ensure_code, Error, ResultExt};

#[derive(Debug, Clone, Default, Deserialize, Serialize, TS)]
#[ts(as = "BTreeMap::<String, Session>")]
pub struct Sessions(pub BTreeMap<InternedString, Session>);
impl Sessions {
    pub fn new() -> Self {
        Self(BTreeMap::new())
    }
}
impl Map for Sessions {
    type Key = InternedString;
    type Value = Session;
    fn key_str(key: &Self::Key) -> Result<impl AsRef<str>, Error> {
        Ok(key)
    }
    fn key_string(key: &Self::Key) -> Result<InternedString, Error> {
        Ok(key.clone())
    }
}

#[derive(Clone, Serialize, Deserialize, TS)]
#[serde(untagged)]
#[ts(export)]
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
pub fn auth<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "login",
            from_fn_async(login_impl)
                .with_metadata("login", Value::Bool(true))
                .no_cli(),
        )
        .subcommand("login", from_fn_async(cli_login).no_display())
        .subcommand::<C, _>(
            "logout",
            from_fn_async(logout)
                .with_metadata("get_session", Value::Bool(true))
                .no_display()
                .with_call_remote::<CliContext>(),
        )
        .subcommand("session", session::<C>())
        .subcommand(
            "reset-password",
            from_fn_async(reset_password_impl).no_cli(),
        )
        .subcommand(
            "reset-password",
            from_fn_async(cli_reset_password).no_display(),
        )
        .subcommand::<C, _>(
            "get-pubkey",
            from_fn_async(get_pubkey)
                .with_metadata("authenticated", Value::Bool(false))
                .no_display()
                .with_call_remote::<CliContext>(),
        )
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
    HandlerArgs {
        context: ctx,
        parent_method,
        method,
        ..
    }: HandlerArgs<CliContext>,
) -> Result<(), RpcError> {
    let password = rpassword::prompt_password("Password: ")?;

    ctx.call_remote::<RpcContext>(
        &parent_method.into_iter().chain(method).join("."),
        json!({
            "password": password,
            "metadata": {
                "platforms": ["cli"],
            },
        }),
    )
    .await?;

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

pub fn check_password_against_db(db: &DatabaseModel, password: &str) -> Result<(), Error> {
    let pw_hash = db.as_private().as_password().de()?;
    check_password(&pw_hash, password)?;
    Ok(())
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct LoginParams {
    password: Option<PasswordType>,
    #[ts(skip)]
    #[serde(rename = "__auth_userAgent")] // from Auth middleware
    user_agent: Option<String>,
    #[serde(default)]
    #[ts(type = "any")]
    metadata: Value,
}

#[instrument(skip_all)]
pub async fn login_impl(
    ctx: RpcContext,
    LoginParams {
        password,
        user_agent,
        metadata,
    }: LoginParams,
) -> Result<LoginRes, Error> {
    let password = password.unwrap_or_default().decrypt(&ctx)?;

    ctx.db
        .mutate(|db| {
            check_password_against_db(db, &password)?;
            let hash_token = HashSessionToken::new();
            db.as_private_mut().as_sessions_mut().insert(
                hash_token.hashed(),
                &Session {
                    logged_in: Utc::now(),
                    last_active: Utc::now(),
                    user_agent,
                    metadata,
                },
            )?;

            Ok(hash_token.to_login_res())
        })
        .await
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct LogoutParams {
    #[ts(skip)]
    #[serde(rename = "__auth_session")] // from Auth middleware
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

#[derive(Debug, Clone, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct Session {
    #[ts(type = "string")]
    pub logged_in: DateTime<Utc>,
    #[ts(type = "string")]
    pub last_active: DateTime<Utc>,
    #[ts(skip)]
    pub user_agent: Option<String>,
    #[ts(type = "any")]
    pub metadata: Value,
}

#[derive(Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SessionList {
    #[ts(type = "string")]
    current: InternedString,
    sessions: Sessions,
}

pub fn session<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand::<C, _>(
            "list",
            from_fn_async(list)
                .with_metadata("get_session", Value::Bool(true))
                .with_display_serializable()
                .with_custom_display_fn(|handle, result| {
                    Ok(display_sessions(handle.params, result))
                })
                .with_call_remote::<CliContext>(),
        )
        .subcommand::<C, _>(
            "kill",
            from_fn_async(kill)
                .no_display()
                .with_call_remote::<CliContext>(),
        )
}

fn display_sessions(params: WithIoFormat<ListParams>, arg: SessionList) {
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
    for (id, session) in arg.sessions.0 {
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

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct ListParams {
    #[arg(skip)]
    #[ts(skip)]
    session: InternedString,
}

// #[command(display(display_sessions))]
#[instrument(skip_all)]
pub async fn list(
    ctx: RpcContext,
    ListParams { session, .. }: ListParams,
) -> Result<SessionList, Error> {
    Ok(SessionList {
        current: HashSessionToken::from_token(session).hashed().clone(),
        sessions: ctx.db.peek().await.into_private().into_sessions().de()?,
    })
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct KillSessionId(InternedString);

impl KillSessionId {
    fn new(id: String) -> Self {
        Self(InternedString::from(id))
    }
}

impl AsLogoutSessionId for KillSessionId {
    fn as_logout_session_id(self) -> InternedString {
        self.0
    }
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct KillParams {
    ids: Vec<String>,
}

#[instrument(skip_all)]
pub async fn kill(ctx: RpcContext, KillParams { ids }: KillParams) -> Result<(), Error> {
    HasLoggedOutSessions::new(ids.into_iter().map(KillSessionId::new), &ctx).await?;
    Ok(())
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct ResetPasswordParams {
    old_password: Option<PasswordType>,
    new_password: Option<PasswordType>,
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

    let new_password = {
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

    ctx.call_remote::<RpcContext>(
        &parent_method.into_iter().chain(method).join("."),
        imbl_value::json!({ "old-password": old_password, "new-password": new_password }),
    )
    .await?;

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
    let account_password = &account.password;
    let account = account.clone();
    ctx.db
        .mutate(|d| {
            d.as_public_mut()
                .as_server_info_mut()
                .as_password_hash_mut()
                .ser(account_password)?;
            account.save(d)?;

            Ok(())
        })
        .await
}

#[instrument(skip_all)]
pub async fn get_pubkey(ctx: RpcContext) -> Result<Jwk, RpcError> {
    let secret = <RpcContext as AsRef<Jwk>>::as_ref(&ctx).clone();
    let pub_key = secret.to_public_key()?;
    Ok(pub_key)
}
