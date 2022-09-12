use std::collections::BTreeMap;
use std::marker::PhantomData;

use chrono::{DateTime, Utc};
use clap::ArgMatches;
use color_eyre::eyre::eyre;
use patch_db::{DbHandle, LockReceipt};
use rpc_toolkit::command;
use rpc_toolkit::command_helpers::prelude::{RequestParts, ResponseParts};
use rpc_toolkit::yajrc::RpcError;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use sqlx::{Executor, Postgres};
use tracing::instrument;

use crate::context::{CliContext, RpcContext};
use crate::middleware::auth::{AsLogoutSessionId, HasLoggedOutSessions, HashSessionToken};
use crate::util::display_none;
use crate::util::serde::{display_serializable, IoFormat};
use crate::{ensure_code, Error, ResultExt};

#[command(subcommands(login, logout, session, reset_password))]
pub fn auth() -> Result<(), Error> {
    Ok(())
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
            &argon2::Config::default()
        )
        .unwrap()
    )
}

#[instrument(skip(ctx, password))]
async fn cli_login(
    ctx: CliContext,
    password: Option<String>,
    metadata: Value,
) -> Result<(), RpcError> {
    let password = if let Some(password) = password {
        password
    } else {
        rpassword::prompt_password("Password: ")?
    };

    rpc_toolkit::command_helpers::call_remote(
        ctx,
        "auth.login",
        serde_json::json!({ "password": password, "metadata": metadata }),
        PhantomData::<()>,
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

#[command(
    custom_cli(cli_login(async, context(CliContext))),
    display(display_none),
    metadata(authenticated = false)
)]
#[instrument(skip(ctx, password))]
pub async fn login(
    #[context] ctx: RpcContext,
    #[request] req: &RequestParts,
    #[response] res: &mut ResponseParts,
    #[arg] password: Option<String>,
    #[arg(
        parse(parse_metadata),
        default = "cli_metadata",
        help = "RPC Only: This value cannot be overidden from the cli"
    )]
    metadata: Value,
) -> Result<(), Error> {
    let password = password.unwrap_or_default();
    let mut handle = ctx.secret_store.acquire().await?;
    check_password_against_db(&mut handle, &password).await?;

    let hash_token = HashSessionToken::new();
    let user_agent = req.headers.get("user-agent").and_then(|h| h.to_str().ok());
    let metadata = serde_json::to_string(&metadata).with_kind(crate::ErrorKind::Database)?;
    let hash_token_hashed = hash_token.hashed();
    sqlx::query!(
        "INSERT INTO session (id, user_agent, metadata) VALUES ($1, $2, $3)",
        hash_token_hashed,
        user_agent,
        metadata,
    )
    .execute(&mut handle)
    .await?;
    res.headers.insert(
        "set-cookie",
        hash_token.header_value()?, // Should be impossible, but don't want to panic
    );

    Ok(())
}

#[command(display(display_none), metadata(authenticated = false))]
#[instrument(skip(ctx))]
pub async fn logout(
    #[context] ctx: RpcContext,
    #[request] req: &RequestParts,
) -> Result<Option<HasLoggedOutSessions>, Error> {
    let auth = match HashSessionToken::from_request_parts(req) {
        Err(_) => return Ok(None),
        Ok(a) => a,
    };
    Ok(Some(HasLoggedOutSessions::new(vec![auth], &ctx).await?))
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

#[command(subcommands(list, kill))]
pub async fn session() -> Result<(), Error> {
    Ok(())
}

fn display_sessions(arg: SessionList, matches: &ArgMatches) {
    use prettytable::*;

    if matches.is_present("format") {
        return display_serializable(arg, matches);
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

#[command(display(display_sessions))]
#[instrument(skip(ctx))]
pub async fn list(
    #[context] ctx: RpcContext,
    #[request] req: &RequestParts,
    #[allow(unused_variables)]
    #[arg(long = "format")]
    format: Option<IoFormat>,
) -> Result<SessionList, Error> {
    Ok(SessionList {
        current: HashSessionToken::from_request_parts(req)?.as_hash(),
        sessions: sqlx::query!(
            "SELECT * FROM session WHERE logged_out IS NULL OR logged_out > CURRENT_TIMESTAMP"
        )
        .fetch_all(&mut ctx.secret_store.acquire().await?)
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

#[command(display(display_none))]
#[instrument(skip(ctx))]
pub async fn kill(
    #[context] ctx: RpcContext,
    #[arg(parse(parse_comma_separated))] ids: Vec<String>,
) -> Result<(), Error> {
    HasLoggedOutSessions::new(ids.into_iter().map(KillSessionId), &ctx).await?;
    Ok(())
}

#[instrument(skip(ctx, old_password, new_password))]
async fn cli_reset_password(
    ctx: CliContext,
    old_password: Option<String>,
    new_password: Option<String>,
) -> Result<(), RpcError> {
    let old_password = if let Some(old_password) = old_password {
        old_password
    } else {
        rpassword::prompt_password("Current Password: ")?
    };

    let new_password = if let Some(new_password) = new_password {
        new_password
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

    rpc_toolkit::command_helpers::call_remote(
        ctx,
        "auth.reset-password",
        serde_json::json!({ "old-password": old_password, "new-password": new_password }),
        PhantomData::<()>,
    )
    .await?
    .result?;

    Ok(())
}

pub struct SetPasswordReceipt(LockReceipt<String, ()>);
impl SetPasswordReceipt {
    pub async fn new<Db: DbHandle>(db: &mut Db) -> Result<Self, Error> {
        let mut locks = Vec::new();

        let setup = Self::setup(&mut locks);
        Ok(setup(&db.lock_all(locks).await?)?)
    }

    pub fn setup(
        locks: &mut Vec<patch_db::LockTargetId>,
    ) -> impl FnOnce(&patch_db::Verifier) -> Result<Self, Error> {
        let password_hash = crate::db::DatabaseModel::new()
            .server_info()
            .password_hash()
            .make_locker(patch_db::LockType::Write)
            .add_to_keys(locks);
        move |skeleton_key| Ok(Self(password_hash.verify(skeleton_key)?))
    }
}

pub async fn set_password<Db: DbHandle, Ex>(
    db: &mut Db,
    receipt: &SetPasswordReceipt,
    secrets: &mut Ex,
    password: &str,
) -> Result<(), Error>
where
    for<'a> &'a mut Ex: Executor<'a, Database = Postgres>,
{
    let password = argon2::hash_encoded(
        password.as_bytes(),
        &rand::random::<[u8; 16]>()[..],
        &argon2::Config::default(),
    )
    .with_kind(crate::ErrorKind::PasswordHashGeneration)?;

    sqlx::query!("UPDATE account SET password = $1", password,)
        .execute(secrets)
        .await?;

    receipt.0.set(db, password).await?;

    Ok(())
}

#[command(
    rename = "reset-password",
    custom_cli(cli_reset_password(async, context(CliContext))),
    display(display_none)
)]
#[instrument(skip(ctx, old_password, new_password))]
pub async fn reset_password(
    #[context] ctx: RpcContext,
    #[arg(rename = "old-password")] old_password: Option<String>,
    #[arg(rename = "new-password")] new_password: Option<String>,
) -> Result<(), Error> {
    let old_password = old_password.unwrap_or_default();
    let new_password = new_password.unwrap_or_default();

    let mut secrets = ctx.secret_store.acquire().await?;
    check_password_against_db(&mut secrets, &old_password).await?;

    let mut db = ctx.db.handle();

    let set_password_receipt = SetPasswordReceipt::new(&mut db).await?;

    set_password(&mut db, &set_password_receipt, &mut secrets, &new_password).await?;

    Ok(())
}
