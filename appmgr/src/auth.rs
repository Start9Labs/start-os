use anyhow::anyhow;
use basic_cookies::Cookie;
use chrono::{DateTime, NaiveDateTime, Utc};
use clap::ArgMatches;
use http::header::COOKIE;
use http::HeaderValue;
use indexmap::IndexMap;
use rpc_toolkit::command;
use rpc_toolkit::command_helpers::prelude::{RequestParts, ResponseParts};
use rpc_toolkit::yajrc::RpcError;
use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::context::EitherContext;
use crate::middleware::auth::{get_id, hash_token};
use crate::util::{display_none, display_serializable, IoFormat};
use crate::{ensure_code, Error, ResultExt};

#[command(subcommands(login, logout, session))]
pub fn auth(#[context] ctx: EitherContext) -> Result<EitherContext, Error> {
    Ok(ctx)
}

pub fn parse_metadata(_: &str, _: &ArgMatches<'_>) -> Result<Value, Error> {
    Ok(serde_json::json!({
        "platforms": ["cli"],
    }))
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

#[command(display(display_none), metadata(authenticated = false))]
pub async fn login(
    #[context] ctx: EitherContext,
    #[request] req: &RequestParts,
    #[response] res: &mut ResponseParts,
    #[arg] password: String,
    #[arg(
        parse(parse_metadata),
        default = "",
        help = "RPC Only: This value cannot be overidden from the cli"
    )]
    metadata: Value,
) -> Result<(), Error> {
    let rpc_ctx = ctx.as_rpc().unwrap();
    let mut handle = rpc_ctx.secret_store.acquire().await?;
    let pw_hash = sqlx::query!("SELECT hash FROM password")
        .fetch_one(&mut handle)
        .await?
        .hash;
    ensure_code!(
        argon2::verify_encoded(&pw_hash, password.as_bytes()).map_err(|_| {
            Error::new(
                anyhow!("Password Incorrect"),
                crate::ErrorKind::Authorization,
            )
        })?,
        crate::ErrorKind::Authorization,
        "Password Incorrect"
    );
    let token = base32::encode(
        base32::Alphabet::RFC4648 { padding: false },
        &rand::random::<[u8; 16]>(),
    )
    .to_lowercase();
    let id = hash_token(&token);
    let user_agent = req.headers.get("user-agent").and_then(|h| h.to_str().ok());
    let metadata = serde_json::to_string(&metadata).with_kind(crate::ErrorKind::Database)?;
    sqlx::query!(
        "INSERT INTO session (id, user_agent, metadata) VALUES (?, ?, ?)",
        id,
        user_agent,
        metadata,
    )
    .execute(&mut handle)
    .await?;
    res.headers.insert(
        "set-cookie",
        HeaderValue::from_str(&format!(
            "session={}; HttpOnly; SameSite=Strict; Expires=Fri, 31 Dec 9999 23:59:59 GMT;",
            token
        ))
        .with_kind(crate::ErrorKind::Unknown)?, // Should be impossible, but don't want to panic
    );

    Ok(())
}

#[command(display(display_none), metadata(authenticated = false))]
pub async fn logout(
    #[context] ctx: EitherContext,
    #[request] req: &RequestParts,
) -> Result<(), Error> {
    if let Some(cookie_header) = req.headers.get(COOKIE) {
        let cookies = Cookie::parse(
            cookie_header
                .to_str()
                .with_kind(crate::ErrorKind::Authorization)?,
        )
        .with_kind(crate::ErrorKind::Authorization)?;
        if let Some(session) = cookies.iter().find(|c| c.get_name() == "session") {
            let token = session.get_value();
            let id = hash_token(token);
            kill(ctx, vec![id]).await?;
        }
    }
    Ok(())
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
    sessions: IndexMap<String, Session>,
}

#[command(subcommands(list, kill))]
pub async fn session(#[context] ctx: EitherContext) -> Result<EitherContext, Error> {
    Ok(ctx)
}

fn display_sessions(arg: SessionList, matches: &ArgMatches<'_>) {
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
    table.print_tty(false);
}

#[command(display(display_sessions))]
pub async fn list(
    #[context] ctx: EitherContext,
    #[request] req: &RequestParts,
    #[allow(unused_variables)]
    #[arg(long = "format")]
    format: Option<IoFormat>,
) -> Result<SessionList, Error> {
    Ok(SessionList {
        current: get_id(req)?,
        sessions: sqlx::query!(
            "SELECT * FROM session WHERE logged_out IS NULL OR logged_out > CURRENT_TIMESTAMP"
        )
        .fetch_all(&mut ctx.as_rpc().unwrap().secret_store.acquire().await?)
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

fn parse_comma_separated(arg: &str, _: &ArgMatches<'_>) -> Result<Vec<String>, RpcError> {
    Ok(arg.split(",").map(|s| s.to_owned()).collect())
}

#[command(display(display_none))]
pub async fn kill(
    #[context] ctx: EitherContext,
    #[arg(parse(parse_comma_separated))] ids: Vec<String>,
) -> Result<(), Error> {
    let rpc_ctx = ctx.as_rpc().unwrap();
    sqlx::query(&format!(
        "UPDATE session SET logged_out = CURRENT_TIMESTAMP WHERE id IN ('{}')",
        ids.join("','")
    ))
    .execute(&mut rpc_ctx.secret_store.acquire().await?)
    .await?;
    Ok(())
}
