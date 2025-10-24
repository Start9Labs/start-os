use clap::Parser;
use imbl::HashMap;
use imbl_value::InternedString;
use itertools::Itertools;
use patch_db::HasModel;
use rpc_toolkit::{from_fn_async, Context, HandlerArgs, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::auth::{check_password, Sessions};
use crate::context::CliContext;
use crate::middleware::auth::AuthContext;
use crate::middleware::signature::SignatureAuthContext;
use crate::prelude::*;
use crate::rpc_continuations::OpenAuthedContinuations;
use crate::sign::AnyVerifyingKey;
use crate::tunnel::context::TunnelContext;
use crate::tunnel::db::TunnelDatabase;
use crate::util::serde::{display_serializable, HandlerExtSerde};
use crate::util::sync::SyncMutex;

impl SignatureAuthContext for TunnelContext {
    type Database = TunnelDatabase;
    type AdditionalMetadata = ();
    type CheckPubkeyRes = ();
    fn db(&self) -> &TypedPatchDb<Self::Database> {
        &self.db
    }
    async fn sig_context(
        &self,
    ) -> impl IntoIterator<Item = Result<impl AsRef<str> + Send, Error>> + Send {
        let peek = self.db().peek().await;
        peek.as_webserver()
            .de()
            .map(|a| a.as_ref().map(InternedString::from_display))
            .transpose()
            .into_iter()
            .chain(
                std::iter::from_fn(move || Some(peek.as_certificates().keys()))
                    .flatten_ok()
                    .map_ok(|h| h.0)
                    .flatten_ok(),
            )
    }
    fn check_pubkey(
        db: &Model<Self::Database>,
        pubkey: Option<&crate::sign::AnyVerifyingKey>,
        _: Self::AdditionalMetadata,
    ) -> Result<Self::CheckPubkeyRes, Error> {
        if let Some(pubkey) = pubkey {
            if db.as_auth_pubkeys().de()?.contains_key(pubkey) {
                return Ok(());
            }
        }

        Err(Error::new(
            eyre!("Key is not authorized"),
            ErrorKind::IncorrectPassword,
        ))
    }
    async fn post_auth_hook(
        &self,
        _: Self::CheckPubkeyRes,
        _: &rpc_toolkit::RpcRequest,
    ) -> Result<(), Error> {
        Ok(())
    }
}
impl AuthContext for TunnelContext {
    const LOCAL_AUTH_COOKIE_PATH: &str = "/run/start-tunnel/rpc.authcookie";
    const LOCAL_AUTH_COOKIE_OWNERSHIP: &str = "root:root";
    fn access_sessions(db: &mut Model<Self::Database>) -> &mut Model<crate::auth::Sessions> {
        db.as_sessions_mut()
    }
    fn ephemeral_sessions(&self) -> &SyncMutex<Sessions> {
        &self.ephemeral_sessions
    }
    fn open_authed_continuations(&self) -> &OpenAuthedContinuations<Option<InternedString>> {
        &self.open_authed_continuations
    }
    fn check_password(db: &Model<Self::Database>, password: &str) -> Result<(), Error> {
        check_password(&db.as_password().de()?.unwrap_or_default(), password)
    }
}

#[derive(Clone, Debug, Deserialize, Serialize, HasModel, TS, Parser)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct SignerInfo {
    pub name: InternedString,
}

pub fn auth_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("set-password", from_fn_async(set_password_rpc).no_cli())
        .subcommand(
            "set-password",
            from_fn_async(set_password_cli)
                .with_about("Set user interface password")
                .no_display(),
        )
        .subcommand(
            "key",
            ParentHandler::<C>::new()
                .subcommand(
                    "add",
                    from_fn_async(add_key)
                        .with_metadata("sync_db", Value::Bool(true))
                        .no_display()
                        .with_about("Add a new authorized key")
                        .with_call_remote::<CliContext>(),
                )
                .subcommand(
                    "remove",
                    from_fn_async(remove_key)
                        .with_metadata("sync_db", Value::Bool(true))
                        .no_display()
                        .with_about("Remove an authorized key")
                        .with_call_remote::<CliContext>(),
                )
                .subcommand(
                    "list",
                    from_fn_async(list_keys)
                        .with_metadata("sync_db", Value::Bool(true))
                        .with_display_serializable()
                        .with_custom_display_fn(|HandlerArgs { params, .. }, res| {
                            use prettytable::*;

                            if let Some(format) = params.format {
                                return display_serializable(format, res);
                            }

                            let mut table = Table::new();
                            table.add_row(row![bc => "NAME", "KEY"]);
                            for (key, info) in res {
                                table.add_row(row![info.name, key]);
                            }

                            table.print_tty(false)?;

                            Ok(())
                        })
                        .with_about("List authorized keys")
                        .with_call_remote::<CliContext>(),
                ),
        )
}

#[derive(Debug, Deserialize, Serialize, Parser)]
#[serde(rename_all = "camelCase")]
pub struct AddKeyParams {
    pub name: InternedString,
    pub key: AnyVerifyingKey,
}

pub async fn add_key(
    ctx: TunnelContext,
    AddKeyParams { name, key }: AddKeyParams,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            db.as_auth_pubkeys_mut().mutate(|auth_pubkeys| {
                auth_pubkeys.insert(key, SignerInfo { name });
                Ok(())
            })
        })
        .await
        .result
}

#[derive(Debug, Deserialize, Serialize, Parser)]
#[serde(rename_all = "camelCase")]
pub struct RemoveKeyParams {
    pub key: AnyVerifyingKey,
}

pub async fn remove_key(
    ctx: TunnelContext,
    RemoveKeyParams { key }: RemoveKeyParams,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            db.as_auth_pubkeys_mut()
                .mutate(|auth_pubkeys| Ok(auth_pubkeys.remove(&key)))
        })
        .await
        .result?;
    Ok(())
}

pub async fn list_keys(ctx: TunnelContext) -> Result<HashMap<AnyVerifyingKey, SignerInfo>, Error> {
    ctx.db.peek().await.into_auth_pubkeys().de()
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct SetPasswordParams {
    pub password: String,
}

pub async fn set_password_rpc(
    ctx: TunnelContext,
    SetPasswordParams { password }: SetPasswordParams,
) -> Result<(), Error> {
    let pwhash = argon2::hash_encoded(
        password.as_bytes(),
        &rand::random::<[u8; 16]>(),
        &argon2::Config::rfc9106_low_mem(),
    )
    .with_kind(ErrorKind::PasswordHashGeneration)?;
    ctx.db
        .mutate(|db| db.as_password_mut().ser(&Some(pwhash)))
        .await
        .result?;

    Ok(())
}

pub async fn set_password_cli(
    HandlerArgs {
        context,
        parent_method,
        method,
        ..
    }: HandlerArgs<CliContext>,
) -> Result<(), Error> {
    let password = rpassword::prompt_password("New Password")?;
    let confirm = rpassword::prompt_password("Confirm Password")?;

    if password != confirm {
        return Err(Error::new(
            eyre!("Passwords do not match"),
            ErrorKind::InvalidRequest,
        ));
    }

    context
        .call_remote::<TunnelContext>(
            &parent_method.iter().chain(method.iter()).join("."),
            to_value(&SetPasswordParams { password })?,
        )
        .await?;

    println!("Password set successfully");

    Ok(())
}
