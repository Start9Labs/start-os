use std::collections::BTreeMap;
use std::path::PathBuf;

use clap::Parser;
use itertools::Itertools;
use rpc_toolkit::{from_fn_async, AnyContext, CallRemote, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::context::CliContext;
use crate::prelude::*;
use crate::registry::context::RegistryContext;
use crate::registry::signer::{ContactInfo, SignerInfo, SignerKey};
use crate::registry::RegistryDatabase;
use crate::rpc_continuations::RequestGuid;
use crate::util::serde::{display_serializable, HandlerExtSerde, Pem, WithIoFormat};

pub fn admin_api() -> ParentHandler {
    ParentHandler::new()
        .subcommand("signer", signers_api())
        .subcommand("add", from_fn_async(add_admin).no_cli())
        .subcommand("add", from_fn_async(cli_add_admin).no_display())
        .subcommand(
            "list",
            from_fn_async(list_admins)
                .with_display_serializable()
                .with_custom_display_fn::<AnyContext, _>(|handle, result| {
                    Ok(display_signers(handle.params, result))
                })
                .with_call_remote::<CliContext>(),
        )
}

fn signers_api() -> ParentHandler {
    ParentHandler::new()
        .subcommand(
            "list",
            from_fn_async(list_signers)
                .with_metadata("admin", Value::Bool(true))
                .with_display_serializable()
                .with_custom_display_fn::<AnyContext, _>(|handle, result| {
                    Ok(display_signers(handle.params, result))
                })
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "add",
            from_fn_async(add_signer)
                .with_metadata("admin", Value::Bool(true))
                .no_cli(),
        )
        .subcommand("add", from_fn_async(cli_add_signer).no_display())
}

impl Model<BTreeMap<RequestGuid, SignerInfo>> {
    pub fn get_signer(&self, key: &SignerKey) -> Result<RequestGuid, Error> {
        self.as_entries()?
            .into_iter()
            .map(|(guid, s)| Ok::<_, Error>((guid, s.as_keys().de()?)))
            .filter_ok(|(_, s)| s.contains(key))
            .next()
            .transpose()?
            .map(|(a, _)| a)
            .ok_or_else(|| Error::new(eyre!("unknown signer"), ErrorKind::Authorization))
    }

    pub fn get_signer_info(&self, key: &SignerKey) -> Result<(RequestGuid, SignerInfo), Error> {
        self.as_entries()?
            .into_iter()
            .map(|(guid, s)| Ok::<_, Error>((guid, s.de()?)))
            .filter_ok(|(_, s)| s.keys.contains(key))
            .next()
            .transpose()?
            .ok_or_else(|| Error::new(eyre!("unknown signer"), ErrorKind::Authorization))
    }

    pub fn add_signer(&mut self, signer: &SignerInfo) -> Result<(), Error> {
        if let Some((guid, s)) = self
            .as_entries()?
            .into_iter()
            .map(|(guid, s)| Ok::<_, Error>((guid, s.de()?)))
            .filter_ok(|(_, s)| !s.keys.is_disjoint(&signer.keys))
            .next()
            .transpose()?
        {
            return Err(Error::new(
                eyre!(
                    "A signer {} ({}) already exists with a matching key",
                    guid,
                    s.name
                ),
                ErrorKind::InvalidRequest,
            ));
        }
        self.insert(&RequestGuid::new(), signer)
    }
}

pub async fn list_signers(
    ctx: RegistryContext,
) -> Result<BTreeMap<RequestGuid, SignerInfo>, Error> {
    ctx.db.peek().await.into_index().into_signers().de()
}

pub fn display_signers<T>(params: WithIoFormat<T>, signers: BTreeMap<RequestGuid, SignerInfo>) {
    use prettytable::*;

    if let Some(format) = params.format {
        return display_serializable(format, signers);
    }

    let mut table = Table::new();
    table.add_row(row![bc =>
        "ID",
        "NAME",
        "CONTACT",
        "KEYS",
    ]);
    for (id, info) in signers {
        table.add_row(row![
            id.as_ref(),
            &info.name,
            &info.contact.into_iter().join("\n"),
            &info.keys.into_iter().join("\n"),
        ]);
    }
    table.print_tty(false).unwrap();
}

pub async fn add_signer(ctx: RegistryContext, signer: SignerInfo) -> Result<(), Error> {
    ctx.db
        .mutate(|db| db.as_index_mut().as_signers_mut().add_signer(&signer))
        .await
}

#[derive(Debug, Deserialize, Serialize, Parser)]
#[command(rename_all = "kebab-case")]
#[serde(rename_all = "camelCase")]
pub struct CliAddSignerParams {
    #[arg(long = "name", short = 'n')]
    pub name: String,
    #[arg(long = "contact", short = 'c')]
    pub contact: Vec<ContactInfo>,
    #[arg(long = "ed25519-key")]
    pub ed25519_keys: Vec<Pem<ed25519_dalek::VerifyingKey>>,
    pub database: Option<PathBuf>,
}

pub async fn cli_add_signer(
    ctx: CliContext,
    CliAddSignerParams {
        name,
        contact,
        ed25519_keys,
        database,
    }: CliAddSignerParams,
) -> Result<(), Error> {
    let signer = SignerInfo {
        name,
        contact,
        keys: ed25519_keys.into_iter().map(SignerKey::Ed25519).collect(),
    };
    if let Some(database) = database {
        TypedPatchDb::<RegistryDatabase>::load(PatchDb::open(database).await?)
            .await?
            .mutate(|db| db.as_index_mut().as_signers_mut().add_signer(&signer))
            .await?;
    } else {
        <CliContext as CallRemote<RegistryContext>>::call_remote(
            &ctx,
            "admin.signer.add",
            to_value(&signer)?,
        )
        .await?;
    }
    Ok(())
}

#[derive(Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AddAdminParams {
    #[ts(type = "string")]
    pub signer: RequestGuid,
}

pub async fn add_admin(
    ctx: RegistryContext,
    AddAdminParams { signer }: AddAdminParams,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            ensure_code!(
                db.as_index().as_signers().contains_key(&signer)?,
                ErrorKind::InvalidRequest,
                "unknown signer {signer}"
            );
            db.as_admins_mut().mutate(|a| Ok(a.insert(signer)))?;
            Ok(())
        })
        .await
}

#[derive(Debug, Deserialize, Serialize, Parser)]
#[command(rename_all = "kebab-case")]
#[serde(rename_all = "camelCase")]
pub struct CliAddAdminParams {
    pub signer: RequestGuid,
    pub database: Option<PathBuf>,
}

pub async fn cli_add_admin(
    ctx: CliContext,
    CliAddAdminParams { signer, database }: CliAddAdminParams,
) -> Result<(), Error> {
    if let Some(database) = database {
        TypedPatchDb::<RegistryDatabase>::load(PatchDb::open(database).await?)
            .await?
            .mutate(|db| {
                ensure_code!(
                    db.as_index().as_signers().contains_key(&signer)?,
                    ErrorKind::InvalidRequest,
                    "unknown signer {signer}"
                );
                db.as_admins_mut().mutate(|a| Ok(a.insert(signer)))?;
                Ok(())
            })
            .await?;
    } else {
        <CliContext as CallRemote<RegistryContext>>::call_remote(
            &ctx,
            "admin.add",
            to_value(&AddAdminParams { signer })?,
        )
        .await?;
    }
    Ok(())
}

pub async fn list_admins(ctx: RegistryContext) -> Result<BTreeMap<RequestGuid, SignerInfo>, Error> {
    let db = ctx.db.peek().await;
    let admins = db.as_admins().de()?;
    Ok(db
        .into_index()
        .into_signers()
        .de()?
        .into_iter()
        .filter(|(id, _)| admins.contains(id))
        .collect())
}
