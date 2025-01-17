use clap::Parser;
use imbl_value::InternedString;
use rpc_toolkit::{from_fn_async, Context, Empty, HandlerArgs, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use torut::onion::OnionAddressV3;
use ts_rs::TS;

use crate::context::{CliContext, RpcContext};
use crate::net::acme::AcmeProvider;
use crate::net::host::HostApiKind;
use crate::prelude::*;
use crate::util::serde::{display_serializable, HandlerExtSerde};

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "kebab-case")]
#[serde(rename_all_fields = "camelCase")]
#[serde(tag = "kind")]
#[ts(export)]
pub enum HostAddress {
    Onion {
        #[ts(type = "string")]
        address: OnionAddressV3,
    },
    Domain {
        #[ts(type = "string")]
        address: InternedString,
        public: bool,
        acme: Option<AcmeProvider>,
    },
}

#[derive(Debug, Deserialize, Serialize, TS)]
pub struct DomainConfig {
    pub public: bool,
    pub acme: Option<AcmeProvider>,
}

pub fn address_api<C: Context, Kind: HostApiKind>(
) -> ParentHandler<C, Kind::Params, Kind::InheritedParams> {
    ParentHandler::<C, Kind::Params, Kind::InheritedParams>::new()
        .subcommand(
            "domain",
            ParentHandler::<C, Empty, Kind::Inheritance>::new()
                .subcommand(
                    "add",
                    from_fn_async(add_domain::<Kind>)
                        .with_metadata("sync_db", Value::Bool(true))
                        .with_inherited(|_, a| a)
                        .no_display()
                        .with_about("Add an address to this host")
                        .with_call_remote::<CliContext>(),
                )
                .subcommand(
                    "remove",
                    from_fn_async(remove_domain::<Kind>)
                        .with_metadata("sync_db", Value::Bool(true))
                        .with_inherited(|_, a| a)
                        .no_display()
                        .with_about("Remove an address from this host")
                        .with_call_remote::<CliContext>(),
                )
                .with_inherited(Kind::inheritance),
        )
        .subcommand(
            "onion",
            ParentHandler::<C, Empty, Kind::Inheritance>::new()
                .subcommand(
                    "add",
                    from_fn_async(add_onion::<Kind>)
                        .with_metadata("sync_db", Value::Bool(true))
                        .with_inherited(|_, a| a)
                        .no_display()
                        .with_about("Add an address to this host")
                        .with_call_remote::<CliContext>(),
                )
                .subcommand(
                    "remove",
                    from_fn_async(remove_onion::<Kind>)
                        .with_metadata("sync_db", Value::Bool(true))
                        .with_inherited(|_, a| a)
                        .no_display()
                        .with_about("Remove an address from this host")
                        .with_call_remote::<CliContext>(),
                )
                .with_inherited(Kind::inheritance),
        )
        .subcommand(
            "list",
            from_fn_async(list_addresses::<Kind>)
                .with_inherited(Kind::inheritance)
                .with_display_serializable()
                .with_custom_display_fn(|HandlerArgs { params, .. }, res| {
                    use prettytable::*;

                    if let Some(format) = params.format {
                        display_serializable(format, res);
                        return Ok(());
                    }

                    let mut table = Table::new();
                    table.add_row(row![bc => "ADDRESS", "PUBLIC", "ACME PROVIDER"]);
                    for address in &res {
                        match address {
                            HostAddress::Onion { address } => {
                                table.add_row(row![address, true, "N/A"]);
                            }
                            HostAddress::Domain {
                                address,
                                public,
                                acme,
                            } => {
                                table.add_row(row![
                                    address,
                                    *public,
                                    acme.as_ref().map(|a| a.0.as_str()).unwrap_or("NONE")
                                ]);
                            }
                        }
                    }

                    table.print_tty(false)?;

                    Ok(())
                })
                .with_about("List addresses for this host")
                .with_call_remote::<CliContext>(),
        )
}

#[derive(Deserialize, Serialize, Parser)]
pub struct AddDomainParams {
    pub domain: InternedString,
    #[arg(long)]
    pub private: bool,
    #[arg(long)]
    pub acme: Option<AcmeProvider>,
}

pub async fn add_domain<Kind: HostApiKind>(
    ctx: RpcContext,
    AddDomainParams {
        domain,
        private,
        acme,
    }: AddDomainParams,
    inheritance: Kind::Inheritance,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            if let Some(acme) = &acme {
                if !db.as_public().as_server_info().as_acme().contains_key(&acme)? {
                    return Err(Error::new(eyre!("unknown acme provider {}, please run acme.init for this provider first", acme.0), ErrorKind::InvalidRequest));
                }
            }

            Kind::host_for(&inheritance, db)?
                .as_domains_mut()
                .insert(
                    &domain,
                    &DomainConfig {
                        public: !private,
                        acme,
                    },
                )
        })
        .await?;
    Kind::update_host(&ctx, inheritance).await?;

    Ok(())
}

#[derive(Deserialize, Serialize, Parser)]
pub struct RemoveDomainParams {
    pub domain: InternedString,
}

pub async fn remove_domain<Kind: HostApiKind>(
    ctx: RpcContext,
    RemoveDomainParams { domain }: RemoveDomainParams,
    inheritance: Kind::Inheritance,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            Kind::host_for(&inheritance, db)?
                .as_domains_mut()
                .remove(&domain)
        })
        .await?;
    Kind::update_host(&ctx, inheritance).await?;

    Ok(())
}

#[derive(Deserialize, Serialize, Parser)]
pub struct OnionParams {
    pub onion: String,
}

pub async fn add_onion<Kind: HostApiKind>(
    ctx: RpcContext,
    OnionParams { onion }: OnionParams,
    inheritance: Kind::Inheritance,
) -> Result<(), Error> {
    let onion = onion
        .strip_suffix(".onion")
        .ok_or_else(|| {
            Error::new(
                eyre!("onion hostname must end in .onion"),
                ErrorKind::InvalidOnionAddress,
            )
        })?
        .parse::<OnionAddressV3>()?;
    ctx.db
        .mutate(|db| {
            db.as_private().as_key_store().as_onion().get_key(&onion)?;

            Kind::host_for(&inheritance, db)?
                .as_onions_mut()
                .mutate(|a| Ok(a.insert(onion)))
        })
        .await?;

    Kind::update_host(&ctx, inheritance).await?;

    Ok(())
}

pub async fn remove_onion<Kind: HostApiKind>(
    ctx: RpcContext,
    OnionParams { onion }: OnionParams,
    inheritance: Kind::Inheritance,
) -> Result<(), Error> {
    let onion = onion
        .strip_suffix(".onion")
        .ok_or_else(|| {
            Error::new(
                eyre!("onion hostname must end in .onion"),
                ErrorKind::InvalidOnionAddress,
            )
        })?
        .parse::<OnionAddressV3>()?;
    ctx.db
        .mutate(|db| {
            Kind::host_for(&inheritance, db)?
                .as_onions_mut()
                .mutate(|a| Ok(a.remove(&onion)))
        })
        .await?;

    Kind::update_host(&ctx, inheritance).await?;

    Ok(())
}

pub async fn list_addresses<Kind: HostApiKind>(
    ctx: RpcContext,
    _: Empty,
    inheritance: Kind::Inheritance,
) -> Result<Vec<HostAddress>, Error> {
    Ok(Kind::host_for(&inheritance, &mut ctx.db.peek().await)?
        .de()?
        .addresses()
        .collect())
}
