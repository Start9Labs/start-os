use std::collections::BTreeSet;
use std::net::Ipv4Addr;

use clap::Parser;
use imbl_value::InternedString;
use models::GatewayId;
use rpc_toolkit::{from_fn_async, Context, Empty, HandlerArgs, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::context::{CliContext, RpcContext};
use crate::db::model::DatabaseModel;
use crate::net::acme::AcmeProvider;
use crate::net::host::{all_hosts, HostApiKind};
use crate::net::tor::OnionAddress;
use crate::prelude::*;
use crate::util::serde::{display_serializable, HandlerExtSerde};

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
#[serde(rename_all_fields = "camelCase")]
#[serde(tag = "kind")]
pub enum HostAddress {
    Onion {
        address: OnionAddress,
    },
    Domain {
        address: InternedString,
        public: Option<PublicDomainConfig>,
        private: bool,
    },
}

#[derive(Debug, Clone, Deserialize, Serialize, TS)]
pub struct PublicDomainConfig {
    pub gateway: GatewayId,
    pub acme: Option<AcmeProvider>,
}

fn handle_duplicates(db: &mut DatabaseModel) -> Result<(), Error> {
    let mut onions = BTreeSet::<OnionAddress>::new();
    let mut domains = BTreeSet::<InternedString>::new();
    let check_onion = |onions: &mut BTreeSet<OnionAddress>, onion: OnionAddress| {
        if onions.contains(&onion) {
            return Err(Error::new(
                eyre!("onion address {onion} is already in use"),
                ErrorKind::InvalidRequest,
            ));
        }
        onions.insert(onion);
        Ok(())
    };
    let check_domain = |domains: &mut BTreeSet<InternedString>, domain: InternedString| {
        if domains.contains(&domain) {
            return Err(Error::new(
                eyre!("domain {domain} is already in use"),
                ErrorKind::InvalidRequest,
            ));
        }
        domains.insert(domain);
        Ok(())
    };
    let mut not_in_use = Vec::new();
    for host in all_hosts(db) {
        let host = host?;
        let in_use = host.as_bindings().de()?.values().any(|v| v.enabled);
        if !in_use {
            not_in_use.push(host);
            continue;
        }
        for onion in host.as_onions().de()? {
            check_onion(&mut onions, onion)?;
        }
        let public = host.as_public_domains().keys()?;
        for domain in &public {
            check_domain(&mut domains, domain.clone())?;
        }
        for domain in host.as_private_domains().de()? {
            if !public.contains(&domain) {
                check_domain(&mut domains, domain)?;
            }
        }
    }
    for host in not_in_use {
        host.as_onions_mut()
            .mutate(|o| Ok(o.retain(|o| !onions.contains(o))))?;
        host.as_public_domains_mut()
            .mutate(|d| Ok(d.retain(|d, _| !domains.contains(d))))?;
        host.as_private_domains_mut()
            .mutate(|d| Ok(d.retain(|d| !domains.contains(d))))?;

        for onion in host.as_onions().de()? {
            check_onion(&mut onions, onion)?;
        }
        let public = host.as_public_domains().keys()?;
        for domain in &public {
            check_domain(&mut domains, domain.clone())?;
        }
        for domain in host.as_private_domains().de()? {
            if !public.contains(&domain) {
                check_domain(&mut domains, domain)?;
            }
        }
    }
    Ok(())
}

pub fn address_api<C: Context, Kind: HostApiKind>(
) -> ParentHandler<C, Kind::Params, Kind::InheritedParams> {
    ParentHandler::<C, Kind::Params, Kind::InheritedParams>::new()
        .subcommand(
            "domain",
            ParentHandler::<C, Empty, Kind::Inheritance>::new()
                .subcommand(
                    "public",
                    ParentHandler::<C, Empty, Kind::Inheritance>::new()
                        .subcommand(
                            "add",
                            from_fn_async(add_public_domain::<Kind>)
                                .with_metadata("sync_db", Value::Bool(true))
                                .with_inherited(|_, a| a)
                                .no_display()
                                .with_about("Add a public domain to this host")
                                .with_call_remote::<CliContext>(),
                        )
                        .subcommand(
                            "remove",
                            from_fn_async(remove_public_domain::<Kind>)
                                .with_metadata("sync_db", Value::Bool(true))
                                .with_inherited(|_, a| a)
                                .no_display()
                                .with_about("Remove a public domain from this host")
                                .with_call_remote::<CliContext>(),
                        )
                        .with_inherited(|_, a| a),
                )
                .subcommand(
                    "private",
                    ParentHandler::<C, Empty, Kind::Inheritance>::new()
                        .subcommand(
                            "add",
                            from_fn_async(add_private_domain::<Kind>)
                                .with_metadata("sync_db", Value::Bool(true))
                                .with_inherited(|_, a| a)
                                .no_display()
                                .with_about("Add a private domain to this host")
                                .with_call_remote::<CliContext>(),
                        )
                        .subcommand(
                            "remove",
                            from_fn_async(remove_private_domain::<Kind>)
                                .with_metadata("sync_db", Value::Bool(true))
                                .with_inherited(|_, a| a)
                                .no_display()
                                .with_about("Remove a private domain from this host")
                                .with_call_remote::<CliContext>(),
                        )
                        .with_inherited(|_, a| a),
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
                        display_serializable(format, res)?;
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
                                public: Some(PublicDomainConfig { gateway, acme }),
                                private,
                            } => {
                                table.add_row(row![
                                    address,
                                    &format!(
                                        "{} ({gateway})",
                                        if *private { "YES" } else { "ONLY" }
                                    ),
                                    acme.as_ref().map(|a| a.0.as_str()).unwrap_or("NONE")
                                ]);
                            }
                            HostAddress::Domain {
                                address,
                                public: None,
                                ..
                            } => {
                                table.add_row(row![address, &format!("NO"), "N/A"]);
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
pub struct AddPublicDomainParams {
    pub fqdn: InternedString,
    #[arg(long)]
    pub acme: Option<AcmeProvider>,
    pub gateway: GatewayId,
}

pub async fn add_public_domain<Kind: HostApiKind>(
    ctx: RpcContext,
    AddPublicDomainParams {
        fqdn,
        acme,
        gateway,
    }: AddPublicDomainParams,
    inheritance: Kind::Inheritance,
) -> Result<Option<Ipv4Addr>, Error> {
    ctx.db
        .mutate(|db| {
            if let Some(acme) = &acme {
                if !db
                    .as_public()
                    .as_server_info()
                    .as_network()
                    .as_acme()
                    .contains_key(&acme)?
                {
                    return Err(Error::new(eyre!("unknown acme provider {}, please run acme.init for this provider first", acme.0), ErrorKind::InvalidRequest));
                }
            }

            Kind::host_for(&inheritance, db)?
                .as_public_domains_mut()
                .insert(&fqdn, &PublicDomainConfig { acme, gateway })?;
            handle_duplicates(db)
        })
        .await
        .result?;
    Kind::sync_host(&ctx, inheritance).await?;

    tokio::task::spawn_blocking(|| {
        crate::net::dns::query_dns(ctx, crate::net::dns::QueryDnsParams { fqdn })
    })
    .await
    .with_kind(ErrorKind::Unknown)?
}

#[derive(Deserialize, Serialize, Parser)]
pub struct RemoveDomainParams {
    pub fqdn: InternedString,
}

pub async fn remove_public_domain<Kind: HostApiKind>(
    ctx: RpcContext,
    RemoveDomainParams { fqdn }: RemoveDomainParams,
    inheritance: Kind::Inheritance,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            Kind::host_for(&inheritance, db)?
                .as_public_domains_mut()
                .remove(&fqdn)
        })
        .await
        .result?;
    Kind::sync_host(&ctx, inheritance).await?;

    Ok(())
}

#[derive(Deserialize, Serialize, Parser)]
pub struct AddPrivateDomainParams {
    pub fqdn: InternedString,
}

pub async fn add_private_domain<Kind: HostApiKind>(
    ctx: RpcContext,
    AddPrivateDomainParams { fqdn }: AddPrivateDomainParams,
    inheritance: Kind::Inheritance,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            Kind::host_for(&inheritance, db)?
                .as_private_domains_mut()
                .mutate(|d| Ok(d.insert(fqdn)))?;
            handle_duplicates(db)
        })
        .await
        .result?;
    Kind::sync_host(&ctx, inheritance).await?;

    Ok(())
}

pub async fn remove_private_domain<Kind: HostApiKind>(
    ctx: RpcContext,
    RemoveDomainParams { fqdn: domain }: RemoveDomainParams,
    inheritance: Kind::Inheritance,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            Kind::host_for(&inheritance, db)?
                .as_private_domains_mut()
                .mutate(|d| Ok(d.remove(&domain)))
        })
        .await
        .result?;
    Kind::sync_host(&ctx, inheritance).await?;

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
    let onion = onion.parse::<OnionAddress>()?;
    ctx.db
        .mutate(|db| {
            db.as_private().as_key_store().as_onion().get_key(&onion)?;

            Kind::host_for(&inheritance, db)?
                .as_onions_mut()
                .mutate(|a| Ok(a.insert(onion)))?;
            handle_duplicates(db)
        })
        .await
        .result?;

    Kind::sync_host(&ctx, inheritance).await?;

    Ok(())
}

pub async fn remove_onion<Kind: HostApiKind>(
    ctx: RpcContext,
    OnionParams { onion }: OnionParams,
    inheritance: Kind::Inheritance,
) -> Result<(), Error> {
    let onion = onion.parse::<OnionAddress>()?;
    ctx.db
        .mutate(|db| {
            Kind::host_for(&inheritance, db)?
                .as_onions_mut()
                .mutate(|a| Ok(a.remove(&onion)))
        })
        .await
        .result?;

    Kind::sync_host(&ctx, inheritance).await?;

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
