use std::collections::BTreeSet;
use std::net::Ipv4Addr;

use clap::Parser;
use imbl_value::InternedString;
use rpc_toolkit::{Context, Empty, HandlerArgs, HandlerExt, ParentHandler, from_fn_async};
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::GatewayId;
use crate::context::{CliContext, RpcContext};
use crate::db::model::DatabaseModel;
use crate::hostname::ServerHostname;
use crate::net::acme::AcmeProvider;
use crate::net::gateway::{CheckDnsParams, CheckPortParams, CheckPortRes, check_dns, check_port};
use crate::net::host::{HostApiKind, all_hosts};
use crate::prelude::*;
use crate::util::serde::{HandlerExtSerde, display_serializable};

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct HostAddress {
    pub address: InternedString,
    pub public: Option<PublicDomainConfig>,
    pub private: Option<BTreeSet<GatewayId>>,
}

#[derive(Debug, Clone, Deserialize, Serialize, TS)]
#[ts(export)]
pub struct PublicDomainConfig {
    pub gateway: GatewayId,
    pub acme: Option<AcmeProvider>,
}

fn handle_duplicates(db: &mut DatabaseModel) -> Result<(), Error> {
    let mut domains = BTreeSet::<InternedString>::new();
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
        let public = host.as_public_domains().keys()?;
        for domain in &public {
            check_domain(&mut domains, domain.clone())?;
        }
        for domain in host.as_private_domains().keys()? {
            if !public.contains(&domain) {
                check_domain(&mut domains, domain)?;
            }
        }
    }
    for host in not_in_use {
        host.as_public_domains_mut()
            .mutate(|d| Ok(d.retain(|d, _| !domains.contains(d))))?;
        host.as_private_domains_mut()
            .mutate(|d| Ok(d.retain(|d, _| !domains.contains(d))))?;

        let public = host.as_public_domains().keys()?;
        for domain in &public {
            check_domain(&mut domains, domain.clone())?;
        }
        for domain in host.as_private_domains().keys()? {
            if !public.contains(&domain) {
                check_domain(&mut domains, domain)?;
            }
        }
    }
    Ok(())
}

pub fn address_api<C: Context, Kind: HostApiKind>()
-> ParentHandler<C, Kind::Params, Kind::InheritedParams> {
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
                                .with_about("about.add-public-domain-to-host")
                                .with_call_remote::<CliContext>(),
                        )
                        .subcommand(
                            "remove",
                            from_fn_async(remove_public_domain::<Kind>)
                                .with_metadata("sync_db", Value::Bool(true))
                                .with_inherited(|_, a| a)
                                .no_display()
                                .with_about("about.remove-public-domain-from-host")
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
                                .with_about("about.add-private-domain-to-host")
                                .with_call_remote::<CliContext>(),
                        )
                        .subcommand(
                            "remove",
                            from_fn_async(remove_private_domain::<Kind>)
                                .with_metadata("sync_db", Value::Bool(true))
                                .with_inherited(|_, a| a)
                                .no_display()
                                .with_about("about.remove-private-domain-from-host")
                                .with_call_remote::<CliContext>(),
                        )
                        .with_inherited(|_, a| a),
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
                    todo!("find a good way to represent this");

                    table.print_tty(false)?;

                    Ok(())
                })
                .with_about("about.list-addresses-for-host")
                .with_call_remote::<CliContext>(),
        )
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[ts(export)]
pub struct AddPublicDomainParams {
    #[arg(help = "help.arg.fqdn")]
    pub fqdn: InternedString,
    #[arg(long, help = "help.arg.acme-provider")]
    pub acme: Option<AcmeProvider>,
    #[arg(help = "help.arg.gateway-id")]
    pub gateway: GatewayId,
}

#[derive(Debug, Clone, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AddPublicDomainRes {
    #[ts(type = "string | null")]
    pub dns: Option<Ipv4Addr>,
    pub port: Vec<CheckPortRes>,
}

pub async fn add_public_domain<Kind: HostApiKind>(
    ctx: RpcContext,
    AddPublicDomainParams {
        fqdn,
        acme,
        gateway,
    }: AddPublicDomainParams,
    inheritance: Kind::Inheritance,
) -> Result<AddPublicDomainRes, Error> {
    let ports = ctx
        .db
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
                .insert(
                    &fqdn,
                    &PublicDomainConfig {
                        acme,
                        gateway: gateway.clone(),
                    },
                )?;
            handle_duplicates(db)?;
            let hostname = ServerHostname::load(db.as_public().as_server_info())?;
            let gateways = db
                .as_public()
                .as_server_info()
                .as_network()
                .as_gateways()
                .de()?;
            let available_ports = db.as_private().as_available_ports().de()?;
            let host = Kind::host_for(&inheritance, db)?;
            host.update_addresses(&hostname, &gateways, &available_ports)?;
            let bindings = host.as_bindings().de()?;
            let ports: BTreeSet<u16> = bindings
                .values()
                .flat_map(|b| &b.addresses.available)
                .filter(|a| a.public && a.hostname == fqdn)
                .filter_map(|a| a.port)
                .collect();
            Ok(ports)
        })
        .await
        .result?;

    let ctx2 = ctx.clone();
    let fqdn2 = fqdn.clone();

    let (dns_result, port_results) = tokio::join!(
        async {
            tokio::task::spawn_blocking(move || {
                crate::net::dns::query_dns(ctx2, crate::net::dns::QueryDnsParams { fqdn: fqdn2 })
            })
            .await
            .with_kind(ErrorKind::Unknown)?
        },
        futures::future::join_all(ports.into_iter().map(|port| {
            check_port(
                ctx.clone(),
                CheckPortParams {
                    port,
                    gateway: gateway.clone(),
                },
            )
        }))
    );

    Ok(AddPublicDomainRes {
        dns: dns_result?,
        port: port_results.into_iter().collect::<Result<Vec<_>, _>>()?,
    })
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[ts(export)]
pub struct RemoveDomainParams {
    #[arg(help = "help.arg.fqdn")]
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
                .remove(&fqdn)?;
            let hostname = ServerHostname::load(db.as_public().as_server_info())?;
            let gateways = db
                .as_public()
                .as_server_info()
                .as_network()
                .as_gateways()
                .de()?;
            let ports = db.as_private().as_available_ports().de()?;
            Kind::host_for(&inheritance, db)?.update_addresses(&hostname, &gateways, &ports)
        })
        .await
        .result?;

    Ok(())
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[ts(export)]
pub struct AddPrivateDomainParams {
    #[arg(help = "help.arg.fqdn")]
    pub fqdn: InternedString,
    pub gateway: GatewayId,
}

pub async fn add_private_domain<Kind: HostApiKind>(
    ctx: RpcContext,
    AddPrivateDomainParams { fqdn, gateway }: AddPrivateDomainParams,
    inheritance: Kind::Inheritance,
) -> Result<bool, Error> {
    ctx.db
        .mutate(|db| {
            Kind::host_for(&inheritance, db)?
                .as_private_domains_mut()
                .upsert(&fqdn, || Ok(BTreeSet::new()))?
                .mutate(|d| Ok(d.insert(gateway.clone())))?;
            handle_duplicates(db)?;
            let hostname = ServerHostname::load(db.as_public().as_server_info())?;
            let gateways = db
                .as_public()
                .as_server_info()
                .as_network()
                .as_gateways()
                .de()?;
            let ports = db.as_private().as_available_ports().de()?;
            Kind::host_for(&inheritance, db)?.update_addresses(&hostname, &gateways, &ports)
        })
        .await
        .result?;

    check_dns(ctx, CheckDnsParams { gateway }).await
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
                .mutate(|d| Ok(d.remove(&domain)))?;
            let hostname = ServerHostname::load(db.as_public().as_server_info())?;
            let gateways = db
                .as_public()
                .as_server_info()
                .as_network()
                .as_gateways()
                .de()?;
            let ports = db.as_private().as_available_ports().de()?;
            Kind::host_for(&inheritance, db)?.update_addresses(&hostname, &gateways, &ports)
        })
        .await
        .result?;

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
