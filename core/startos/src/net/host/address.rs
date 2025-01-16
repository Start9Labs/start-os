use clap::Parser;
use imbl_value::InternedString;
use models::{HostId, PackageId};
use rpc_toolkit::{from_fn_async, Context, Empty, HandlerArgs, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use torut::onion::OnionAddressV3;
use ts_rs::TS;

use crate::context::{CliContext, RpcContext};
use crate::net::acme::AcmeProvider;
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

#[derive(Deserialize, Serialize, Parser)]
pub struct AddressApiParams {
    host: HostId,
}

pub fn address<C: Context>() -> ParentHandler<C, AddressApiParams, PackageId> {
    ParentHandler::<C, AddressApiParams, PackageId>::new()
        .subcommand(
            "domain",
            ParentHandler::<C, Empty, (PackageId, HostId)>::new()
                .subcommand(
                    "add",
                    from_fn_async(add_domain)
                        .with_metadata("sync_db", Value::Bool(true))
                        .with_inherited(|_, a| a)
                        .no_display()
                        .with_about("Add an address to this host")
                        .with_call_remote::<CliContext>(),
                )
                .subcommand(
                    "remove",
                    from_fn_async(remove_domain)
                        .with_metadata("sync_db", Value::Bool(true))
                        .with_inherited(|_, a| a)
                        .no_display()
                        .with_about("Remove an address from this host")
                        .with_call_remote::<CliContext>(),
                )
                .with_inherited(|AddressApiParams { host }, package| (package, host)),
        )
        .subcommand(
            "onion",
            ParentHandler::<C, Empty, (PackageId, HostId)>::new()
                .subcommand(
                    "add",
                    from_fn_async(add_onion)
                        .with_metadata("sync_db", Value::Bool(true))
                        .with_inherited(|_, a| a)
                        .no_display()
                        .with_about("Add an address to this host")
                        .with_call_remote::<CliContext>(),
                )
                .subcommand(
                    "remove",
                    from_fn_async(remove_onion)
                        .with_metadata("sync_db", Value::Bool(true))
                        .with_inherited(|_, a| a)
                        .no_display()
                        .with_about("Remove an address from this host")
                        .with_call_remote::<CliContext>(),
                )
                .with_inherited(|AddressApiParams { host }, package| (package, host)),
        )
        .subcommand(
            "list",
            from_fn_async(list_addresses)
                .with_inherited(|AddressApiParams { host }, package| (package, host))
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

pub async fn add_domain(
    ctx: RpcContext,
    AddDomainParams {
        domain,
        private,
        acme,
    }: AddDomainParams,
    (package, host): (PackageId, HostId),
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            if let Some(acme) = &acme {
                if !db.as_public().as_server_info().as_acme().contains_key(&acme)? {
                    return Err(Error::new(eyre!("unknown acme provider {}, please run acme.init for this provider first", acme.0), ErrorKind::InvalidRequest));
                }
            }

            db.as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(&package)
                .or_not_found(&package)?
                .as_hosts_mut()
                .as_idx_mut(&host)
                .or_not_found(&host)?
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
    let service = ctx.services.get(&package).await;
    let service_ref = service.as_ref().or_not_found(&package)?;
    service_ref.update_host(host).await?;

    Ok(())
}

#[derive(Deserialize, Serialize, Parser)]
pub struct RemoveDomainParams {
    pub domain: InternedString,
}

pub async fn remove_domain(
    ctx: RpcContext,
    RemoveDomainParams { domain }: RemoveDomainParams,
    (package, host): (PackageId, HostId),
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            db.as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(&package)
                .or_not_found(&package)?
                .as_hosts_mut()
                .as_idx_mut(&host)
                .or_not_found(&host)?
                .as_domains_mut()
                .remove(&domain)
        })
        .await?;
    let service = ctx.services.get(&package).await;
    let service_ref = service.as_ref().or_not_found(&package)?;
    service_ref.update_host(host).await?;

    Ok(())
}

#[derive(Deserialize, Serialize, Parser)]
pub struct OnionParams {
    pub onion: String,
}

pub async fn add_onion(
    ctx: RpcContext,
    OnionParams { onion }: OnionParams,
    (package, host): (PackageId, HostId),
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

            db.as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(&package)
                .or_not_found(&package)?
                .as_hosts_mut()
                .as_idx_mut(&host)
                .or_not_found(&host)?
                .as_onions_mut()
                .mutate(|a| Ok(a.insert(onion)))
        })
        .await?;
    let service = ctx.services.get(&package).await;
    let service_ref = service.as_ref().or_not_found(&package)?;
    service_ref.update_host(host).await?;

    Ok(())
}

pub async fn remove_onion(
    ctx: RpcContext,
    OnionParams { onion }: OnionParams,
    (package, host): (PackageId, HostId),
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
            db.as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(&package)
                .or_not_found(&package)?
                .as_hosts_mut()
                .as_idx_mut(&host)
                .or_not_found(&host)?
                .as_onions_mut()
                .mutate(|a| Ok(a.remove(&onion)))
        })
        .await?;
    let service = ctx.services.get(&package).await;
    let service_ref = service.as_ref().or_not_found(&package)?;
    service_ref.update_host(host).await?;

    Ok(())
}

pub async fn list_addresses(
    ctx: RpcContext,
    _: Empty,
    (package, host): (PackageId, HostId),
) -> Result<Vec<HostAddress>, Error> {
    Ok(ctx
        .db
        .peek()
        .await
        .into_public()
        .into_package_data()
        .into_idx(&package)
        .or_not_found(&package)?
        .into_hosts()
        .into_idx(&host)
        .or_not_found(&host)?
        .de()?
        .addresses()
        .collect())
}
