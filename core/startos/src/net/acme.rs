use std::collections::{BTreeMap, BTreeSet};
use std::str::FromStr;

use clap::builder::ValueParserFactory;
use clap::Parser;
use imbl_value::InternedString;
use itertools::Itertools;
use models::{ErrorData, FromStrParser};
use openssl::pkey::{PKey, Private};
use openssl::x509::X509;
use rpc_toolkit::{from_fn_async, Context, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use url::Url;

use crate::context::{CliContext, RpcContext};
use crate::db::model::public::AcmeSettings;
use crate::db::model::Database;
use crate::prelude::*;
use crate::util::serde::Pem;

#[derive(Debug, Default, Deserialize, Serialize, HasModel)]
#[model = "Model<Self>"]
pub struct AcmeCertStore {
    pub accounts: BTreeMap<JsonKey<Vec<String>>, Pem<PKey<Private>>>,
    pub certs: BTreeMap<Url, BTreeMap<JsonKey<BTreeSet<InternedString>>, AcmeCert>>,
}
impl AcmeCertStore {
    pub fn new() -> Self {
        Self::default()
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct AcmeCert {
    pub key: Pem<PKey<Private>>,
    pub fullchain: Vec<Pem<X509>>,
}

pub struct AcmeCertCache<'a>(pub &'a TypedPatchDb<Database>);
#[async_trait::async_trait]
impl<'a> async_acme::cache::AcmeCache for AcmeCertCache<'a> {
    type Error = ErrorData;

    async fn read_account(&self, contacts: &[&str]) -> Result<Option<Vec<u8>>, Self::Error> {
        let contacts = JsonKey::new(contacts.into_iter().map(|s| (*s).to_owned()).collect_vec());
        let Some(account) = self
            .0
            .peek()
            .await
            .into_private()
            .into_key_store()
            .into_acme()
            .into_accounts()
            .into_idx(&contacts)
        else {
            return Ok(None);
        };
        Ok(Some(
            account
                .de()?
                .0
                .private_key_to_der()
                .with_kind(ErrorKind::OpenSsl)?,
        ))
    }

    async fn write_account(&self, contacts: &[&str], contents: &[u8]) -> Result<(), Self::Error> {
        let contacts = JsonKey::new(contacts.into_iter().map(|s| (*s).to_owned()).collect_vec());
        let key = Pem::new(
            PKey::<Private>::private_key_from_der(contents).with_kind(ErrorKind::OpenSsl)?,
        );
        self.0
            .mutate(|db| {
                db.as_private_mut()
                    .as_key_store_mut()
                    .as_acme_mut()
                    .as_accounts_mut()
                    .insert(&contacts, &key)
            })
            .await?;
        Ok(())
    }

    async fn read_certificate(
        &self,
        domains: &[String],
        directory_url: &str,
    ) -> Result<Option<(String, String)>, Self::Error> {
        let domains = JsonKey::new(domains.into_iter().map(InternedString::intern).collect());
        let directory_url = directory_url
            .parse::<Url>()
            .with_kind(ErrorKind::ParseUrl)?;
        let Some(cert) = self
            .0
            .peek()
            .await
            .into_private()
            .into_key_store()
            .into_acme()
            .into_certs()
            .into_idx(&directory_url)
            .and_then(|a| a.into_idx(&domains))
        else {
            return Ok(None);
        };
        let cert = cert.de()?;
        Ok(Some((
            String::from_utf8(
                cert.key
                    .0
                    .private_key_to_pem_pkcs8()
                    .with_kind(ErrorKind::OpenSsl)?,
            )
            .with_kind(ErrorKind::Utf8)?,
            cert.fullchain
                .into_iter()
                .map(|cert| {
                    String::from_utf8(cert.0.to_pem().with_kind(ErrorKind::OpenSsl)?)
                        .with_kind(ErrorKind::Utf8)
                })
                .collect::<Result<Vec<_>, _>>()?
                .join("\n"),
        )))
    }

    async fn write_certificate(
        &self,
        domains: &[String],
        directory_url: &str,
        key_pem: &str,
        certificate_pem: &str,
    ) -> Result<(), Self::Error> {
        let domains = JsonKey::new(domains.into_iter().map(InternedString::intern).collect());
        let directory_url = directory_url
            .parse::<Url>()
            .with_kind(ErrorKind::ParseUrl)?;
        let cert = AcmeCert {
            key: Pem(PKey::<Private>::private_key_from_pem(key_pem.as_bytes())
                .with_kind(ErrorKind::OpenSsl)?),
            fullchain: X509::stack_from_pem(certificate_pem.as_bytes())
                .with_kind(ErrorKind::OpenSsl)?
                .into_iter()
                .map(Pem)
                .collect(),
        };
        self.0
            .mutate(|db| {
                db.as_private_mut()
                    .as_key_store_mut()
                    .as_acme_mut()
                    .as_certs_mut()
                    .upsert(&directory_url, || Ok(BTreeMap::new()))?
                    .insert(&domains, &cert)
            })
            .await?;

        Ok(())
    }
}

pub fn acme<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "init",
            from_fn_async(init)
                .with_about("Setup ACME certificate acquisition")
                .no_display()
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "domain",
            domain::<C>()
                .with_about("Add, remove, or view domains for which to acquire ACME certificates"),
        )
}

#[derive(Clone, Deserialize, Serialize)]
pub struct AcmeProvider(pub Url);
impl FromStr for AcmeProvider {
    type Err = <Url as FromStr>::Err;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "letsencrypt" => async_acme::acme::LETS_ENCRYPT_PRODUCTION_DIRECTORY.parse(),
            "letsencrypt-staging" => async_acme::acme::LETS_ENCRYPT_STAGING_DIRECTORY.parse(),
            s => s.parse(),
        }
    }
}
impl ValueParserFactory for AcmeProvider {
    type Parser = FromStrParser<Self>;
    fn value_parser() -> Self::Parser {
        Self::Parser::new()
    }
}

#[derive(Deserialize, Serialize, Parser)]
pub struct InitAcmeParams {
    #[arg(long)]
    pub provider: AcmeProvider,
    #[arg(long)]
    pub contact: Vec<String>,
}

pub async fn init(
    ctx: RpcContext,
    InitAcmeParams {
        provider: AcmeProvider(provider),
        contact,
    }: InitAcmeParams,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            db.as_public_mut()
                .as_server_info_mut()
                .as_acme_mut()
                .map_mutate(|acme| {
                    Ok(Some(AcmeSettings {
                        provider,
                        contact,
                        domains: acme.map(|acme| acme.domains).unwrap_or_default(),
                    }))
                })
        })
        .await?;
    Ok(())
}

pub fn domain<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "add",
            from_fn_async(add_domain)
                .with_about("Add a domain for which to acquire ACME certificates")
                .no_display()
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "remove",
            from_fn_async(remove_domain)
                .with_about("Remove a domain for which to acquire ACME certificates")
                .no_display()
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "list",
            from_fn_async(list_domains)
                .with_about("List domains for which to acquire ACME certificates")
                .with_custom_display_fn(|_, res| {
                    for domain in res {
                        println!("{domain}")
                    }
                    Ok(())
                })
                .with_call_remote::<CliContext>(),
        )
}

#[derive(Deserialize, Serialize, Parser)]
pub struct DomainParams {
    pub domain: InternedString,
}

pub async fn add_domain(
    ctx: RpcContext,
    DomainParams { domain }: DomainParams,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            db.as_public_mut()
                .as_server_info_mut()
                .as_acme_mut()
                .transpose_mut()
                .ok_or_else(|| {
                    Error::new(
                        eyre!("Please call `start-cli net acme init` before adding a domain"),
                        ErrorKind::InvalidRequest,
                    )
                })?
                .as_domains_mut()
                .mutate(|domains| {
                    domains.insert(domain);
                    Ok(())
                })
        })
        .await?;
    Ok(())
}

pub async fn remove_domain(
    ctx: RpcContext,
    DomainParams { domain }: DomainParams,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            if let Some(acme) = db
                .as_public_mut()
                .as_server_info_mut()
                .as_acme_mut()
                .transpose_mut()
            {
                acme.as_domains_mut().mutate(|domains| {
                    domains.remove(&domain);
                    Ok(())
                })
            } else {
                Ok(())
            }
        })
        .await?;
    Ok(())
}

pub async fn list_domains(ctx: RpcContext) -> Result<BTreeSet<InternedString>, Error> {
    if let Some(acme) = ctx
        .db
        .peek()
        .await
        .into_public()
        .into_server_info()
        .into_acme()
        .transpose()
    {
        acme.into_domains().de()
    } else {
        Ok(BTreeSet::new())
    }
}
