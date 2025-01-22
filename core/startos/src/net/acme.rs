use std::collections::{BTreeMap, BTreeSet};
use std::str::FromStr;

use async_acme::acme::Identifier;
use clap::builder::ValueParserFactory;
use clap::Parser;
use imbl_value::InternedString;
use itertools::Itertools;
use models::{ErrorData, FromStrParser};
use openssl::pkey::{PKey, Private};
use openssl::x509::X509;
use rpc_toolkit::{from_fn_async, Context, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use ts_rs::TS;
use url::Url;

use crate::context::{CliContext, RpcContext};
use crate::db::model::public::AcmeSettings;
use crate::db::model::Database;
use crate::prelude::*;
use crate::util::serde::{Pem, Pkcs8Doc};

#[derive(Debug, Default, Deserialize, Serialize, HasModel)]
#[model = "Model<Self>"]
pub struct AcmeCertStore {
    pub accounts: BTreeMap<JsonKey<Vec<String>>, Pem<Pkcs8Doc>>,
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
        Ok(Some(account.de()?.0.document.into_vec()))
    }

    async fn write_account(&self, contacts: &[&str], contents: &[u8]) -> Result<(), Self::Error> {
        let contacts = JsonKey::new(contacts.into_iter().map(|s| (*s).to_owned()).collect_vec());
        let key = Pkcs8Doc {
            tag: "EC PRIVATE KEY".into(),
            document: pkcs8::Document::try_from(contents).with_kind(ErrorKind::Pem)?,
        };
        self.0
            .mutate(|db| {
                db.as_private_mut()
                    .as_key_store_mut()
                    .as_acme_mut()
                    .as_accounts_mut()
                    .insert(&contacts, &Pem::new(key))
            })
            .await?;
        Ok(())
    }

    async fn read_certificate(
        &self,
        identifiers: &[Identifier],
        directory_url: &str,
    ) -> Result<Option<(String, String)>, Self::Error> {
        let identifiers = JsonKey::new(
            identifiers
                .into_iter()
                .map(|d| match d {
                    Identifier::Dns(d) => d.into(),
                    Identifier::Ip(ip) => InternedString::from_display(ip),
                })
                .collect(),
        );
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
            .and_then(|a| a.into_idx(&identifiers))
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
        identifiers: &[Identifier],
        directory_url: &str,
        key_pem: &str,
        certificate_pem: &str,
    ) -> Result<(), Self::Error> {
        tracing::info!("Saving new certificate for {identifiers:?}");
        let identifiers = JsonKey::new(
            identifiers
                .into_iter()
                .map(|d| match d {
                    Identifier::Dns(d) => d.into(),
                    Identifier::Ip(ip) => InternedString::from_display(ip),
                })
                .collect(),
        );
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
                    .insert(&identifiers, &cert)
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
                .with_metadata("sync_db", Value::Bool(true))
                .no_display()
                .with_about("Setup ACME certificate acquisition")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "remove",
            from_fn_async(remove)
                .with_metadata("sync_db", Value::Bool(true))
                .no_display()
                .with_about("Setup ACME certificate acquisition")
                .with_call_remote::<CliContext>(),
        )
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, TS)]
#[ts(type = "string")]
pub struct AcmeProvider(pub Url);
impl FromStr for AcmeProvider {
    type Err = <Url as FromStr>::Err;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "letsencrypt" => async_acme::acme::LETS_ENCRYPT_PRODUCTION_DIRECTORY.parse(),
            "letsencrypt-staging" => async_acme::acme::LETS_ENCRYPT_STAGING_DIRECTORY.parse(),
            s => s.parse(),
        }
        .map(|mut u: Url| {
            let path = u
                .path_segments()
                .into_iter()
                .flatten()
                .filter(|p| !p.is_empty())
                .map(|p| p.to_owned())
                .collect::<Vec<_>>();
            if let Ok(mut path_mut) = u.path_segments_mut() {
                path_mut.clear();
                path_mut.extend(path);
            }
            u
        })
        .map(Self)
    }
}
impl<'de> Deserialize<'de> for AcmeProvider {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        crate::util::serde::deserialize_from_str(deserializer)
    }
}
impl AsRef<str> for AcmeProvider {
    fn as_ref(&self) -> &str {
        self.0.as_str()
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
    InitAcmeParams { provider, contact }: InitAcmeParams,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            db.as_public_mut()
                .as_server_info_mut()
                .as_acme_mut()
                .insert(&provider, &AcmeSettings { contact })
        })
        .await?;
    Ok(())
}

#[derive(Deserialize, Serialize, Parser)]
pub struct RemoveAcmeParams {
    #[arg(long)]
    pub provider: AcmeProvider,
}

pub async fn remove(
    ctx: RpcContext,
    RemoveAcmeParams { provider }: RemoveAcmeParams,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            db.as_public_mut()
                .as_server_info_mut()
                .as_acme_mut()
                .remove(&provider)
        })
        .await?;
    Ok(())
}
