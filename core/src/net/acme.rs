use std::collections::{BTreeMap, BTreeSet};
use std::net::IpAddr;
use std::str::FromStr;
use std::sync::Arc;

use async_acme::acme::{ACME_TLS_ALPN_NAME, Identifier};
use clap::Parser;
use clap::builder::ValueParserFactory;
use futures::StreamExt;
use imbl_value::InternedString;
use itertools::Itertools;
use openssl::pkey::{PKey, Private};
use openssl::x509::X509;
use rpc_toolkit::{Context, HandlerExt, ParentHandler, from_fn_async};
use serde::{Deserialize, Serialize};
use tokio_rustls::rustls::ServerConfig;
use tokio_rustls::rustls::crypto::CryptoProvider;
use tokio_rustls::rustls::pki_types::{CertificateDer, PrivateKeyDer, PrivatePkcs8KeyDer};
use tokio_rustls::rustls::server::ClientHello;
use tokio_rustls::rustls::sign::CertifiedKey;
use ts_rs::TS;
use url::Url;

use crate::context::{CliContext, RpcContext};
use crate::db::model::Database;
use crate::db::model::public::AcmeSettings;
use crate::db::{DbAccess, DbAccessByKey, DbAccessMut};
use crate::error::ErrorData;
use crate::net::ssl::should_use_cert;
use crate::net::tls::{SingleCertResolver, TlsHandler};
use crate::net::web_server::Accept;
use crate::prelude::*;
use crate::util::FromStrParser;
use crate::util::serde::{Pem, Pkcs8Doc};
use crate::util::sync::{SyncMutex, Watch};

pub type AcmeTlsAlpnCache =
    Arc<SyncMutex<BTreeMap<InternedString, Watch<Option<Arc<CertifiedKey>>>>>>;

pub struct AcmeTlsHandler<M: HasModel, S> {
    pub db: TypedPatchDb<M>,
    pub acme_cache: AcmeTlsAlpnCache,
    pub crypto_provider: Arc<CryptoProvider>,
    pub get_provider: S,
    pub in_progress: Watch<BTreeSet<BTreeSet<InternedString>>>,
}
impl<M, S> AcmeTlsHandler<M, S>
where
    for<'a> M: DbAccessByKey<AcmeSettings, Key<'a> = &'a AcmeProvider>
        + DbAccessMut<AcmeCertStore>
        + HasModel<Model = Model<M>>
        + Send
        + Sync,
    S: GetAcmeProvider + Clone,
{
    pub async fn get_cert(&self, san_info: &BTreeSet<InternedString>) -> Option<CertifiedKey> {
        let provider = self.get_provider.get_provider(san_info).await?;
        let provider = provider.as_ref();
        loop {
            let peek = self.db.peek().await;
            let store = <M as DbAccess<AcmeCertStore>>::access(&peek);
            if let Some(cert) = store
                .as_certs()
                .as_idx(&provider.0)
                .and_then(|p| p.as_idx(JsonKey::new_ref(san_info)))
            {
                let cert = cert.de().log_err()?;
                if cert
                    .fullchain
                    .get(0)
                    .and_then(|c| should_use_cert(&c.0).log_err())
                    .unwrap_or(false)
                {
                    return Some(
                        CertifiedKey::from_der(
                            cert.fullchain
                                .into_iter()
                                .map(|c| Ok(CertificateDer::from(c.to_der()?)))
                                .collect::<Result<_, Error>>()
                                .log_err()?,
                            PrivateKeyDer::from(PrivatePkcs8KeyDer::from(
                                cert.key.0.private_key_to_pkcs8().log_err()?,
                            )),
                            &*self.crypto_provider,
                        )
                        .log_err()?,
                    );
                }
            }

            if !self.in_progress.send_if_modified(|x| {
                if !x.contains(san_info) {
                    x.insert(san_info.clone());
                    true
                } else {
                    false
                }
            }) {
                self.in_progress
                    .clone()
                    .wait_for(|x| !x.contains(san_info))
                    .await;
                continue;
            }

            let contact = <M as DbAccessByKey<AcmeSettings>>::access_by_key(&peek, &provider)?
                .as_contact()
                .de()
                .log_err()?;

            let identifiers: Vec<_> = san_info
                .iter()
                .map(|d| match d.parse::<IpAddr>() {
                    Ok(a) => Identifier::Ip(a),
                    _ => Identifier::Dns((&**d).into()),
                })
                .collect::<Vec<_>>();

            let cache_entries = san_info
                .iter()
                .cloned()
                .map(|d| (d, Watch::new(None)))
                .collect::<BTreeMap<_, _>>();
            self.acme_cache.mutate(|c| {
                c.extend(cache_entries.iter().map(|(k, v)| (k.clone(), v.clone())));
            });

            let cert = async_acme::rustls_helper::order(
                |identifier, cert| {
                    let domain = InternedString::from_display(&identifier);
                    if let Some(entry) = cache_entries.get(&domain) {
                        entry.send(Some(Arc::new(cert)));
                    }
                    Ok(())
                },
                provider.0.as_str(),
                &identifiers,
                Some(&AcmeCertCache(&self.db)),
                &contact,
            )
            .await
            .log_err()?;

            self.acme_cache
                .mutate(|c| c.retain(|c, _| !cache_entries.contains_key(c)));

            self.in_progress.send_modify(|i| i.remove(san_info));

            return Some(cert);
        }
    }
}

pub trait GetAcmeProvider {
    fn get_provider<'a, 'b: 'a>(
        &'b self,
        san_info: &'a BTreeSet<InternedString>,
    ) -> impl Future<Output = Option<impl AsRef<AcmeProvider> + Send + 'b>> + Send + 'a;
}

impl<'a, A, M, S> TlsHandler<'a, A> for Arc<AcmeTlsHandler<M, S>>
where
    A: Accept + 'a,
    <A as Accept>::Metadata: Send + Sync,
    for<'m> M: DbAccessByKey<AcmeSettings, Key<'m> = &'m AcmeProvider>
        + DbAccessMut<AcmeCertStore>
        + HasModel<Model = Model<M>>
        + Send
        + Sync,
    S: GetAcmeProvider + Clone + Send + Sync,
{
    async fn get_config(
        &'a mut self,
        hello: &'a ClientHello<'a>,
        _: &'a <A as Accept>::Metadata,
    ) -> Option<ServerConfig> {
        let domain = hello.server_name()?;
        if hello
            .alpn()
            .into_iter()
            .flatten()
            .any(|a| a == ACME_TLS_ALPN_NAME)
        {
            let cert = self
                .acme_cache
                .peek(|c| c.get(domain).cloned())
                .ok_or_else(|| {
                    Error::new(
                        eyre!("No challenge recv available for {domain}"),
                        ErrorKind::OpenSsl,
                    )
                })
                .log_err()?;
            tracing::info!("Waiting for verification cert for {domain}");
            let cert = cert
                .filter(|c| futures::future::ready(c.is_some()))
                .next()
                .await
                .flatten()?;
            tracing::info!("Verification cert received for {domain}");
            let mut cfg = ServerConfig::builder_with_provider(self.crypto_provider.clone())
                .with_safe_default_protocol_versions()
                .log_err()?
                .with_no_client_auth()
                .with_cert_resolver(Arc::new(SingleCertResolver(cert)));

            cfg.alpn_protocols = vec![ACME_TLS_ALPN_NAME.to_vec()];
            tracing::info!("performing ACME auth challenge");

            return Some(cfg);
        }

        let domains: BTreeSet<InternedString> = [domain.into()].into_iter().collect();

        let crypto_provider = self.crypto_provider.clone();
        if let Some(cert) = self.get_cert(&domains).await {
            return Some(
                ServerConfig::builder_with_provider(crypto_provider)
                    .with_safe_default_protocol_versions()
                    .log_err()?
                    .with_no_client_auth()
                    .with_cert_resolver(Arc::new(SingleCertResolver(Arc::new(cert)))),
            );
        }

        None
    }
}

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

impl DbAccess<AcmeCertStore> for Database {
    fn access<'a>(db: &'a Model<Self>) -> &'a Model<AcmeCertStore> {
        db.as_private().as_key_store().as_acme()
    }
}
impl DbAccessMut<AcmeCertStore> for Database {
    fn access_mut<'a>(db: &'a mut Model<Self>) -> &'a mut Model<AcmeCertStore> {
        db.as_private_mut().as_key_store_mut().as_acme_mut()
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct AcmeCert {
    pub key: Pem<PKey<Private>>,
    pub fullchain: Vec<Pem<X509>>,
}

pub struct AcmeCertCache<'a, M: HasModel>(pub &'a TypedPatchDb<M>);
#[async_trait::async_trait]
impl<'a, M> async_acme::cache::AcmeCache for AcmeCertCache<'a, M>
where
    M: HasModel<Model = Model<M>> + DbAccessMut<AcmeCertStore> + Send + Sync,
{
    type Error = ErrorData;

    async fn read_account(&self, contacts: &[&str]) -> Result<Option<Vec<u8>>, Self::Error> {
        let contacts = JsonKey::new(contacts.into_iter().map(|s| (*s).to_owned()).collect_vec());
        let peek = self.0.peek().await;
        let Some(account) = M::access(&peek).as_accounts().as_idx(&contacts) else {
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
                M::access_mut(db)
                    .as_accounts_mut()
                    .insert(&contacts, &Pem::new(key))
            })
            .await
            .result?;
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
        let peek = self.0.peek().await;
        let Some(cert) = M::access(&peek)
            .as_certs()
            .as_idx(&directory_url)
            .and_then(|a| a.as_idx(&identifiers))
        else {
            return Ok(None);
        };
        let cert = cert.de()?;
        if !cert
            .fullchain
            .get(0)
            .map(|c| should_use_cert(&c.0))
            .transpose()
            .map_err(Error::from)?
            .unwrap_or(false)
        {
            return Ok(None);
        }
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
                M::access_mut(db)
                    .as_certs_mut()
                    .upsert(&directory_url, || Ok(BTreeMap::new()))?
                    .insert(&identifiers, &cert)
            })
            .await
            .result?;

        Ok(())
    }
}

pub fn acme_api<C: Context>() -> ParentHandler<C> {
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
                .with_about("Remove ACME certificate acquisition configuration")
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
impl AsRef<AcmeProvider> for AcmeProvider {
    fn as_ref(&self) -> &AcmeProvider {
        self
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
                .as_network_mut()
                .as_acme_mut()
                .insert(&provider, &AcmeSettings { contact })
        })
        .await
        .result?;
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
                .as_network_mut()
                .as_acme_mut()
                .remove(&provider)
        })
        .await
        .result?;
    Ok(())
}
