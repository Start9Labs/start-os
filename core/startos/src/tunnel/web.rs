use std::collections::VecDeque;
use std::io::Write;
use std::net::{IpAddr, Ipv6Addr, SocketAddr};
use std::sync::Arc;

use clap::Parser;
use imbl_value::{InternedString, json};
use itertools::Itertools;
use openssl::pkey::{PKey, Private};
use openssl::x509::X509;
use rpc_toolkit::{Context, Empty, HandlerArgs, HandlerExt, ParentHandler, from_fn_async};
use serde::{Deserialize, Serialize};
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio_rustls::rustls::ServerConfig;
use tokio_rustls::rustls::crypto::CryptoProvider;
use tokio_rustls::rustls::pki_types::{CertificateDer, PrivateKeyDer, PrivatePkcs8KeyDer};
use tokio_rustls::rustls::server::ClientHello;

use crate::context::CliContext;
use crate::net::ssl::SANInfo;
use crate::net::tls::TlsHandler;
use crate::net::web_server::Accept;
use crate::prelude::*;
use crate::tunnel::context::TunnelContext;
use crate::tunnel::db::TunnelDatabase;
use crate::util::serde::{HandlerExtSerde, Pem};

#[derive(Debug, Default, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
pub struct WebserverInfo {
    pub enabled: bool,
    pub listen: Option<SocketAddr>,
    pub certificate: Option<TunnelCertData>,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct TunnelCertData {
    pub key: Pem<PKey<Private>>,
    pub cert: Pem<Vec<X509>>,
}

#[derive(Clone)]
pub struct TunnelCertHandler {
    pub db: TypedPatchDb<TunnelDatabase>,
    pub crypto_provider: Arc<CryptoProvider>,
}
impl<'a, A> TlsHandler<'a, A> for TunnelCertHandler
where
    A: Accept + 'a,
    <A as Accept>::Metadata: Send + Sync,
{
    async fn get_config(
        &'a mut self,
        _: &'a ClientHello<'a>,
        _: &'a <A as Accept>::Metadata,
    ) -> Option<ServerConfig> {
        let cert_info = self
            .db
            .peek()
            .await
            .as_webserver()
            .as_certificate()
            .de()
            .log_err()??;
        let cert_chain: Vec<_> = cert_info
            .cert
            .0
            .iter()
            .map(|c| Ok::<_, Error>(CertificateDer::from(c.to_der()?)))
            .collect::<Result<_, _>>()
            .log_err()?;
        let cert_key = cert_info.key.0.private_key_to_pkcs8().log_err()?;

        Some(
            ServerConfig::builder_with_provider(self.crypto_provider.clone())
                .with_safe_default_protocol_versions()
                .log_err()?
                .with_no_client_auth()
                .with_single_cert(
                    cert_chain,
                    PrivateKeyDer::Pkcs8(PrivatePkcs8KeyDer::from(cert_key)),
                )
                .log_err()?,
        )
    }
}

pub fn web_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "init",
            from_fn_async(init_web)
                .no_display()
                .with_about("Initialize the webserver"),
        )
        .subcommand(
            "set-listen",
            from_fn_async(set_listen)
                .no_display()
                .with_about("Set the listen address for the webserver")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "get-listen",
            from_fn_async(get_listen)
                .with_display_serializable()
                .with_about("Get the listen address for the webserver")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "get-available-ips",
            from_fn_async(get_available_ips)
                .with_display_serializable()
                .with_about("Get available IP addresses to bind to")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "import-certificate",
            from_fn_async(import_certificate_rpc).no_cli(),
        )
        .subcommand(
            "import-certificate",
            from_fn_async(import_certificate_cli)
                .no_display()
                .with_about("Import a certificate to use for the webserver"),
        )
        .subcommand(
            "generate-certificate",
            from_fn_async(generate_certificate)
                .with_about("Generate a self signed certificaet to use for the webserver")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "enable",
            from_fn_async(enable_web)
                .with_about("Enable the webserver")
                .no_display()
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "disable",
            from_fn_async(disable_web)
                .no_display()
                .with_about("Disable the webserver")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "reset",
            from_fn_async(reset_web)
                .no_display()
                .with_about("Reset the webserver")
                .with_call_remote::<CliContext>(),
        )
}

pub async fn import_certificate_rpc(
    ctx: TunnelContext,
    cert_data: TunnelCertData,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            db.as_webserver_mut()
                .as_certificate_mut()
                .ser(&Some(cert_data))
        })
        .await
        .result?;
    Ok(())
}

pub async fn import_certificate_cli(
    HandlerArgs {
        context,
        parent_method,
        method,
        ..
    }: HandlerArgs<CliContext>,
) -> Result<(), Error> {
    println!("Please paste in your PEM encoded private key: ");
    let mut stdin_lines = BufReader::new(tokio::io::stdin()).lines();
    let mut key_string = String::new();

    while let Some(line) = stdin_lines.next_line().await? {
        key_string.push_str(&line);
        key_string.push_str("\n");
        if line.trim().starts_with("-----END") {
            break;
        }
    }

    let key: Pem<PKey<Private>> = key_string.parse()?;

    println!("Please paste in your PEM encoded certificate (or certificate chain): ");

    let mut chain = Vec::<X509>::new();

    loop {
        let mut cert_string = String::new();

        while let Some(line) = stdin_lines.next_line().await? {
            cert_string.push_str(&line);
            cert_string.push_str("\n");
            if line.trim().starts_with("-----END") {
                break;
            }
        }

        let cert: Pem<X509> = cert_string.parse()?;

        let key = cert.0.public_key()?;

        if let Some(prev) = chain.last() {
            if !prev.verify(&key)? {
                return Err(Error::new(
                    eyre!(
                        "Invalid Fullchain: Previous cert was not signed by this certificate's key"
                    ),
                    ErrorKind::InvalidSignature,
                ));
            }
        }

        let is_root = cert.0.verify(&key)?;

        chain.push(cert.0);

        if is_root {
            break;
        }
    }

    context
        .call_remote::<TunnelContext>(
            &parent_method.iter().chain(method.iter()).join("."),
            to_value(&TunnelCertData {
                key,
                cert: Pem(chain),
            })?,
        )
        .await?;

    Ok(())
}

#[derive(Debug, Deserialize, Serialize, Parser)]
pub struct GenerateCertParams {
    #[arg(help = "Subject Alternative Name(s)")]
    pub subject: Vec<InternedString>,
}

pub async fn generate_certificate(
    ctx: TunnelContext,
    GenerateCertParams { subject }: GenerateCertParams,
) -> Result<Pem<X509>, Error> {
    let saninfo = SANInfo::new(&subject.into_iter().collect());

    let key = crate::net::ssl::generate_key()?;
    let cert = crate::net::ssl::make_self_signed((&key, &saninfo))?;

    ctx.db
        .mutate(|db| {
            db.as_webserver_mut()
                .as_certificate_mut()
                .ser(&Some(TunnelCertData {
                    key: Pem(key),
                    cert: Pem(vec![cert.clone()]),
                }))
        })
        .await
        .result?;

    Ok(Pem(cert))
}

#[derive(Debug, Deserialize, Serialize, Parser)]
#[serde(rename_all = "camelCase")]
pub struct SetListenParams {
    pub listen: SocketAddr,
}

pub async fn set_listen(
    ctx: TunnelContext,
    SetListenParams { listen }: SetListenParams,
) -> Result<(), Error> {
    // Validate that the address is available to bind
    tokio::net::TcpListener::bind(listen)
        .await
        .with_kind(ErrorKind::Network)
        .with_ctx(|_| {
            (
                ErrorKind::Network,
                format!("{} is not available to bind to", listen),
            )
        })?;

    ctx.db
        .mutate(|db| {
            db.as_webserver_mut().as_listen_mut().ser(&Some(listen))?;

            Ok(())
        })
        .await
        .result
}

pub async fn get_listen(ctx: TunnelContext) -> Result<Option<SocketAddr>, Error> {
    ctx.db.peek().await.as_webserver().as_listen().de()
}

pub async fn get_available_ips(ctx: TunnelContext) -> Result<Vec<IpAddr>, Error> {
    let ips = ctx.net_iface.peek(|interfaces| {
        interfaces
            .values()
            .filter_map(|info| {
                info.ip_info
                    .as_ref()
                    .and_then(|ip_info| ip_info.subnets.iter().next().map(|subnet| subnet.addr()))
            })
            .collect::<Vec<IpAddr>>()
    });

    Ok(ips)
}

pub async fn enable_web(ctx: TunnelContext) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            if db.as_webserver().as_listen().transpose_ref().is_none() {
                return Err(Error::new(
                    eyre!("Listen is not set"),
                    ErrorKind::ParseNetAddress,
                ));
            }
            if db.as_webserver().as_certificate().transpose_ref().is_none() {
                return Err(Error::new(
                    eyre!("Certificate is not set"),
                    ErrorKind::OpenSsl,
                ));
            }
            if db.as_password().transpose_ref().is_none() {
                return Err(Error::new(
                    eyre!("Password is not set"),
                    ErrorKind::Authorization,
                ));
            };
            db.as_webserver_mut().as_enabled_mut().ser(&true)
        })
        .await
        .result
}

pub async fn disable_web(ctx: TunnelContext) -> Result<(), Error> {
    ctx.db
        .mutate(|db| db.as_webserver_mut().as_enabled_mut().ser(&false))
        .await
        .result
}

pub async fn reset_web(ctx: TunnelContext) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            db.as_webserver_mut().as_enabled_mut().ser(&false)?;
            db.as_webserver_mut().as_listen_mut().ser(&None)?;
            db.as_webserver_mut().as_certificate_mut().ser(&None)?;
            db.as_password_mut().ser(&None)?;
            Ok(())
        })
        .await
        .result
}

pub async fn init_web(ctx: CliContext) -> Result<(), Error> {
    loop {
        match ctx
            .call_remote::<TunnelContext>("web.enable", json!({}))
            .await
        {
            Ok(_) => println!("Webserver Initialized"),
            Err(e) if e.code == ErrorKind::ParseNetAddress as i32 => {
                println!("A listen address has not been set yet. Setting one up now...");

                let available_ips = from_value::<Vec<IpAddr>>(
                    ctx.call_remote::<TunnelContext>("web.get-available-ips", json!({}))
                        .await?,
                )?;

                let suggested_addr = available_ips
                    .into_iter()
                    .find(|ip| match ip {
                        IpAddr::V4(ipv4) => !ipv4.is_private() && !ipv4.is_loopback(),
                        IpAddr::V6(ipv6) => {
                            !ipv6.is_loopback()
                                && !ipv6.is_unique_local()
                                && !ipv6.is_unicast_link_local()
                        }
                    })
                    .map(|ip| SocketAddr::new(ip, 8443))
                    .unwrap_or_else(|| SocketAddr::from((Ipv6Addr::UNSPECIFIED, 8443)));

                let (mut readline, _writer) = rustyline_async::Readline::new(format!(
                    "Listen Address [{}]: ",
                    suggested_addr
                ))
                .with_kind(ErrorKind::Filesystem)?;

                let listen: SocketAddr = loop {
                    match readline.readline().await.with_kind(ErrorKind::Filesystem)? {
                        rustyline_async::ReadlineEvent::Line(l) if !l.trim().is_empty() => {
                            match l.trim().parse() {
                                Ok(addr) => break addr,
                                Err(_) => {
                                    println!(
                                        "Invalid socket address. Please enter in format IP:PORT (e.g., 0.0.0.0:8443)"
                                    );
                                    readline.clear_history();
                                }
                            }
                        }
                        rustyline_async::ReadlineEvent::Line(_) => {
                            break suggested_addr;
                        }
                        _ => return Err(Error::new(eyre!("Aborted"), ErrorKind::Unknown)),
                    }
                };

                ctx.call_remote::<TunnelContext>(
                    "web.set-listen",
                    to_value(&SetListenParams { listen })?,
                )
                .await?;
            }
            Err(e) if e.code == ErrorKind::OpenSsl as i32 => {
                println!(
                    "StartTunnel has not been set up with an SSL Certificate yet. Setting one up now..."
                );
                println!("[1]: Generate a Self Signed Certificate");
                println!("[2]: Provide your own certificate and key");
                let (mut readline, mut writer) =
                    rustyline_async::Readline::new("What would you like to do? [1-2]: ".into())
                        .with_kind(ErrorKind::Filesystem)?;
                readline.add_history_entry("1".into());
                readline.add_history_entry("2".into());
                let self_signed;
                loop {
                    match readline.readline().await.with_kind(ErrorKind::Filesystem)? {
                        rustyline_async::ReadlineEvent::Line(l)
                            if l.trim_matches(|c: char| c.is_whitespace() || c == '"') == "1" =>
                        {
                            self_signed = true;
                            break;
                        }
                        rustyline_async::ReadlineEvent::Line(l)
                            if l.trim_matches(|c: char| c.is_whitespace() || c == '"') == "2" =>
                        {
                            self_signed = false;
                            break;
                        }
                        rustyline_async::ReadlineEvent::Line(_) => {
                            readline.clear_history();
                            readline.add_history_entry("1".into());
                            readline.add_history_entry("2".into());
                            writeln!(writer, "Invalid response. Enter either \"1\" or \"2\".")?;
                        }
                        _ => return Err(Error::new(eyre!("Aborted"), ErrorKind::Unknown)),
                    }
                }
                if self_signed {
                    let listen = from_value::<Option<SocketAddr>>(
                        ctx.call_remote::<TunnelContext>("web.get-listen", json!({}))
                            .await?,
                    )?
                    .filter(|a| !a.ip().is_unspecified());
                    writeln!(
                        writer,
                        "Enter the name(s) to sign the certificate for, separated by commas."
                    )?;
                    readline.clear_history();
                    let default_prompt = if let Some(listen) = listen {
                        format!("Subject Alternative Name(s) [{}]: ", listen.ip())
                    } else {
                        "Subject Alternative Name(s): ".to_string()
                    };
                    readline
                        .update_prompt(&default_prompt)
                        .with_kind(ErrorKind::Filesystem)?;
                    let mut saninfo = Vec::new();
                    loop {
                        match readline.readline().await.with_kind(ErrorKind::Filesystem)? {
                            rustyline_async::ReadlineEvent::Line(l) if !l.trim().is_empty() => {
                                saninfo.extend(l.split(",").map(|h| h.trim().into()));
                                break;
                            }
                            rustyline_async::ReadlineEvent::Line(_) => {
                                saninfo.extend(
                                    listen
                                        .map(|l| l.ip())
                                        .as_ref()
                                        .map(InternedString::from_display),
                                );
                                readline.clear_history();
                                if !saninfo.is_empty() {
                                    break;
                                }
                            }
                            _ => return Err(Error::new(eyre!("Aborted"), ErrorKind::Unknown)),
                        }
                    }

                    ctx.call_remote::<TunnelContext>(
                        "web.generate-certificate",
                        to_value(&GenerateCertParams { subject: saninfo })?,
                    )
                    .await?;
                } else {
                    drop((readline, writer));
                    import_certificate_cli(HandlerArgs {
                        context: ctx.clone(),
                        parent_method: vec!["web", "import-certificate"].into(),
                        method: VecDeque::new(),
                        params: Empty {},
                        inherited_params: Empty {},
                        raw_params: json!({}),
                    })
                    .await?;
                }
            }
            Err(e) if e.code == ErrorKind::Authorization as i32 => {
                println!("A password has not been setup yet. Setting one up now...");

                super::auth::set_password_cli(HandlerArgs {
                    context: ctx.clone(),
                    parent_method: vec!["auth", "set-password"].into(),
                    method: VecDeque::new(),
                    params: Empty {},
                    inherited_params: Empty {},
                    raw_params: json!({}),
                })
                .await?;
            }
            Err(e) => return Err(e.into()),
        }
    }
}
