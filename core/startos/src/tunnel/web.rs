use std::collections::{BTreeSet, VecDeque};
use std::io::Write;
use std::net::{Ipv4Addr, Ipv6Addr, SocketAddr};

use clap::Parser;
use imbl_value::{json, InternedString};
use itertools::Itertools;
use openssl::pkey::{PKey, Private};
use openssl::x509::X509;
use rpc_toolkit::{from_fn_async, Context, Empty, HandlerArgs, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use tokio::io::{AsyncBufReadExt, BufReader};

use crate::context::CliContext;
use crate::net::ssl::SANInfo;
use crate::prelude::*;
use crate::tunnel::context::TunnelContext;
use crate::util::serde::Pem;

#[derive(Debug, Deserialize, Serialize, Parser)]
#[serde(rename_all = "camelCase")]
pub struct TunnelCertData {
    #[arg(long)]
    pub key: Pem<PKey<Private>>,
    #[arg(long)]
    pub cert: Pem<Vec<X509>>,
}

pub fn web_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("init", from_fn_async(init_web_rpc).no_cli())
        .subcommand(
            "init",
            from_fn_async(init_web_cli)
                .with_about("Initialize the webserver")
                .no_display(),
        )
        .subcommand(
            "import-certificate",
            from_fn_async(import_certificate)
                .with_about("Import a certificate to use for the webserver")
                .no_display()
                .with_call_remote::<CliContext>(),
        )
        // .subcommand(
        //     "forget-certificate",
        //     from_fn_async(forget_certificate)
        //         .with_about("Forget a certificate that was imported into the webserver")
        //         .no_display()
        //         .with_call_remote::<CliContext>(),
        // )
        .subcommand(
            "uninit",
            from_fn_async(uninit_web)
                .with_about("Disable the webserver")
                .no_display()
                .with_call_remote::<CliContext>(),
        )
}

pub async fn import_certificate(
    ctx: TunnelContext,
    cert_data: TunnelCertData,
) -> Result<(), Error> {
    let mut saninfo = BTreeSet::new();
    let leaf = cert_data.cert.get(0).ok_or_else(|| {
        Error::new(
            eyre!("certificate chain is empty"),
            ErrorKind::InvalidRequest,
        )
    })?;
    for san in leaf.subject_alt_names().into_iter().flatten() {
        if let Some(dns) = san.dnsname() {
            saninfo.insert(dns.into());
        }
        if let Some(ip) = san.ipaddress() {
            if let Ok::<[u8; 4], _>(ip) = ip.try_into() {
                saninfo.insert(InternedString::from_display(&Ipv4Addr::from_bits(
                    u32::from_be_bytes(ip),
                )));
            } else if let Ok::<[u8; 16], _>(ip) = ip.try_into() {
                saninfo.insert(InternedString::from_display(&Ipv6Addr::from_bits(
                    u128::from_be_bytes(ip),
                )));
            }
        }
    }
    ctx.db
        .mutate(|db| {
            db.as_certificates_mut()
                .insert(&JsonKey(saninfo), &cert_data)
        })
        .await
        .result?;
    Ok(())
}

#[derive(Debug, Deserialize, Serialize, Parser)]
pub struct InitWebParams {
    listen: SocketAddr,
}

pub async fn init_web_rpc(
    ctx: TunnelContext,
    InitWebParams { listen }: InitWebParams,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            if db.as_certificates().de()?.is_empty() {
                return Err(Error::new(
                    eyre!("No certificate available"),
                    ErrorKind::OpenSsl,
                ));
            }
            if db.as_password().transpose_ref().is_none() {
                return Err(Error::new(
                    eyre!("Password not set"),
                    ErrorKind::Authorization,
                ));
            }

            db.as_webserver_mut().ser(&Some(listen))?;

            Ok(())
        })
        .await
        .result
}

pub async fn uninit_web(ctx: TunnelContext) -> Result<(), Error> {
    ctx.db
        .mutate(|db| db.as_webserver_mut().ser(&None))
        .await
        .result
}

pub async fn init_web_cli(
    HandlerArgs {
        context,
        parent_method,
        method,
        params: InitWebParams { listen },
        ..
    }: HandlerArgs<CliContext, InitWebParams>,
) -> Result<(), Error> {
    loop {
        match context
            .call_remote::<TunnelContext>(
                &parent_method.iter().chain(method.iter()).join("."),
                to_value(&InitWebParams { listen })?,
            )
            .await
        {
            Ok(_) => println!("Webserver Initialized"),
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
                    writeln!(
                        writer,
                        "Enter the name(s) to sign the certificate for, separated by commas."
                    )?;
                    readline.clear_history();
                    readline
                        .update_prompt(&format!("Subject Alternative Name(s) [{}]: ", listen.ip()))
                        .with_kind(ErrorKind::Filesystem)?;
                    let mut saninfo = BTreeSet::new();
                    loop {
                        match readline.readline().await.with_kind(ErrorKind::Filesystem)? {
                            rustyline_async::ReadlineEvent::Line(l) if !l.trim().is_empty() => {
                                saninfo.extend(l.split(",").map(|h| h.trim().into()));
                                break;
                            }
                            rustyline_async::ReadlineEvent::Line(_) => {
                                readline.clear_history();
                            }
                            _ => return Err(Error::new(eyre!("Aborted"), ErrorKind::Unknown)),
                        }
                    }
                    let key = crate::net::ssl::gen_nistp256()?;
                    let cert = crate::net::ssl::make_self_signed((&key, &SANInfo::new(&saninfo)))?;

                    context
                        .call_remote::<TunnelContext>(
                            "web.import-certificate",
                            to_value(&TunnelCertData {
                                key: Pem(key),
                                cert: Pem(vec![cert]),
                            })?,
                        )
                        .await?;
                } else {
                    drop((readline, writer));
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

                    println!(
                        "Please paste in your PEM encoded certificate (or certificate chain): "
                    );

                    let mut chain = Vec::new();

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

                        let is_root = cert.0.authority_key_id().is_none();

                        chain.push(cert.0);

                        if is_root {
                            break;
                        }
                    }

                    context
                        .call_remote::<TunnelContext>(
                            "web.import-certificate",
                            to_value(&TunnelCertData {
                                key,
                                cert: Pem(chain),
                            })?,
                        )
                        .await?;
                }
            }
            Err(e) if e.code == ErrorKind::Authorization as i32 => {
                println!("A password has not been setup yet. Setting one up now...");

                super::auth::set_password_cli(HandlerArgs {
                    context: context.clone(),
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
