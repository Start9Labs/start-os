use std::fmt;
use std::net::{IpAddr, Ipv4Addr, Ipv6Addr};
use std::path::Path;
use std::str::FromStr;

use async_stream::try_stream;
use color_eyre::eyre::eyre;
use futures::stream::BoxStream;
use futures::{StreamExt, TryStreamExt};
use http::{Request, Uri};
use hyper::Body;
use tokio::process::Command;

use crate::util::Invoke;
use crate::Error;

fn parse_iface_ip(output: &str) -> Result<Option<&str>, Error> {
    let output = output.trim();
    if output.is_empty() {
        return Ok(None);
    }
    if let Some(ip) = output
        .split_ascii_whitespace()
        .nth(4)
        .and_then(|range| range.split("/").next())
    {
        Ok(Some(ip))
    } else {
        Err(Error::new(
            eyre!("malformed output from `ip`"),
            crate::ErrorKind::Network,
        ))
    }
}

pub async fn get_iface_ipv4_addr(iface: &str) -> Result<Option<Ipv4Addr>, Error> {
    Ok(parse_iface_ip(&String::from_utf8(
        Command::new("ip")
            .arg("-4")
            .arg("-o")
            .arg("addr")
            .arg("show")
            .arg(iface)
            .invoke(crate::ErrorKind::Network)
            .await?,
    )?)?
    .map(|s| s.parse())
    .transpose()?)
}

pub async fn get_iface_ipv6_addr(iface: &str) -> Result<Option<Ipv6Addr>, Error> {
    Ok(parse_iface_ip(&String::from_utf8(
        Command::new("ip")
            .arg("-6")
            .arg("-o")
            .arg("addr")
            .arg("show")
            .arg(iface)
            .invoke(crate::ErrorKind::Network)
            .await?,
    )?)?
    .map(|s| s.parse())
    .transpose()?)
}

pub async fn iface_is_physical(iface: &str) -> bool {
    tokio::fs::metadata(Path::new("/sys/class/net").join(iface).join("device"))
        .await
        .is_ok()
}

pub async fn iface_is_wireless(iface: &str) -> bool {
    tokio::fs::metadata(Path::new("/sys/class/net").join(iface).join("wireless"))
        .await
        .is_ok()
}

pub fn list_interfaces() -> BoxStream<'static, Result<String, Error>> {
    try_stream! {
        let mut ifaces = tokio::fs::read_dir("/sys/class/net").await?;
        while let Some(iface) = ifaces.next_entry().await? {
            if let Some(iface) = iface.file_name().into_string().ok() {
                yield iface;
            }
        }
    }
    .boxed()
}

pub async fn find_wifi_iface() -> Result<Option<String>, Error> {
    let mut ifaces = list_interfaces();
    while let Some(iface) = ifaces.try_next().await? {
        if iface_is_wireless(&iface).await {
            return Ok(Some(iface));
        }
    }
    Ok(None)
}

pub async fn find_eth_iface() -> Result<String, Error> {
    let mut ifaces = list_interfaces();
    while let Some(iface) = ifaces.try_next().await? {
        if iface_is_physical(&iface).await && !iface_is_wireless(&iface).await {
            return Ok(iface);
        }
    }
    Err(Error::new(
        eyre!("Could not detect ethernet interface"),
        crate::ErrorKind::Network,
    ))
}

pub fn host_addr_fqdn(req: &Request<Body>) -> Result<ResourceFqdn, Error> {
    let host = req.headers().get(http::header::HOST);

    match host {
        Some(host) => {
            let host_str = host
                .to_str()
                .map_err(|e| Error::new(eyre!("{}", e), crate::ErrorKind::Ascii))?
                .to_string();

            let host_uri: ResourceFqdn = host_str.split(':').next().unwrap().parse()?;

            Ok(host_uri)
        }

        None => Err(Error::new(
            eyre!("No Host header"),
            crate::ErrorKind::MissingHeader,
        )),
    }
}

#[derive(Eq, PartialEq, PartialOrd, Ord, Debug, Clone)]
pub enum ResourceFqdn {
    IpAddr,
    Uri {
        full_uri: String,
        root: String,
        tld: Tld,
    },
    LocalHost,
}

impl fmt::Display for ResourceFqdn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ResourceFqdn::Uri {
                full_uri,
                root: _,
                tld: _,
            } => {
                write!(f, "{}", full_uri)
            }
            ResourceFqdn::LocalHost => write!(f, "localhost"),
            ResourceFqdn::IpAddr => write!(f, "ip-address"),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
pub enum Tld {
    Local,
    Onion,
    Embassy,
}

impl fmt::Display for Tld {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Tld::Local => write!(f, ".local"),
            Tld::Onion => write!(f, ".onion"),
            Tld::Embassy => write!(f, ".embassy"),
        }
    }
}

impl FromStr for ResourceFqdn {
    type Err = Error;

    fn from_str(input: &str) -> Result<ResourceFqdn, Self::Err> {
        if input.parse::<IpAddr>().is_ok() {
            return Ok(ResourceFqdn::IpAddr);
        }

        if input == "localhost" {
            return Ok(ResourceFqdn::LocalHost);
        }

        let hostname_split: Vec<&str> = input.split('.').collect();

        if hostname_split.len() != 2 {
            return Err(Error::new(
                eyre!("invalid url tld number: add support for tldextract to parse complex urls like blah.domain.co.uk and etc?"),
                crate::ErrorKind::ParseUrl,
            ));
        }

        match hostname_split[1] {
            "local" => Ok(ResourceFqdn::Uri {
                full_uri: input.to_owned(),
                root: hostname_split[0].to_owned(),
                tld: Tld::Local,
            }),
            "embassy" => Ok(ResourceFqdn::Uri {
                full_uri: input.to_owned(),
                root: hostname_split[0].to_owned(),
                tld: Tld::Embassy,
            }),
            "onion" => Ok(ResourceFqdn::Uri {
                full_uri: input.to_owned(),
                root: hostname_split[0].to_owned(),
                tld: Tld::Onion,
            }),
            _ => Err(Error::new(
                eyre!("Unknown TLD for enum"),
                crate::ErrorKind::ParseUrl,
            )),
        }
    }
}

impl TryFrom<Uri> for ResourceFqdn {
    type Error = Error;

    fn try_from(value: Uri) -> Result<Self, Self::Error> {
        Self::from_str(&value.to_string())
    }
}

pub fn is_upgrade_req(req: &Request<Body>) -> bool {
    req.headers()
        .get("connection")
        .and_then(|c| c.to_str().ok())
        .map(|c| {
            c.split(",")
                .any(|c| c.trim().eq_ignore_ascii_case("upgrade"))
        })
        .unwrap_or(false)
}
