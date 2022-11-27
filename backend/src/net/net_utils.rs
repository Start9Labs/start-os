use std::fmt;
use std::net::IpAddr;
use std::str::FromStr;

use color_eyre::eyre::eyre;
use http::{Request, Uri};
use hyper::Body;

use crate::Error;

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
