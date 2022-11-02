use std::cmp::Ordering;
use std::fmt;
use std::net::IpAddr;
use std::str::FromStr;

use color_eyre::eyre::eyre;
use http::{Request, Uri};
use hyper::Body;

use crate::Error;

pub fn host_addr_fqdn(req: &Request<Body>) -> Result<Fqdn, Error> {
    let host = req.headers().get(http::header::HOST);

    match host {
        Some(host) => {
            let host_str = host
                .to_str()
                .map_err(|e| Error::new(eyre!("{}", e), crate::ErrorKind::AsciiError))?
                .to_string();

            let host_uri: Fqdn = host_str.parse()?;

            Ok(host_uri)
        }

        None => Err(Error::new(eyre!("No Host"), crate::ErrorKind::NoHost)),
    }
}

impl PartialOrd for Fqdn {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Fqdn {
    fn cmp(&self, other: &Self) -> Ordering {
        self.cmp(other)
    }
}

impl PartialEq for Fqdn {
    fn eq(&self, other: &Self) -> bool {
        // match self {
        //     Fqdn::IpAddr(ip_left) => match other {
        //         Fqdn::IpAddr(ip_right) => ip_left == ip_right,
        //         Fqdn::Uri { full_uri, root, tld } => {

        //         },
        //     },
        //     Fqdn::Uri { full_uri, root, tld } => todo!(),
        // }
        self == other
    }
}

#[derive(Eq, Debug, Clone)]
pub enum Fqdn {
    IpAddr(IpAddr),
    Uri {
        full_uri: Uri,
        root: String,
        tld: Tld,
    },
}

impl fmt::Display for Fqdn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Fqdn::IpAddr(ip) => {
                write!(f, "{}", ip)
            }
            Fqdn::Uri {
                full_uri,
                root,
                tld,
            } => {
                write!(f, "{}", full_uri)
            }
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

impl FromStr for Fqdn {
    type Err = Error;

    fn from_str(input: &str) -> Result<Fqdn, Self::Err> {

        dbg!(input);

        
        let full_fqdn = {
            if let Ok(ip) = input.parse::<IpAddr>() {
                Ok(Fqdn::IpAddr(ip))
            } else {
                let fqdn_url = {
                    let hostname_split: Vec<&str> = input.split('.').collect();

                    if hostname_split.len() != 2 {
                        return Err(Error::new(
                    eyre!("invalid url tld number: add support for tldextract to parse complex urls like blah.domain.co.uk and etc?"),
                    crate::ErrorKind::ParseUrl,
                ));
                    }

                    match hostname_split[1] {
                        ".local" => Ok(Fqdn::Uri {
                            full_uri: input.parse::<Uri>()?,
                            root: hostname_split[0].to_owned(),
                            tld: Tld::Local,
                        }),
                        ".embassy" => Ok(Fqdn::Uri {
                            full_uri: input.parse::<Uri>()?,
                            root: hostname_split[0].to_owned(),
                            tld: Tld::Embassy,
                        }),
                        ".onion" => Ok(Fqdn::Uri {
                            full_uri: input.parse::<Uri>()?,
                            root: hostname_split[0].to_owned(),
                            tld: Tld::Onion,
                        }),
                        _ => Err(Error::new(
                            eyre!("Unknown TLD for enum"),
                            crate::ErrorKind::ParseUrl,
                        )),
                    }
                };

                fqdn_url
            }
        };

        full_fqdn
    }
}

impl TryFrom<Uri> for Fqdn {
    type Error = Error;

    fn try_from(value: Uri) -> Result<Self, Self::Error> {
        Self::from_str(&value.to_string())
    }
}
