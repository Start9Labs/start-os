use std::net::{IpAddr, Ipv4Addr, Ipv6Addr, SocketAddr, SocketAddrV6};
use std::path::Path;

use async_stream::try_stream;
use color_eyre::eyre::eyre;
use futures::stream::BoxStream;
use futures::{StreamExt, TryStreamExt};
use imbl_value::InternedString;
use ipnet::{IpNet, Ipv4Net, Ipv6Net};
use nix::net::if_::if_nametoindex;
use tokio::net::{TcpListener, TcpStream};
use tokio::process::Command;

use crate::prelude::*;
use crate::util::Invoke;

pub fn ipv6_is_link_local(addr: Ipv6Addr) -> bool {
    (addr.segments()[0] & 0xffc0) == 0xfe80
}

pub fn ipv6_is_local(addr: Ipv6Addr) -> bool {
    addr.is_loopback() || (addr.segments()[0] & 0xfe00) == 0xfc00 || ipv6_is_link_local(addr)
}

fn parse_iface_ip(output: &str) -> Result<Vec<&str>, Error> {
    let output = output.trim();
    if output.is_empty() {
        return Ok(Vec::new());
    }
    let mut res = Vec::new();
    for line in output.lines() {
        if let Some(ip) = line.split_ascii_whitespace().nth(3) {
            res.push(ip)
        } else {
            return Err(Error::new(
                eyre!("malformed output from `ip`"),
                crate::ErrorKind::Network,
            ));
        }
    }
    Ok(res)
}

pub async fn get_iface_ipv4_addr(iface: &str) -> Result<Option<(Ipv4Addr, Ipv4Net)>, Error> {
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
    .into_iter()
    .map(|s| Ok::<_, Error>((s.split("/").next().unwrap().parse()?, s.parse()?)))
    .next()
    .transpose()?)
}

pub async fn get_iface_ipv6_addr(iface: &str) -> Result<Option<(Ipv6Addr, Ipv6Net)>, Error> {
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
    .into_iter()
    .find(|ip| !ip.starts_with("fe80::"))
    .map(|s| Ok::<_, Error>((s.split("/").next().unwrap().parse()?, s.parse()?)))
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

pub async fn all_socket_addrs_for(port: u16) -> Result<Vec<(InternedString, SocketAddr)>, Error> {
    let mut res = Vec::new();

    let raw = String::from_utf8(
        Command::new("ip")
            .arg("-o")
            .arg("addr")
            .arg("show")
            .invoke(ErrorKind::ParseSysInfo)
            .await?,
    )?;
    let err = |item: &str, lineno: usize, line: &str| {
        Error::new(
            eyre!("failed to parse ip info ({item}[line:{lineno}]) from {line:?}"),
            ErrorKind::ParseSysInfo,
        )
    };
    for (idx, line) in raw
        .lines()
        .map(|l| l.trim())
        .enumerate()
        .filter(|(_, l)| !l.is_empty())
    {
        let mut split = line.split_whitespace();
        let _num = split.next();
        let ifname = split.next().ok_or_else(|| err("ifname", idx, line))?;
        let _kind = split.next();
        let ipnet_str = split.next().ok_or_else(|| err("ipnet", idx, line))?;
        let ipnet = ipnet_str
            .parse::<IpNet>()
            .with_ctx(|_| (ErrorKind::ParseSysInfo, err("ipnet", idx, ipnet_str)))?;
        match ipnet.addr() {
            IpAddr::V4(ip4) => res.push((ifname.into(), SocketAddr::new(ip4.into(), port))),
            IpAddr::V6(ip6) => res.push((
                ifname.into(),
                SocketAddr::V6(SocketAddrV6::new(
                    ip6,
                    port,
                    0,
                    if_nametoindex(ifname)
                        .with_ctx(|_| (ErrorKind::ParseSysInfo, "reading scope_id"))?,
                )),
            )),
        }
    }

    Ok(res)
}

pub struct TcpListeners {
    listeners: Vec<TcpListener>,
}
impl TcpListeners {
    pub fn new(listeners: impl IntoIterator<Item = TcpListener>) -> Self {
        Self {
            listeners: listeners.into_iter().collect(),
        }
    }

    pub async fn accept(&self) -> std::io::Result<(TcpStream, SocketAddr)> {
        futures::future::select_all(self.listeners.iter().map(|l| Box::pin(l.accept())))
            .await
            .0
    }
}
// impl hyper::server::accept::Accept for TcpListeners {
//     type Conn = TcpStream;
//     type Error = std::io::Error;

//     fn poll_accept(
//         self: std::pin::Pin<&mut Self>,
//         cx: &mut std::task::Context<'_>,
//     ) -> std::task::Poll<Option<Result<Self::Conn, Self::Error>>> {
//         for listener in self.listeners.iter() {
//             let poll = listener.poll_accept(cx);
//             if poll.is_ready() {
//                 return poll.map(|a| a.map(|a| a.0)).map(Some);
//             }
//         }
//         std::task::Poll::Pending
//     }
// }
// TODO
