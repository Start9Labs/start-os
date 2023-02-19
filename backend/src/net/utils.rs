use std::convert::Infallible;
use std::net::{Ipv4Addr, Ipv6Addr};
use std::path::Path;

use async_stream::try_stream;
use color_eyre::eyre::eyre;
use futures::stream::BoxStream;
use futures::{StreamExt, TryStreamExt};
use ipnet::{Ipv4Net, Ipv6Net};
use tokio::process::Command;

use crate::util::Invoke;
use crate::Error;

fn parse_iface_ip(output: &str) -> Result<Option<&str>, Error> {
    let output = output.trim();
    if output.is_empty() {
        return Ok(None);
    }
    if let Some(ip) = output.split_ascii_whitespace().nth(3) {
        Ok(Some(ip))
    } else {
        Err(Error::new(
            eyre!("malformed output from `ip`"),
            crate::ErrorKind::Network,
        ))
    }
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
    .map(|s| Ok::<_, Error>((s.split("/").next().unwrap().parse()?, s.parse()?)))
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

#[pin_project::pin_project]
pub struct SingleAccept<T>(Option<T>);
impl<T> SingleAccept<T> {
    pub fn new(conn: T) -> Self {
        Self(Some(conn))
    }
}
impl<T> hyper::server::accept::Accept for SingleAccept<T> {
    type Conn = T;
    type Error = Infallible;
    fn poll_accept(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Result<Self::Conn, Self::Error>>> {
        std::task::Poll::Ready(self.project().0.map(Ok))
    }
}
