use std::collections::BTreeMap;
use std::net::{Ipv4Addr, SocketAddrV4};

use ed25519_dalek::{SigningKey, VerifyingKey};
use imbl_value::InternedString;
use ipnet::Ipv4Net;
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use tokio::process::Command;

use crate::prelude::*;
use crate::util::Invoke;
use crate::util::io::write_file_atomic;
use crate::util::serde::Base64;

#[derive(Deserialize, Serialize, HasModel)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
pub struct WgServer {
    pub port: u16,
    pub key: Base64<WgKey>,
    pub subnets: WgSubnetMap,
}
impl Default for WgServer {
    fn default() -> Self {
        Self {
            port: 51820,
            key: Base64(WgKey::generate()),
            subnets: WgSubnetMap::default(),
        }
    }
}
impl WgServer {
    pub fn server_config<'a>(&'a self) -> ServerConfig<'a> {
        ServerConfig(self)
    }
    pub async fn sync(&self) -> Result<(), Error> {
        Command::new("wg-quick")
            .arg("down")
            .arg("wg0")
            .invoke(ErrorKind::Network)
            .await
            .or_else(|e| {
                let msg = e.source.to_string();
                if msg.contains("does not exist") || msg.contains("is not a WireGuard interface") {
                    Ok(Vec::new())
                } else {
                    Err(e)
                }
            })?;
        write_file_atomic(
            "/etc/wireguard/wg0.conf",
            self.server_config().to_string().as_bytes(),
        )
        .await?;
        Command::new("wg-quick")
            .arg("up")
            .arg("wg0")
            .invoke(ErrorKind::Network)
            .await?;
        Ok(())
    }
}

#[derive(Default, Deserialize, Serialize)]
pub struct WgSubnetMap(pub BTreeMap<Ipv4Net, WgSubnetConfig>);
impl Map for WgSubnetMap {
    type Key = Ipv4Net;
    type Value = WgSubnetConfig;
    fn key_str(key: &Self::Key) -> Result<impl AsRef<str>, Error> {
        Self::key_string(key)
    }
    fn key_string(key: &Self::Key) -> Result<InternedString, Error> {
        Ok(InternedString::from_display(key))
    }
}

#[derive(Default, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
pub struct WgSubnetConfig {
    pub default_forward_target: Option<Ipv4Addr>,
    pub clients: BTreeMap<Ipv4Addr, WgConfig>,
}
impl WgSubnetConfig {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn add_client<'a>(
        &'a mut self,
        subnet: Ipv4Net,
    ) -> Result<(Ipv4Addr, &'a WgConfig), Error> {
        let addr = subnet
            .hosts()
            .find(|a| !self.clients.contains_key(a))
            .ok_or_else(|| Error::new(eyre!("subnet exhausted"), ErrorKind::Network))?;
        let config = self.clients.entry(addr).or_insert(WgConfig::generate());
        Ok((addr, config))
    }
}

pub struct WgKey(SigningKey);
impl WgKey {
    pub fn generate() -> Self {
        Self(SigningKey::generate(
            &mut ssh_key::rand_core::OsRng::default(),
        ))
    }
}
impl AsRef<[u8]> for WgKey {
    fn as_ref(&self) -> &[u8] {
        self.0.as_bytes()
    }
}
impl TryFrom<Vec<u8>> for WgKey {
    type Error = ed25519_dalek::SignatureError;
    fn try_from(value: Vec<u8>) -> Result<Self, Self::Error> {
        Ok(Self(value.as_slice().try_into()?))
    }
}
impl std::ops::Deref for WgKey {
    type Target = SigningKey;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl Base64<WgKey> {
    pub fn verifying_key(&self) -> Base64<VerifyingKey> {
        Base64(self.0.verifying_key())
    }
}

#[derive(Deserialize, Serialize, HasModel)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
pub struct WgConfig {
    pub key: Base64<WgKey>,
    pub psk: Base64<[u8; 32]>,
}
impl WgConfig {
    pub fn generate() -> Self {
        Self {
            key: Base64(WgKey::generate()),
            psk: Base64(rand::random()),
        }
    }
    pub fn server_peer_config<'a>(&'a self, addr: Ipv4Addr) -> ServerPeerConfig<'a> {
        ServerPeerConfig {
            client_config: self,
            client_addr: addr,
        }
    }
    pub fn client_config<'a>(
        &'a self,
        addr: Ipv4Addr,
        server_pubkey: Base64<VerifyingKey>,
        server_addr: SocketAddrV4,
    ) -> ClientConfig<'a> {
        ClientConfig {
            client_config: self,
            client_addr: addr,
            server_pubkey,
            server_addr,
        }
    }
}

pub struct ServerPeerConfig<'a> {
    client_config: &'a WgConfig,
    client_addr: Ipv4Addr,
}
impl<'a> std::fmt::Display for ServerPeerConfig<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            include_str!("./server-peer.conf.template"),
            pubkey = self.client_config.key.verifying_key().to_padded_string(),
            psk = self.client_config.psk.to_padded_string(),
            addr = self.client_addr,
        )
    }
}

pub struct ClientConfig<'a> {
    client_config: &'a WgConfig,
    client_addr: Ipv4Addr,
    server_pubkey: Base64<VerifyingKey>,
    server_addr: SocketAddrV4,
}
impl<'a> std::fmt::Display for ClientConfig<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            include_str!("./client.conf.template"),
            privkey = self.client_config.key.to_padded_string(),
            psk = self.client_config.psk,
            addr = self.client_addr,
            server_pubkey = self.server_pubkey.to_padded_string(),
            server_addr = self.server_addr,
        )
    }
}

pub struct ServerConfig<'a>(&'a WgServer);
impl<'a> std::fmt::Display for ServerConfig<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self(server) = *self;
        write!(
            f,
            include_str!("./server.conf.template"),
            subnets = server.subnets.0.keys().join(", "),
            server_port = server.port,
            server_privkey = server.key.to_padded_string(),
        )?;
        for (addr, peer) in server.subnets.0.values().flat_map(|s| &s.clients) {
            write!(f, "{}", peer.server_peer_config(*addr))?;
        }
        Ok(())
    }
}
