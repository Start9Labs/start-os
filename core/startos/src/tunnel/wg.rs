use std::collections::BTreeMap;
use std::net::{Ipv4Addr, SocketAddr};

use imbl_value::InternedString;
use ipnet::Ipv4Net;
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use tokio::process::Command;
use ts_rs::TS;
use x25519_dalek::{PublicKey, StaticSecret};

use crate::prelude::*;
use crate::util::Invoke;
use crate::util::io::write_file_atomic;
use crate::util::serde::Base64;

pub const WIREGUARD_INTERFACE_NAME: &str = "wg-start-tunnel";

#[derive(Deserialize, Serialize, HasModel, TS)]
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
            .arg(WIREGUARD_INTERFACE_NAME)
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
            const_format::formatcp!("/etc/wireguard/{WIREGUARD_INTERFACE_NAME}.conf"),
            self.server_config().to_string().as_bytes(),
        )
        .await?;
        Command::new("wg-quick")
            .arg("up")
            .arg(WIREGUARD_INTERFACE_NAME)
            .invoke(ErrorKind::Network)
            .await?;
        Ok(())
    }
}

#[derive(Default, Deserialize, Serialize, TS)]
pub struct WgSubnetMap(
    #[ts(as = "BTreeMap::<String, WgSubnetConfig>")] pub BTreeMap<Ipv4Net, WgSubnetConfig>,
);
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

#[derive(Default, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
pub struct WgSubnetConfig {
    pub name: InternedString,
    pub clients: WgSubnetClients,
}
impl WgSubnetConfig {
    pub fn new(name: InternedString) -> Self {
        Self {
            name,
            ..Self::default()
        }
    }
}

#[derive(Default, Deserialize, Serialize, TS)]
pub struct WgSubnetClients(pub BTreeMap<Ipv4Addr, WgConfig>);
impl Map for WgSubnetClients {
    type Key = Ipv4Addr;
    type Value = WgConfig;
    fn key_str(key: &Self::Key) -> Result<impl AsRef<str>, Error> {
        Self::key_string(key)
    }
    fn key_string(key: &Self::Key) -> Result<InternedString, Error> {
        Ok(InternedString::from_display(key))
    }
}

#[derive(Clone)]
pub struct WgKey(StaticSecret);
impl WgKey {
    pub fn generate() -> Self {
        Self(StaticSecret::random_from_rng(
            ssh_key::rand_core::OsRng::default(),
        ))
    }
}
impl AsRef<[u8]> for WgKey {
    fn as_ref(&self) -> &[u8] {
        self.0.as_bytes()
    }
}
impl TryFrom<Vec<u8>> for WgKey {
    type Error = Error;
    fn try_from(value: Vec<u8>) -> Result<Self, Self::Error> {
        Ok(Self(
            <[u8; 32]>::try_from(value)
                .map_err(|_| Error::new(eyre!("invalid key length"), ErrorKind::Deserialization))?
                .into(),
        ))
    }
}
impl std::ops::Deref for WgKey {
    type Target = StaticSecret;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl Base64<WgKey> {
    pub fn verifying_key(&self) -> Base64<PublicKey> {
        Base64((&*self.0).into())
    }
}

#[derive(Clone, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
pub struct WgConfig {
    pub name: InternedString,
    pub key: Base64<WgKey>,
    pub psk: Base64<[u8; 32]>,
}
impl WgConfig {
    pub fn generate(name: InternedString) -> Self {
        Self {
            name,
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
    pub fn client_config(
        self,
        addr: Ipv4Addr,
        subnet: Ipv4Net,
        server_pubkey: Base64<PublicKey>,
        server_addr: SocketAddr,
    ) -> ClientConfig {
        ClientConfig {
            client_config: self,
            client_addr: addr,
            subnet,
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

fn deserialize_verifying_key<'de, D>(deserializer: D) -> Result<Base64<PublicKey>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    Base64::<Vec<u8>>::deserialize(deserializer).and_then(|b| {
        Ok(Base64(PublicKey::from(<[u8; 32]>::try_from(b.0).map_err(
            |e: Vec<u8>| serde::de::Error::invalid_length(e.len(), &"a 32 byte base64 string"),
        )?)))
    })
}

#[derive(Clone, Serialize, Deserialize)]
pub struct ClientConfig {
    client_config: WgConfig,
    client_addr: Ipv4Addr,
    subnet: Ipv4Net,
    #[serde(deserialize_with = "deserialize_verifying_key")]
    server_pubkey: Base64<PublicKey>,
    server_addr: SocketAddr,
}
impl std::fmt::Display for ClientConfig {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            include_str!("./client.conf.template"),
            name = self.client_config.name,
            privkey = self.client_config.key.to_padded_string(),
            psk = self.client_config.psk.to_padded_string(),
            addr = self.client_addr,
            subnet = self.subnet,
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
        for (addr, peer) in server.subnets.0.values().flat_map(|s| &s.clients.0) {
            write!(f, "{}", peer.server_peer_config(*addr))?;
        }
        Ok(())
    }
}
