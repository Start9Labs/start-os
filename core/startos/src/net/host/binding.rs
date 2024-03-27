use imbl_value::InternedString;
use serde::{Deserialize, Serialize};

use crate::net::forward::AvailablePorts;
use crate::net::vhost::AlpnInfo;
use crate::prelude::*;

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct BindInfo {
    pub options: BindOptions,
    pub assigned_lan_port: Option<u16>,
}
impl BindInfo {
    pub fn new(available_ports: &mut AvailablePorts, options: BindOptions) -> Result<Self, Error> {
        let mut assigned_lan_port = None;
        if options.add_ssl.is_some() || options.secure {
            assigned_lan_port = Some(available_ports.alloc()?);
        }
        Ok(Self {
            options,
            assigned_lan_port,
        })
    }
    pub fn update(
        self,
        available_ports: &mut AvailablePorts,
        options: BindOptions,
    ) -> Result<Self, Error> {
        let Self {
            mut assigned_lan_port,
            ..
        } = self;
        if options.add_ssl.is_some() || options.secure {
            assigned_lan_port = if let Some(port) = assigned_lan_port.take() {
                Some(port)
            } else {
                Some(available_ports.alloc()?)
            };
        } else {
            if let Some(port) = assigned_lan_port.take() {
                available_ports.free([port]);
            }
        }
        Ok(Self {
            options,
            assigned_lan_port,
        })
    }
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct BindOptions {
    pub scheme: InternedString,
    pub preferred_external_port: u16,
    pub add_ssl: Option<AddSslOptions>,
    pub secure: bool,
    pub ssl: bool,
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct AddSslOptions {
    pub scheme: InternedString,
    pub preferred_external_port: u16,
    // #[serde(default)]
    // pub add_x_forwarded_headers: bool, // TODO
    #[serde(default)]
    pub alpn: AlpnInfo,
}
