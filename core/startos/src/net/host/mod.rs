use imbl_value::InternedString;
use serde::{Deserialize, Serialize};

use crate::net::host::multi::MultiHost;

pub mod multi;

pub enum Host {
    Multi(MultiHost),
    // Single(SingleHost),
    // Static(StaticHost),
}

#[derive(Deserialize, Serialize)]
pub struct BindOptions {
    scheme: InternedString,
    preferred_external_port: u16,
    add_ssl: Option<AddSslOptions>,
    secure: bool,
    ssl: bool,
}

#[derive(Deserialize, Serialize)]
pub struct AddSslOptions {
    scheme: InternedString,
    preferred_external_port: u16,
    #[serde(default)]
    add_x_forwarded_headers: bool,
}
