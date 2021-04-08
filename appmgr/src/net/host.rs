use std::ffi::{OsStr, OsString};
use std::net::Ipv4Addr;

use indexmap::IndexMap;
use patch_db::DbHandle;
use serde::{Deserialize, Serialize};

use crate::s9pk::manifest::PackageId;
use crate::{Error, HOST_IP};

pub const TLD: &'static str = "embassy";

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct Hosts(pub IndexMap<PackageId, Ipv4Addr>);
impl Hosts {
    pub fn docker_args(&self) -> Vec<OsString> {
        let mut res = Vec::with_capacity(self.0.len() + 1);
        res.push(format!("--add-host={}:{}", TLD, Ipv4Addr::from(HOST_IP)).into());
        for (id, ip) in &self.0 {
            res.push(format!("--add-host={}.{}:{}", id, TLD, ip).into());
        }
        res
    }
}
