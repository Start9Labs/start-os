use reqwest::Url;

use crate::context::RpcContext;
use crate::s9pk::manifest::Manifest;

pub struct ManagerSeed {
    pub ctx: RpcContext,
    pub manifest: Manifest,
    pub container_name: String,
    pub marketplace_url: Option<Url>,
}
