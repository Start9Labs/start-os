use std::collections::BTreeMap;
use std::convert::identity;
use std::ops::Deref;

use axum::extract::Request;
use axum::response::Response;
use emver::{Version, VersionRange};
use http::HeaderValue;
use imbl_value::InternedString;
use rpc_toolkit::{Middleware, RpcRequest, RpcResponse};
use serde::{Deserialize, Serialize};
use ts_rs::TS;
use url::Url;

use crate::context::RpcContext;
use crate::prelude::*;
use crate::registry::context::RegistryContext;
use crate::util::VersionString;
use crate::version::VersionT;

pub const DEVICE_INFO_HEADER: &str = "X-StartOS-Device-Info";

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct DeviceInfo {
    pub os: OsInfo,
    pub hardware: HardwareInfo,
}
impl From<&RpcContext> for DeviceInfo {
    fn from(value: &RpcContext) -> Self {
        Self {
            os: OsInfo::from(value),
            hardware: HardwareInfo::from(value),
        }
    }
}
impl DeviceInfo {
    pub fn to_header_value(&self) -> HeaderValue {
        let mut url: Url = "http://localhost".parse().unwrap();
        url.query_pairs_mut()
            .append_pair("os.version", &self.os.version.to_string())
            .append_pair("os.compat", &self.os.compat.to_string())
            .append_pair("os.platform", &*self.os.platform)
            .append_pair("hardware.arch", &*self.hardware.arch)
            .append_pair("hardware.ram", &self.hardware.ram.to_string());

        for (class, products) in &self.hardware.devices {
            for product in products {
                url.query_pairs_mut()
                    .append_pair(&format!("hardware.device.{}", class), product);
            }
        }

        HeaderValue::from_str(url.query().unwrap_or_default()).unwrap()
    }
    pub fn from_header_value(header: &HeaderValue) -> Result<Self, Error> {
        let url: Url = format!(
            "http://localhost/?{}",
            header.to_str().with_kind(ErrorKind::ParseUrl)?
        )
        .parse()?;
        let query: BTreeMap<_, _> = url.query_pairs().collect();
        Ok(Self {
            os: OsInfo {
                version: query
                    .get("os.version")
                    .or_not_found("os.version")?
                    .parse()?,
                compat: query.get("os.compat").or_not_found("os.compat")?.parse()?,
                platform: query
                    .get("os.platform")
                    .or_not_found("os.platform")?
                    .deref()
                    .into(),
            },
            hardware: HardwareInfo {
                arch: query
                    .get("hardware.arch")
                    .or_not_found("hardware.arch")?
                    .parse()?,
                ram: query
                    .get("hardware.ram")
                    .or_not_found("hardware.ram")?
                    .parse()?,
                devices: identity(query)
                    .split_off("hardware.device.")
                    .into_iter()
                    .filter_map(|(k, v)| {
                        k.strip_prefix("hardware.device.")
                            .map(|k| (k.into(), v.into_owned()))
                    })
                    .fold(BTreeMap::new(), |mut acc, (k, v)| {
                        let mut devs = acc.remove(&k).unwrap_or_default();
                        devs.push(v);
                        acc.insert(k, devs);
                        acc
                    }),
            },
        })
    }
}

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct OsInfo {
    #[ts(as = "VersionString")]
    pub version: Version,
    #[ts(type = "string")]
    pub compat: VersionRange,
    #[ts(type = "string")]
    pub platform: InternedString,
}
impl From<&RpcContext> for OsInfo {
    fn from(_: &RpcContext) -> Self {
        Self {
            version: crate::version::Current::new().semver(),
            compat: crate::version::Current::new().compat().clone(),
            platform: InternedString::intern(&*crate::PLATFORM),
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct HardwareInfo {
    #[ts(type = "string")]
    pub arch: InternedString,
    #[ts(type = "number")]
    pub ram: u64,
    #[ts(as = "BTreeMap::<String, Vec<String>>")]
    pub devices: BTreeMap<InternedString, Vec<String>>,
}

impl From<&RpcContext> for HardwareInfo {
    fn from(value: &RpcContext) -> Self {
        Self {
            arch: InternedString::intern(&**crate::ARCH),
            ram: value.hardware.ram,
            devices: value
                .hardware
                .devices
                .iter()
                .fold(BTreeMap::new(), |mut acc, dev| {
                    let mut devs = acc.remove(dev.class()).unwrap_or_default();
                    devs.push(dev.product().to_owned());
                    acc.insert(dev.class().into(), devs);
                    acc
                }),
        }
    }
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Metadata {
    #[serde(default)]
    get_device_info: bool,
}

#[derive(Clone)]
pub struct DeviceInfoMiddleware {
    device_info: Option<HeaderValue>,
}
impl DeviceInfoMiddleware {
    pub fn new() -> Self {
        Self { device_info: None }
    }
}

impl Middleware<RegistryContext> for DeviceInfoMiddleware {
    type Metadata = Metadata;
    async fn process_http_request(
        &mut self,
        _: &RegistryContext,
        request: &mut Request,
    ) -> Result<(), Response> {
        self.device_info = request.headers_mut().remove(DEVICE_INFO_HEADER);
        Ok(())
    }
    async fn process_rpc_request(
        &mut self,
        _: &RegistryContext,
        metadata: Self::Metadata,
        request: &mut RpcRequest,
    ) -> Result<(), RpcResponse> {
        async move {
            if metadata.get_device_info {
                if let Some(device_info) = &self.device_info {
                    request.params["__device_info"] =
                        to_value(&DeviceInfo::from_header_value(device_info)?)?;
                }
            }

            Ok::<_, Error>(())
        }
        .await
        .map_err(|e| RpcResponse::from_result(Err(e)))
    }
}
