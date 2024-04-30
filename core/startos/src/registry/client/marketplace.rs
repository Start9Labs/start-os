use base64::Engine;
use clap::Parser;
use color_eyre::eyre::eyre;
use reqwest::{StatusCode, Url};
use rpc_toolkit::{command, from_fn_async, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use ts_rs::TS;

use crate::context::{CliContext, RpcContext};
use crate::version::VersionT;
use crate::{Error, ResultExt};

pub fn marketplace() -> ParentHandler {
    ParentHandler::new().subcommand("get", from_fn_async(get).with_call_remote::<CliContext>())
}

pub fn with_query_params(ctx: RpcContext, mut url: Url) -> Url {
    url.query_pairs_mut()
        .append_pair(
            "os.version",
            &crate::version::Current::new().semver().to_string(),
        )
        .append_pair(
            "os.compat",
            &crate::version::Current::new().compat().to_string(),
        )
        .append_pair("os.arch", &*crate::PLATFORM)
        .append_pair("hardware.arch", &*crate::ARCH)
        .append_pair("hardware.ram", &ctx.hardware.ram.to_string());

    for hw in &ctx.hardware.devices {
        url.query_pairs_mut()
            .append_pair(&format!("hardware.device.{}", hw.class()), hw.product());
    }

    url
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct GetParams {
    #[ts(type = "string")]
    url: Url,
}

pub async fn get(ctx: RpcContext, GetParams { url }: GetParams) -> Result<Value, Error> {
    let mut response = ctx
        .client
        .get(with_query_params(ctx.clone(), url))
        .send()
        .await
        .with_kind(crate::ErrorKind::Network)?;
    let status = response.status();
    if status.is_success() {
        match response
            .headers_mut()
            .remove("Content-Type")
            .as_ref()
            .and_then(|h| h.to_str().ok())
            .and_then(|h| h.split(";").next())
            .map(|h| h.trim())
        {
            Some("application/json") => response
                .json()
                .await
                .with_kind(crate::ErrorKind::Deserialization),
            Some("text/plain") => Ok(Value::String(
                response
                    .text()
                    .await
                    .with_kind(crate::ErrorKind::Registry)?,
            )),
            Some(ctype) => Ok(Value::String(format!(
                "data:{};base64,{}",
                ctype,
                base64::engine::general_purpose::URL_SAFE.encode(
                    &response
                        .bytes()
                        .await
                        .with_kind(crate::ErrorKind::Registry)?
                )
            ))),
            _ => Err(Error::new(
                eyre!("missing Content-Type"),
                crate::ErrorKind::Registry,
            )),
        }
    } else {
        let message = response.text().await.with_kind(crate::ErrorKind::Network)?;
        Err(Error::new(
            eyre!("{}", message),
            match status {
                StatusCode::BAD_REQUEST => crate::ErrorKind::InvalidRequest,
                StatusCode::NOT_FOUND => crate::ErrorKind::NotFound,
                _ => crate::ErrorKind::Registry,
            },
        ))
    }
}
