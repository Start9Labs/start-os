use color_eyre::eyre::eyre;
use reqwest::{StatusCode, Url};
use rpc_toolkit::command;
use serde_json::Value;

use crate::context::RpcContext;
use crate::version::VersionT;
use crate::{Error, ResultExt};

#[command(subcommands(get))]
pub fn marketplace() -> Result<(), Error> {
    Ok(())
}

#[command]
pub async fn get(#[context] ctx: RpcContext, #[arg] mut url: Url) -> Result<Value, Error> {
    url.query_pairs_mut()
        .append_pair(
            "os.version",
            &crate::version::Current::new().semver().to_string(),
        )
        .append_pair(
            "os.compat",
            &crate::version::Current::new().compat().to_string(),
        )
        .append_pair("os.arch", crate::OS_ARCH)
        .append_pair("hardware.arch", &*crate::ARCH)
        .append_pair("hardware.ram", &ctx.hardware.ram.to_string());

    for hw in &ctx.hardware.devices {
        url.query_pairs_mut()
            .append_pair(&format!("hardware.device.{}", hw.class()), hw.product());
    }

    let mut response = ctx
        .client
        .get(url)
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
                base64::encode_config(
                    &response
                        .bytes()
                        .await
                        .with_kind(crate::ErrorKind::Registry)?,
                    base64::URL_SAFE
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
