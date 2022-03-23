use color_eyre::eyre::eyre;
use reqwest::{StatusCode, Url};
use rpc_toolkit::command;
use serde_json::Value;

use crate::{Error, ResultExt};

#[command(subcommands(get))]
pub fn marketplace() -> Result<(), Error> {
    Ok(())
}

#[command]
pub async fn get(#[arg] url: Url) -> Result<Value, Error> {
    let mut response = reqwest::get(url)
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
