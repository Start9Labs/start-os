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
    let response = reqwest::get(url)
        .await
        .with_kind(crate::ErrorKind::Network)?;
    let status = response.status();
    if status.is_success() {
        response
            .json()
            .await
            .with_kind(crate::ErrorKind::Deserialization)
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
