use clap::ArgMatches;
use rpc_toolkit::command;
use serde_json::Value;

use crate::context::RpcContext;
use crate::update::latest_information::LatestInformation;
use crate::{Error, ErrorKind, ResultExt};

const URL: &str = "https://beta-registry-0-3.start9labs.com/eos/latest";
mod latest_information;

pub fn display_properties(response: (), _: &ArgMatches<'_>) {
    println!("Test");
}
#[command(display(display_properties))]
pub async fn update_system(#[context] ctx: RpcContext) -> Result<(), Error> {
    if let None = fetch_file(ctx).await? {
        return Ok(());
    }
    todo!()
}

pub async fn fetch_file(ctx: RpcContext) -> Result<Option<()>, Error> {
    let mut db = ctx.db.handle();
    let latest_version = reqwest::get(URL)
        .await
        .with_kind(ErrorKind::Network)?
        .json::<LatestInformation>()
        .await
        .with_kind(ErrorKind::Network)?
        .version;
    let current_version = crate::db::DatabaseModel::new()
        .server_info()
        .version()
        .get(&mut db, false)
        .await?;
    if &latest_version > &current_version {
        todo!("If new pull down only one (Mutex lock this)")
    } else {
        todo!("Skip the rest")
    }
}

pub async fn check_download(ctx: RpcContext) -> Result<Value, Error> {
    let hash_from_header = todo!();
    let hash_from_file = todo!();
    todo!("Fail if bad check")
}

pub async fn swap(ctx: RpcContext) -> Result<Value, Error> {
    todo!("Do swap");
    todo!("Let system know that we need a reboot or something")
}
