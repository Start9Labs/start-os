use patch_db::DbHandle;
use reqwest::Url;
use rpc_toolkit::command;

use crate::context::RpcContext;
use crate::util::display_none;
use crate::Error;

#[command(rename = "set-marketplace", display(display_none))]
pub async fn set_eos_url(#[context] ctx: RpcContext, #[arg] url: Url) -> Result<(), Error> {
    let mut db = ctx.db.handle();
    let mut tx = db.begin().await?;
    crate::db::DatabaseModel::new()
        .server_info()
        .eos_marketplace()
        .put(&mut tx, &url)
        .await?;
    ctx.set_nginx_conf(&mut tx).await?;
    tx.commit(None).await?;
    Ok(())
}

#[command(rename = "set-marketplace", display(display_none))]
pub async fn set_package_url(#[context] ctx: RpcContext, #[arg] url: Url) -> Result<(), Error> {
    let mut db = ctx.db.handle();
    let mut tx = db.begin().await?;
    crate::db::DatabaseModel::new()
        .server_info()
        .package_marketplace()
        .put(&mut tx, &Some(url))
        .await?;
    ctx.set_nginx_conf(&mut tx).await?;
    tx.commit(None).await?;
    Ok(())
}
