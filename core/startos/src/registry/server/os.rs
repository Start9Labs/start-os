use std::collections::BTreeMap;
use std::panic::UnwindSafe;
use std::path::PathBuf;
use std::time::Duration;

use axum::response::Response;
use clap::Parser;
use emver::VersionRange;
use futures::FutureExt;
use helpers::const_true;
use imbl_value::InternedString;
use rpc_toolkit::{from_fn_async, CallRemote, Empty, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use ts_rs::TS;
use url::Url;

use crate::context::{CliContext, RegistryContext};
use crate::prelude::*;
use crate::registry::asset::RegistryAsset;
use crate::registry::signer::{Blake3Ed25519Signature, Signature, SignatureInfo, SignerInfo};
use crate::rpc_continuations::{RequestGuid, RestHandler, RpcContinuation};
use crate::util::serde::HandlerExtSerde;
use crate::util::Version;

pub const SIG_CONTEXT: &str = "startos";

#[derive(Debug, Default, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct OsIndex {
    pub signers: Vec<SignerInfo>,
    #[ts(as = "BTreeMap::<String, OsVersionInfo>")]
    pub versions: BTreeMap<Version, OsVersionInfo>,
}

#[derive(Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct OsVersionInfo {
    pub headline: String,
    pub release_notes: String,
    #[ts(type = "string")]
    pub source_version: VersionRange,
    #[ts(as = "BTreeMap::<String, RegistryAsset>")]
    pub iso: BTreeMap<InternedString, RegistryAsset>, // platform (i.e. x86_64-nonfree) -> asset
    #[ts(as = "BTreeMap::<String, RegistryAsset>")]
    pub squashfs: BTreeMap<InternedString, RegistryAsset>, // platform (i.e. x86_64-nonfree) -> asset
    #[ts(as = "BTreeMap::<String, RegistryAsset>")]
    pub img: BTreeMap<InternedString, RegistryAsset>, // platform (i.e. raspberrypi) -> asset
}

pub async fn get_os_index(ctx: RegistryContext) -> Result<OsIndex, Error> {
    ctx.db.peek().await.into_index().into_os().de()
}

#[derive(Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AddParams {
    #[ts(type = "string")]
    pub url: Url,
    pub signature: Signature,
    #[ts(type = "string")]
    pub version: Version,
    #[ts(type = "string")]
    pub platform: InternedString,
    #[serde(default)]
    pub upload: bool,
}

async fn add(
    ctx: RegistryContext,
    AddParams {
        url,
        signature,
        version,
        platform,
        upload,
    }: AddParams,
    accessor: impl FnOnce(&mut Model<OsVersionInfo>) -> &mut Model<BTreeMap<InternedString, RegistryAsset>>
        + UnwindSafe
        + Send,
) -> Result<Option<RequestGuid>, Error> {
    signature.validate(SIG_CONTEXT)?;

    ctx.db
        .mutate(|db| {
            if db
                .as_index()
                .as_os()
                .as_signers()
                .de()?
                .into_iter()
                .flat_map(|s| s.keys)
                .any(|k| k == signature.signer())
            {
                accessor(
                    db.as_index_mut()
                        .as_os_mut()
                        .as_versions_mut()
                        .as_idx_mut(&version)
                        .or_not_found(&version)?,
                )
                .upsert(&platform, || RegistryAsset {
                    url,
                    signature_info: SignatureInfo::new(),
                })?
                .as_signature_info_mut()
                .mutate(|s| s.add_sig(&signature, SIG_CONTEXT))?;
                Ok(())
            } else {
                Err(Error::new(eyre!("UNAUTHORIZED"), ErrorKind::Authorization))
            }
        })
        .await?;

    let guid = if upload {
        let guid = RequestGuid::new();
        let auth_guid = guid.clone();
        let signer = signature.signer();
        ctx.rpc_continuations
            .add(
                guid.clone(),
                RpcContinuation::rest(
                    Box::new(|req| {
                        async move {
                            Ok(
                                if async move {
                                    let auth_sig = base64::decode(
                                        req.headers().get("X-StartOS-Registry-Auth")?,
                                    )
                                    .ok()?;
                                    signer
                                        .verify_message(auth_guid.as_ref().as_bytes(), &auth_sig)
                                        .ok()?;

                                    Some(())
                                }
                                .await
                                .is_some()
                                {
                                    Response::builder()
                                        .status(200)
                                        .body(axum::body::Body::empty())
                                        .with_kind(ErrorKind::Network)?
                                } else {
                                    Response::builder()
                                        .status(401)
                                        .body(axum::body::Body::empty())
                                        .with_kind(ErrorKind::Network)?
                                },
                            )
                        }
                        .boxed()
                    }),
                    Duration::from_secs(30),
                ),
            )
            .await;
        Some(guid)
    } else {
        None
    };

    Ok(guid)
}

pub async fn add_iso(
    ctx: RegistryContext,
    params: AddParams,
) -> Result<Option<RequestGuid>, Error> {
    add(ctx, params, |m| m.as_iso_mut()).await
}

pub async fn add_img(
    ctx: RegistryContext,
    params: AddParams,
) -> Result<Option<RequestGuid>, Error> {
    add(ctx, params, |m| m.as_img_mut()).await
}

pub async fn add_squashfs(
    ctx: RegistryContext,
    params: AddParams,
) -> Result<Option<RequestGuid>, Error> {
    add(ctx, params, |m| m.as_squashfs_mut()).await
}

#[derive(Debug, Deserialize, Serialize, Parser)]
#[serde(rename_all = "camelCase")]
pub struct CliAddParams {
    #[arg(short = 'p', long = "platform")]
    pub platform: InternedString,
    #[arg(short = 'v', long = "version")]
    pub version: Version,
    pub file: PathBuf,
    pub url: Url,
    #[arg(short = 'u', long = "upload")]
    pub upload: bool,
}

pub async fn cli_add(
    ctx: CliContext,
    CliAddParams {
        platform,
        version,
        file,
        url,
        upload,
    }: CliAddParams,
) -> Result<(), Error> {
    let ext = match file.extension().and_then(|e| e.to_str()) {
        Some("iso") => "iso",
        Some("img") => "img",
        Some("squashfs") => "squashfs",
        _ => {
            return Err(Error::new(
                eyre!("Unknown extension"),
                ErrorKind::InvalidRequest,
            ))
        }
    };

    let file = tokio::fs::File::open(file).await?.into();
    let signature = Signature::Blake3Ed25519(
        Blake3Ed25519Signature::sign_file(ctx.developer_key()?, &file, SIG_CONTEXT).await?,
    );

    let add_res = from_value::<Option<RequestGuid>>(
        <CliContext as CallRemote<RegistryContext>>::call_remote(
            &ctx,
            &format!("os.add.{ext}"),
            to_value(&AddParams {
                platform,
                version,
                url,
                signature,
                upload,
            })?,
        )
        .await?,
    )?;
    todo!()
}

pub fn os_api() -> ParentHandler {
    ParentHandler::new()
        .subcommand(
            "index",
            from_fn_async(get_os_index).with_display_serializable(), // .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "add",
            ParentHandler::<Empty, Empty>::new()
                .root_handler(from_fn_async(cli_add).no_display())
                .subcommand("iso", from_fn_async(add_iso).no_cli())
                .subcommand("img", from_fn_async(add_img).no_cli())
                .subcommand("squashfs", from_fn_async(add_squashfs).no_cli()),
        )
}
