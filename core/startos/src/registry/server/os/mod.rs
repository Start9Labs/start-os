use std::collections::BTreeMap;
use std::panic::UnwindSafe;
use std::path::PathBuf;
use std::time::Duration;

use axum::response::Response;
use clap::Parser;
use emver::VersionRange;
use futures::{FutureExt, TryStreamExt};
use helpers::NonDetachingJoinHandle;
use imbl_value::InternedString;
use rpc_toolkit::{from_fn_async, AnyContext, CallRemote, Empty, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha512};
use ts_rs::TS;
use url::Url;

use crate::context::CliContext;
use crate::prelude::*;
use crate::progress::{FullProgressTracker, PhasedProgressBar};
use crate::registry::asset::RegistryAsset;
use crate::registry::server::context::RegistryContext;
use crate::registry::signer::{
    Blake3Ed25519Signature, Signature, SignatureInfo, SignerInfo, SignerKey,
};
use crate::rpc_continuations::{RequestGuid, RpcContinuation};
use crate::s9pk::merkle_archive::source::ArchiveSource;
use crate::util::serde::HandlerExtSerde;
use crate::util::{Apply, Version};

pub const SIG_CONTEXT: &str = "startos";

#[derive(Debug, Default, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct OsIndex {
    #[ts(as = "BTreeMap::<String, OsVersionInfo>")]
    pub versions: BTreeMap<Version, OsVersionInfo>,
}

#[derive(Debug, Default, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct OsVersionInfo {
    pub headline: String,
    pub release_notes: String,
    #[ts(type = "string")]
    pub source_version: VersionRange,
    pub signers: Vec<SignerInfo>,
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

#[derive(Debug, Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AddVersionParams {
    #[ts(type = "string")]
    pub version: Version,
    pub headline: String,
    pub release_notes: String,
    #[ts(type = "string")]
    pub source_version: VersionRange,
}

async fn add_version(
    ctx: RegistryContext,
    AddVersionParams {
        version,
        headline,
        release_notes,
        source_version,
    }: AddVersionParams,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            db.as_index_mut()
                .as_os_mut()
                .as_versions_mut()
                .upsert(&version, || OsVersionInfo::default())?
                .mutate(|i| {
                    i.headline = headline;
                    i.release_notes = release_notes;
                    i.source_version = source_version;
                    Ok(())
                })
        })
        .await
}

#[derive(Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AddAssetParams {
    #[ts(type = "string")]
    pub url: Url,
    pub signature: Signature,
    #[ts(type = "string")]
    pub version: Version,
    #[ts(type = "string")]
    pub platform: InternedString,
    #[serde(default)]
    pub upload: bool,
    #[serde(rename = "__auth_signer")]
    pub signer: SignerKey,
}

async fn add_asset(
    ctx: RegistryContext,
    AddAssetParams {
        url,
        signature,
        version,
        platform,
        upload,
        signer,
    }: AddAssetParams,
    accessor: impl FnOnce(&mut Model<OsVersionInfo>) -> &mut Model<BTreeMap<InternedString, RegistryAsset>>
        + UnwindSafe
        + Send,
) -> Result<Option<RequestGuid>, Error> {
    ensure_code!(
        signature.signer() == signer,
        ErrorKind::InvalidSignature,
        "asset signature does not match request signer"
    );
    signature.validate(SIG_CONTEXT)?;

    ctx.db
        .mutate(|db| {
            let model = db
                .as_index_mut()
                .as_os_mut()
                .as_versions_mut()
                .as_idx_mut(&version)
                .or_not_found(&version)?;
            if model
                .as_signers()
                .de()?
                .into_iter()
                .flat_map(|s| s.keys)
                .any(|k| k == signature.signer())
            {
                accessor(model)
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
        let hostname = ctx.hostname.clone();
        ctx.rpc_continuations
            .add(
                guid.clone(),
                RpcContinuation::rest(
                    Box::new(|req| {
                        async move {
                            Ok(
                                if async move {
                                    let auth_sig = base64::decode(
                                        req.headers().get("X-StartOS-Registry-Auth-Sig")?,
                                    )
                                    .ok()?;
                                    signer
                                        .verify_message(
                                            auth_guid.as_ref().as_bytes(),
                                            &auth_sig,
                                            &hostname,
                                        )
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
    params: AddAssetParams,
) -> Result<Option<RequestGuid>, Error> {
    add_asset(ctx, params, |m| m.as_iso_mut()).await
}

pub async fn add_img(
    ctx: RegistryContext,
    params: AddAssetParams,
) -> Result<Option<RequestGuid>, Error> {
    add_asset(ctx, params, |m| m.as_img_mut()).await
}

pub async fn add_squashfs(
    ctx: RegistryContext,
    params: AddAssetParams,
) -> Result<Option<RequestGuid>, Error> {
    add_asset(ctx, params, |m| m.as_squashfs_mut()).await
}

#[derive(Debug, Deserialize, Serialize, Parser)]
#[serde(rename_all = "camelCase")]
pub struct CliAddAssetParams {
    #[arg(short = 'p', long = "platform")]
    pub platform: InternedString,
    #[arg(short = 'v', long = "version")]
    pub version: Version,
    pub file: PathBuf,
    pub url: Url,
    #[arg(short = 'u', long = "upload")]
    pub upload: bool,
}

pub async fn cli_add_asset(
    ctx: CliContext,
    CliAddAssetParams {
        platform,
        version,
        file: path,
        url,
        upload,
    }: CliAddAssetParams,
) -> Result<(), Error> {
    let ext = match path.extension().and_then(|e| e.to_str()) {
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

    let file = tokio::fs::File::open(&path).await?.into();

    let mut progress = FullProgressTracker::new();
    let progress_handle = progress.handle();
    let mut sign_phase =
        progress_handle.add_phase(InternedString::intern("Signing File"), Some(10));
    let mut index_phase = progress_handle.add_phase(
        InternedString::intern("Adding File to Registry Index"),
        Some(1),
    );
    let mut upload_phase = if upload {
        Some(progress_handle.add_phase(InternedString::intern("Uploading File"), Some(100)))
    } else {
        None
    };

    let progress_task: NonDetachingJoinHandle<()> = tokio::spawn(async move {
        let mut bar = PhasedProgressBar::new(&format!("Adding {} to registry...", path.display()));
        loop {
            let snap = progress.snapshot();
            bar.update(&snap);
            if snap.overall.is_complete() {
                break;
            }
            progress.changed().await
        }
    })
    .into();

    let blake3_sig =
        Blake3Ed25519Signature::sign_file(ctx.developer_key()?, &file, SIG_CONTEXT).await?;
    let size = blake3_sig.size;
    let signature = Signature::Blake3Ed25519(blake3_sig);
    sign_phase.complete();

    let add_res = from_value::<Option<RequestGuid>>(
        <CliContext as CallRemote<RegistryContext>>::call_remote(
            &ctx,
            &format!("os.add-asset.{ext}"),
            imbl_value::json!({
                "platform": platform,
                "version": version,
                "url": &url,
                "signature": signature,
                "upload": upload,
            }),
        )
        .await?,
    )?;
    index_phase.complete();

    if let Some(guid) = add_res {
        upload_phase.as_mut().map(|p| p.set_total(size));
        ctx.client
            .post(url)
            .header("X-StartOS-Registry-Token", guid.as_ref())
            .header(
                "X-StartOS-Registry-Auth-Sig",
                base64::encode(
                    ctx.developer_key()?
                        .sign_prehashed(
                            Sha512::new_with_prefix(guid.as_ref().as_bytes()),
                            Some(
                                ctx.registry_url
                                    .host()
                                    .or_not_found("registry hostname")?
                                    .to_string()
                                    .as_bytes(),
                            ),
                        )?
                        .to_bytes(),
                ),
            )
            .body(reqwest::Body::wrap_stream(
                tokio_util::io::ReaderStream::new(file.fetch(0, size).await?).inspect_ok(
                    move |b| {
                        upload_phase
                            .as_mut()
                            .map(|p| *p += b.len() as u64)
                            .apply(|_| ())
                    },
                ),
            ))
            .send()
            .await?;
    }

    progress_handle.complete();

    progress_task.await.with_kind(ErrorKind::Unknown)?;

    Ok(())
}

pub fn os_api() -> ParentHandler {
    ParentHandler::new()
        .subcommand(
            "index",
            from_fn_async(get_os_index).with_display_serializable(), // .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "asset",
            ParentHandler::<Empty, Empty>::new()
                .subcommand(
                    "add",
                    ParentHandler::<Empty, Empty>::new()
                        .root_handler(from_fn_async(cli_add_asset).no_display())
                        .subcommand(
                            "iso",
                            from_fn_async(add_iso)
                                .with_metadata("getSigner", Value::Bool(true))
                                .no_cli(),
                        )
                        .subcommand(
                            "img",
                            from_fn_async(add_img)
                                .with_metadata("getSigner", Value::Bool(true))
                                .no_cli(),
                        )
                        .subcommand(
                            "squashfs",
                            from_fn_async(add_squashfs)
                                .with_metadata("getSigner", Value::Bool(true))
                                .no_cli(),
                        ),
                )
                .subcommand(
                    "sign",
                    ParentHandler::<Empty, Empty>::new()
                        .root_handler(from_fn_async(cli_sign_asset).no_display())
                        .subcommand("iso", from_fn_async(sign_iso).no_cli())
                        .subcommand("img", from_fn_async(sign_img).no_cli())
                        .subcommand("squashfs", from_fn_async(sign_squashfs).no_cli()),
                )
                .subcommand(
                    "get",
                    ParentHandler::<Empty, Empty>::new()
                        .subcommand("iso", from_fn_async(get_iso).no_cli())
                        .subcommand("iso", from_fn_async(cli_get_iso).no_display())
                        .subcommand("img", from_fn_async(get_img).no_cli())
                        .subcommand("img", from_fn_async(cli_get_img).no_display())
                        .subcommand("squashfs", from_fn_async(get_squashfs).no_cli())
                        .subcommand("squashfs", from_fn_async(cli_get_squashfs).no_display()),
                ),
        )
        .subcommand(
            "version",
            ParentHandler::<Empty, Empty>::new()
                .subcommand(
                    "add",
                    from_fn_async(add_version)
                        .with_metadata("admin", Value::Bool(true))
                        .with_metadata("getSigner", Value::Bool(true))
                        .no_display()
                        .with_call_remote::<CliContext>(),
                )
                .subcommand(
                    "signer",
                    ParentHandler::<Empty, Empty>::new()
                        .subcommand(
                            "add",
                            from_fn_async(add_version_signer)
                                .with_metadata("admin", Value::Bool(true))
                                .no_display()
                                .with_call_remote::<CliContext>(),
                        )
                        .subcommand(
                            "remove",
                            from_fn_async(remove_version_signer)
                                .with_metadata("admin", Value::Bool(true))
                                .no_display()
                                .with_call_remote::<CliContext>(),
                        ),
                )
                .subcommand(
                    "get",
                    from_fn_async(get_version)
                        .with_display_serializable()
                        .with_custom_display_fn::<AnyContext, _>(|handle, result| {
                            Ok(display_version_info(handle.params, result))
                        })
                        .with_call_remote::<CliContext>(),
                ),
        )
}
