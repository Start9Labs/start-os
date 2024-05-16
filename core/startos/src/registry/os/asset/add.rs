use std::collections::{BTreeMap, HashMap};
use std::panic::UnwindSafe;
use std::path::PathBuf;
use std::time::Duration;

use axum::response::Response;
use clap::Parser;
use futures::{FutureExt, TryStreamExt};
use helpers::NonDetachingJoinHandle;
use imbl_value::InternedString;
use itertools::Itertools;
use rpc_toolkit::{from_fn_async, Context, HandlerArgs, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha512};
use ts_rs::TS;
use url::Url;

use crate::context::CliContext;
use crate::prelude::*;
use crate::progress::{FullProgressTracker, PhasedProgressBar};
use crate::registry::asset::RegistryAsset;
use crate::registry::context::RegistryContext;
use crate::registry::os::index::OsVersionInfo;
use crate::registry::os::SIG_CONTEXT;
use crate::registry::signer::commitment::blake3::Blake3Commitment;
use crate::registry::signer::commitment::Digestable;
use crate::registry::signer::sign::ed25519::Ed25519;
use crate::registry::signer::sign::{AnySignature, AnyVerifyingKey, SignatureScheme};
use crate::rpc_continuations::{Guid, RpcContinuation};
use crate::s9pk::merkle_archive::source::multi_cursor_file::MultiCursorFile;
use crate::s9pk::merkle_archive::source::ArchiveSource;
use crate::util::serde::Base64;
use crate::util::{Apply, VersionString};

pub fn add_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
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
        )
}

#[derive(Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AddAssetParams {
    pub version: VersionString,
    #[ts(type = "string")]
    pub platform: InternedString,
    #[serde(default)]
    pub upload: bool,
    #[ts(type = "string")]
    pub url: Url,
    #[serde(rename = "__auth_signer")]
    #[ts(skip)]
    pub signer: AnyVerifyingKey,
    pub signature: AnySignature,
    pub commitment: Blake3Commitment,
}

async fn add_asset(
    ctx: RegistryContext,
    AddAssetParams {
        version,
        platform,
        upload,
        url,
        signer,
        signature,
        commitment,
    }: AddAssetParams,
    accessor: impl FnOnce(
            &mut Model<OsVersionInfo>,
        ) -> &mut Model<BTreeMap<InternedString, RegistryAsset<Blake3Commitment>>>
        + UnwindSafe
        + Send,
) -> Result<Option<Guid>, Error> {
    signer
        .scheme()
        .verify_commitment(&signer, &commitment, SIG_CONTEXT, &signature)?;
    ctx.db
        .mutate(|db| {
            let signer_guid = db.as_index().as_signers().get_signer(&signer)?;
            if db
                .as_index()
                .as_os()
                .as_versions()
                .as_idx(&version)
                .or_not_found(&version)?
                .as_signers()
                .de()?
                .contains(&signer_guid)
            {
                accessor(
                    db.as_index_mut()
                        .as_os_mut()
                        .as_versions_mut()
                        .as_idx_mut(&version)
                        .or_not_found(&version)?,
                )
                .upsert(&platform, || {
                    Ok(RegistryAsset {
                        url,
                        commitment: commitment.clone(),
                        signatures: HashMap::new(),
                    })
                })?
                .mutate(|s| {
                    if s.commitment != commitment {
                        Err(Error::new(
                            eyre!("commitment does not match"),
                            ErrorKind::InvalidSignature,
                        ))
                    } else {
                        s.signatures.insert(signer, signature);
                        Ok(())
                    }
                })?;
                Ok(())
            } else {
                Err(Error::new(eyre!("UNAUTHORIZED"), ErrorKind::Authorization))
            }
        })
        .await?;

    let guid = if upload {
        let guid = Guid::new();
        let auth_guid = guid.clone();
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
                                    todo!();

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

pub async fn add_iso(ctx: RegistryContext, params: AddAssetParams) -> Result<Option<Guid>, Error> {
    add_asset(ctx, params, |m| m.as_iso_mut()).await
}

pub async fn add_img(ctx: RegistryContext, params: AddAssetParams) -> Result<Option<Guid>, Error> {
    add_asset(ctx, params, |m| m.as_img_mut()).await
}

pub async fn add_squashfs(
    ctx: RegistryContext,
    params: AddAssetParams,
) -> Result<Option<Guid>, Error> {
    add_asset(ctx, params, |m| m.as_squashfs_mut()).await
}

#[derive(Debug, Deserialize, Serialize, Parser)]
#[command(rename_all = "kebab-case")]
#[serde(rename_all = "camelCase")]
pub struct CliAddAssetParams {
    #[arg(short = 'p', long = "platform")]
    pub platform: InternedString,
    #[arg(short = 'v', long = "version")]
    pub version: VersionString,
    pub file: PathBuf,
    pub url: Url,
    #[arg(short = 'u', long = "upload")]
    pub upload: bool,
}

pub async fn cli_add_asset(
    HandlerArgs {
        context: ctx,
        parent_method,
        method,
        params:
            CliAddAssetParams {
                platform,
                version,
                file: path,
                url,
                upload,
            },
        ..
    }: HandlerArgs<CliContext, CliAddAssetParams>,
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

    let file = MultiCursorFile::from(tokio::fs::File::open(&path).await?);

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

    sign_phase.start();
    let blake3 = file.blake3_mmap().await?;
    let size = file
        .size()
        .await
        .ok_or_else(|| Error::new(eyre!("failed to read file metadata"), ErrorKind::Filesystem))?;
    let commitment = Blake3Commitment {
        hash: Base64(*blake3.as_bytes()),
        size,
    };
    let signature = Ed25519.sign_commitment(ctx.developer_key()?, &commitment, SIG_CONTEXT)?;
    sign_phase.complete();

    index_phase.start();
    let add_res = from_value::<Option<Guid>>(
        ctx.call_remote::<RegistryContext>(
            &parent_method
                .into_iter()
                .chain(method)
                .chain([ext])
                .join("."),
            imbl_value::json!({
                "platform": platform,
                "version": version,
                "upload": upload,
                "url": &url,
                "signature": signature,
                "commitment": commitment,
            }),
        )
        .await?,
    )?;
    index_phase.complete();

    if let Some(guid) = add_res {
        upload_phase.as_mut().map(|p| p.start());
        upload_phase.as_mut().map(|p| p.set_total(size));
        let reg_url = ctx.registry_url.as_ref().or_not_found("--registry")?;
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
                                reg_url
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
        // upload_phase.as_mut().map(|p| p.complete());
    }

    progress_handle.complete();

    progress_task.await.with_kind(ErrorKind::Unknown)?;

    Ok(())
}
