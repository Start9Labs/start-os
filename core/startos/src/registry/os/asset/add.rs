use std::collections::{BTreeMap, HashMap};
use std::panic::UnwindSafe;
use std::path::PathBuf;

use chrono::Utc;
use clap::Parser;
use imbl_value::InternedString;
use itertools::Itertools;
use rpc_toolkit::{from_fn_async, Context, HandlerArgs, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use ts_rs::TS;
use url::Url;

use crate::context::CliContext;
use crate::prelude::*;
use crate::progress::FullProgressTracker;
use crate::registry::asset::RegistryAsset;
use crate::registry::context::RegistryContext;
use crate::registry::os::index::OsVersionInfo;
use crate::registry::os::SIG_CONTEXT;
use crate::registry::signer::commitment::blake3::Blake3Commitment;
use crate::registry::signer::sign::ed25519::Ed25519;
use crate::registry::signer::sign::{AnySignature, AnyVerifyingKey, SignatureScheme};
use crate::s9pk::merkle_archive::hash::VerifyingWriter;
use crate::s9pk::merkle_archive::source::http::HttpSource;
use crate::s9pk::merkle_archive::source::multi_cursor_file::MultiCursorFile;
use crate::s9pk::merkle_archive::source::ArchiveSource;
use crate::util::io::open_file;
use crate::util::serde::Base64;
use crate::util::VersionString;

pub fn add_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "iso",
            from_fn_async(add_iso)
                .with_metadata("get_signer", Value::Bool(true))
                .no_cli(),
        )
        .subcommand(
            "img",
            from_fn_async(add_img)
                .with_metadata("get_signer", Value::Bool(true))
                .no_cli(),
        )
        .subcommand(
            "squashfs",
            from_fn_async(add_squashfs)
                .with_metadata("get_signer", Value::Bool(true))
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
) -> Result<(), Error> {
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
                .as_authorized()
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
                        published_at: Utc::now(),
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

    Ok(())
}

pub async fn add_iso(ctx: RegistryContext, params: AddAssetParams) -> Result<(), Error> {
    add_asset(ctx, params, |m| m.as_iso_mut()).await
}

pub async fn add_img(ctx: RegistryContext, params: AddAssetParams) -> Result<(), Error> {
    add_asset(ctx, params, |m| m.as_img_mut()).await
}

pub async fn add_squashfs(ctx: RegistryContext, params: AddAssetParams) -> Result<(), Error> {
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

    let file = MultiCursorFile::from(open_file(&path).await?);

    let progress = FullProgressTracker::new();
    let mut sign_phase = progress.add_phase(InternedString::intern("Signing File"), Some(10));
    let mut verify_phase = progress.add_phase(InternedString::intern("Verifying URL"), Some(100));
    let mut index_phase = progress.add_phase(
        InternedString::intern("Adding File to Registry Index"),
        Some(1),
    );

    let progress_task =
        progress.progress_bar_task(&format!("Adding {} to registry...", path.display()));

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

    verify_phase.start();
    let src = HttpSource::new(ctx.client.clone(), url.clone()).await?;
    let mut writer = verify_phase.writer(VerifyingWriter::new(
        tokio::io::sink(),
        Some((blake3::Hash::from_bytes(*commitment.hash), commitment.size)),
    ));
    src.copy_all_to(&mut writer).await?;
    let (verifier, mut verify_phase) = writer.into_inner();
    verifier.verify().await?;
    verify_phase.complete();

    index_phase.start();
    ctx.call_remote::<RegistryContext>(
        &parent_method
            .into_iter()
            .chain(method)
            .chain([ext])
            .join("."),
        imbl_value::json!({
            "platform": platform,
            "version": version,
            "url": &url,
            "signature": signature,
            "commitment": commitment,
        }),
    )
    .await?;
    index_phase.complete();

    progress.complete();

    progress_task.await.with_kind(ErrorKind::Unknown)?;

    Ok(())
}
