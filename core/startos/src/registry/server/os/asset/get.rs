use std::collections::BTreeMap;
use std::panic::UnwindSafe;
use std::path::PathBuf;
use std::time::Duration;

use axum::response::Response;
use clap::Parser;
use futures::{FutureExt, TryStreamExt};
use helpers::NonDetachingJoinHandle;
use imbl_value::InternedString;
use rpc_toolkit::{CallRemote, ParentHandler};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha512};
use ts_rs::TS;
use url::Url;

use crate::context::CliContext;
use crate::prelude::*;
use crate::progress::{FullProgressTracker, PhasedProgressBar};
use crate::registry::asset::RegistryAsset;
use crate::registry::server::context::RegistryContext;
use crate::registry::server::os::index::OsVersionInfo;
use crate::registry::server::os::SIG_CONTEXT;
use crate::registry::signer::{Blake3Ed25519Signature, Signature, SignatureInfo, SignerKey};
use crate::rpc_continuations::{RequestGuid, RpcContinuation};
use crate::s9pk::merkle_archive::source::ArchiveSource;
use crate::util::{Apply, Version};

pub fn get_api() -> ParentHandler {
    ParentHandler::new()
    // .subcommand("iso", from_fn_async(get_iso).no_cli())
    // .subcommand("iso", from_fn_async(cli_get_iso).no_display())
    // .subcommand("img", from_fn_async(get_img).no_cli())
    // .subcommand("img", from_fn_async(cli_get_img).no_display())
    // .subcommand("squashfs", from_fn_async(get_squashfs).no_cli())
    // .subcommand("squashfs", from_fn_async(cli_get_squashfs).no_display())
}
