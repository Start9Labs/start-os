use std::fs::File;
use std::io::Write;
use std::path::Path;

use ed25519::pkcs8::EncodePrivateKey;
use ed25519_dalek::Keypair;
use rpc_toolkit::command;
use tracing::instrument;

use crate::context::SdkContext;
use crate::util::display_none;
use crate::{Error, ResultExt};

#[command(cli_only, blocking, display(display_none))]
#[instrument(skip(ctx))]
pub fn init(#[context] ctx: SdkContext) -> Result<(), Error> {
    if !ctx.developer_key_path.exists() {
        let parent = ctx.developer_key_path.parent().unwrap_or(Path::new("/"));
        if !parent.exists() {
            std::fs::create_dir_all(parent)
                .with_ctx(|_| (crate::ErrorKind::Filesystem, parent.display().to_string()))?;
        }
        tracing::info!("Generating new developer key...");
        let keypair = Keypair::generate(&mut rand_old::thread_rng());
        tracing::info!("Writing key to {}", ctx.developer_key_path.display());
        let keypair_bytes = ed25519::KeypairBytes {
            secret_key: keypair.secret.to_bytes(),
            public_key: Some(keypair.public.to_bytes()),
        };
        let mut dev_key_file = File::create(&ctx.developer_key_path)?;
        dev_key_file.write_all(
            keypair_bytes
                .to_pkcs8_pem(base64ct::LineEnding::default())
                .with_kind(crate::ErrorKind::Pem)?
                .as_bytes(),
        )?;
        dev_key_file.sync_all()?;
    }
    Ok(())
}

#[command(subcommands(crate::s9pk::verify, crate::config::verify_spec))]
pub fn verify() -> Result<(), Error> {
    Ok(())
}
