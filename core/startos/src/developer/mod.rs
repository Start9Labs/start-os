use std::fs::File;
use std::io::Write;
use std::path::Path;

use ed25519::pkcs8::EncodePrivateKey;
use ed25519::PublicKeyBytes;
use ed25519_dalek::{SigningKey, VerifyingKey};
use rpc_toolkit::{from_fn_async, HandlerExt, ParentHandler};
use tracing::instrument;

use crate::context::CliContext;
use crate::{Error, ResultExt};

#[instrument(skip_all)]
pub fn init(ctx: CliContext) -> Result<(), Error> {
    if !ctx.developer_key_path.exists() {
        let parent = ctx.developer_key_path.parent().unwrap_or(Path::new("/"));
        if !parent.exists() {
            std::fs::create_dir_all(parent)
                .with_ctx(|_| (crate::ErrorKind::Filesystem, parent.display().to_string()))?;
        }
        tracing::info!("Generating new developer key...");
        let secret = SigningKey::generate(&mut rand::thread_rng());
        tracing::info!("Writing key to {}", ctx.developer_key_path.display());
        let keypair_bytes = ed25519::KeypairBytes {
            secret_key: secret.to_bytes(),
            public_key: Some(PublicKeyBytes(VerifyingKey::from(&secret).to_bytes())),
        };
        let mut dev_key_file = File::create(&ctx.developer_key_path)?;
        dev_key_file.write_all(
            keypair_bytes
                .to_pkcs8_pem(base64ct::LineEnding::default())
                .with_kind(crate::ErrorKind::Pem)?
                .as_bytes(),
        )?;
        dev_key_file.sync_all()?;
        println!(
            "New developer key generated at {}",
            ctx.developer_key_path.display()
        );
    } else {
        println!(
            "Developer key already exists at {}",
            ctx.developer_key_path.display()
        );
    }
    Ok(())
}

pub fn verify() -> ParentHandler {
    ParentHandler::new().subcommand("verify", from_fn_async(crate::s9pk::verify).no_display())
}
