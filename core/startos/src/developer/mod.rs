use std::path::{Path, PathBuf};

use ed25519::PublicKeyBytes;
use ed25519::pkcs8::EncodePrivateKey;
use ed25519_dalek::{SigningKey, VerifyingKey};
use tokio::io::AsyncWriteExt;
use tracing::instrument;

use crate::context::CliContext;
use crate::context::config::local_config_path;
use crate::prelude::*;
use crate::util::io::create_file_mod;
use crate::util::serde::Pem;

pub const OS_DEVELOPER_KEY_PATH: &str = "/run/startos/developer.key.pem";

pub fn default_developer_key_path() -> PathBuf {
    local_config_path()
        .as_deref()
        .unwrap_or_else(|| Path::new(crate::context::config::CONFIG_PATH))
        .parent()
        .unwrap_or(Path::new("/"))
        .join("developer.key.pem")
}

pub async fn write_developer_key(
    secret: &ed25519_dalek::SigningKey,
    path: impl AsRef<Path>,
) -> Result<(), Error> {
    let keypair_bytes = ed25519::KeypairBytes {
        secret_key: secret.to_bytes(),
        public_key: Some(PublicKeyBytes(VerifyingKey::from(secret).to_bytes())),
    };
    let mut file = create_file_mod(path, 0o046).await?;
    file.write_all(
        keypair_bytes
            .to_pkcs8_pem(base64ct::LineEnding::default())
            .with_kind(crate::ErrorKind::Pem)?
            .as_bytes(),
    )
    .await?;
    file.sync_all().await?;
    Ok(())
}

#[instrument(skip_all)]
pub async fn init(ctx: CliContext) -> Result<(), Error> {
    if tokio::fs::metadata(OS_DEVELOPER_KEY_PATH).await.is_ok() {
        println!("Developer key already exists at {}", OS_DEVELOPER_KEY_PATH);
    } else if tokio::fs::metadata(&ctx.developer_key_path).await.is_err() {
        tracing::info!("Generating new developer key...");
        let secret = SigningKey::generate(&mut ssh_key::rand_core::OsRng::default());
        tracing::info!("Writing key to {}", ctx.developer_key_path.display());
        write_developer_key(&secret, &ctx.developer_key_path).await?;
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

pub fn pubkey(ctx: CliContext) -> Result<Pem<ed25519_dalek::VerifyingKey>, Error> {
    Ok(Pem(ctx.developer_key()?.verifying_key()))
}
