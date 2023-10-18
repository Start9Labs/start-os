use ed25519_dalek::hazmat::ExpandedSecretKey;
use ed25519_dalek::{SecretKey, EXPANDED_SECRET_KEY_LENGTH};

#[inline]
pub fn ed25519_expand_key(key: &SecretKey) -> [u8; EXPANDED_SECRET_KEY_LENGTH] {
    let key = ExpandedSecretKey::from(key);

    let mut bytes: [u8; 64] = [0u8; 64];

    bytes[..32].copy_from_slice(key.scalar.as_bytes());
    bytes[32..].copy_from_slice(&key.hash_prefix[..]);
    bytes
}
