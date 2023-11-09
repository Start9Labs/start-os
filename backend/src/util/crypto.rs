use ed25519_dalek::{SecretKey, EXPANDED_SECRET_KEY_LENGTH};

#[inline]
pub fn ed25519_expand_key(key: &SecretKey) -> [u8; EXPANDED_SECRET_KEY_LENGTH] {
    ed25519_dalek_v1::ExpandedSecretKey::from(
        &ed25519_dalek_v1::SecretKey::from_bytes(key).unwrap(),
    )
    .to_bytes()
}
