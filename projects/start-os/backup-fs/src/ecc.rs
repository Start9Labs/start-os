//! Reed-Solomon erasure coding over an opaque byte payload.
//!
//! Used by [`crate::vault`] to make every on-disk object (inode metadata,
//! content blocks, the inode pool, the crypt header) self-healing: the
//! payload is split into `data` shards and `parity` extra shards are
//! computed, so the original payload can be reconstructed as long as no
//! more than `parity` shards are lost or corrupt.
//!
//! Reed-Solomon natively corrects *erasures* (shards known to be missing),
//! not silent bit-flips. The vault layer turns silent corruption into
//! erasures by checksumming each shard independently and treating any
//! shard whose checksum fails as missing before calling [`decode`].

use reed_solomon_erasure::galois_8::ReedSolomon;

/// The Galois field is GF(2^8), so the total shard count can't exceed 255.
pub const MAX_SHARDS: usize = 255;

#[derive(Debug, Clone, Copy)]
pub struct EccError;

/// Length of each equal-sized shard for a payload of `payload_len` bytes
/// split across `data` data shards. At least one byte so empty payloads
/// still produce well-formed (all-zero) shards.
pub fn shard_len(payload_len: usize, data: usize) -> usize {
    payload_len.div_ceil(data).max(1)
}

fn validate(data: usize, parity: usize) -> Result<(), EccError> {
    if data == 0 || parity == 0 || data + parity > MAX_SHARDS {
        return Err(EccError);
    }
    Ok(())
}

/// Split `payload` into `data` equal shards (zero-padded) and append
/// `parity` computed parity shards. Returns `data + parity` shards, each
/// of identical length.
pub fn encode(payload: &[u8], data: usize, parity: usize) -> Result<Vec<Vec<u8>>, EccError> {
    validate(data, parity)?;
    let slen = shard_len(payload.len(), data);
    let mut shards: Vec<Vec<u8>> = Vec::with_capacity(data + parity);
    for i in 0..data {
        let start = (i * slen).min(payload.len());
        let end = (start + slen).min(payload.len());
        let mut shard = vec![0u8; slen];
        shard[..end - start].copy_from_slice(&payload[start..end]);
        shards.push(shard);
    }
    for _ in 0..parity {
        shards.push(vec![0u8; slen]);
    }
    let rs = ReedSolomon::new(data, parity).map_err(|_| EccError)?;
    rs.encode(&mut shards).map_err(|_| EccError)?;
    Ok(shards)
}

/// Reconstruct the original payload from `shards`, where `None` marks a
/// shard that is missing or failed its integrity check. Succeeds as long
/// as at least `data` shards are present. The returned buffer is truncated
/// to `payload_len`.
pub fn decode(
    mut shards: Vec<Option<Vec<u8>>>,
    data: usize,
    parity: usize,
    payload_len: usize,
) -> Result<Vec<u8>, EccError> {
    validate(data, parity)?;
    if shards.len() != data + parity {
        return Err(EccError);
    }
    let rs = ReedSolomon::new(data, parity).map_err(|_| EccError)?;
    rs.reconstruct(&mut shards).map_err(|_| EccError)?;
    let mut out = Vec::with_capacity(payload_len);
    for shard in shards.into_iter().take(data) {
        out.extend_from_slice(&shard.ok_or(EccError)?);
    }
    out.truncate(payload_len);
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn roundtrip_no_loss() {
        let payload = b"the quick brown fox jumps over the lazy dog".to_vec();
        let shards = encode(&payload, 4, 2).unwrap();
        assert_eq!(shards.len(), 6);
        let opts = shards.into_iter().map(Some).collect();
        let out = decode(opts, 4, 2, payload.len()).unwrap();
        assert_eq!(out, payload);
    }

    #[test]
    fn recovers_up_to_parity_losses() {
        let payload: Vec<u8> = (0..1000u32).map(|i| (i % 251) as u8).collect();
        let shards = encode(&payload, 6, 3).unwrap();
        // Drop 3 shards (== parity): still recoverable.
        let mut opts: Vec<Option<Vec<u8>>> = shards.into_iter().map(Some).collect();
        opts[0] = None;
        opts[4] = None;
        opts[7] = None;
        let out = decode(opts, 6, 3, payload.len()).unwrap();
        assert_eq!(out, payload);
    }

    #[test]
    fn fails_beyond_parity() {
        let payload: Vec<u8> = (0..500u32).map(|i| i as u8).collect();
        let shards = encode(&payload, 4, 2).unwrap();
        let mut opts: Vec<Option<Vec<u8>>> = shards.into_iter().map(Some).collect();
        // Drop 3 shards with only 2 parity — unrecoverable.
        opts[0] = None;
        opts[1] = None;
        opts[2] = None;
        assert!(decode(opts, 4, 2, payload.len()).is_err());
    }

    #[test]
    fn empty_payload() {
        let shards = encode(&[], 3, 2).unwrap();
        let opts = shards.into_iter().map(Some).collect();
        let out = decode(opts, 3, 2, 0).unwrap();
        assert!(out.is_empty());
    }
}
