//! Compatibility with clients and registries that label Ed25519ph signatures `id-Ed25519`.

use crate::prelude::*;
use crate::sign::commitment::SignedCommitment;
use crate::sign::ed25519::Ed25519ph;
use crate::sign::{AnySignature, AnyVerifyingKey, SignatureScheme};

/// Accepts an `id-Ed25519` signature as Ed25519ph when it fails as pure Ed25519.
pub fn verify_commitment<C: SignedCommitment>(
    key: &AnyVerifyingKey,
    commitment: &C,
    context: &str,
    signature: &AnySignature,
) -> Result<(), Error> {
    key.verify_commitment(commitment, context, signature)
        .or_else(|e| match (key, signature) {
            (AnyVerifyingKey::Ed25519(k), AnySignature::Ed25519(s)) => Ed25519ph
                .verify_commitment(k, commitment, context, s)
                .map_err(|_| e),
            _ => Err(e),
        })
}

/// Relabels stored `id-Ed25519` registry signatures as Ed25519ph.
pub fn relabel_legacy_signatures(value: &mut Value) {
    map_signatures(value, &mut |sig| match sig.as_str()?.parse() {
        Ok(AnySignature::Ed25519(s)) => {
            Some(Value::String(AnySignature::Ed25519ph(s).to_string().into()))
        }
        _ => Some(sig.clone()),
    });
}

/// Rewrites a response for a client that reads `id-Ed25519` as Ed25519ph.
/// Drops every signature such a client cannot verify.
pub fn downgrade_signatures(value: &mut Value) {
    map_signatures(value, &mut |sig| match sig.as_str()?.parse() {
        Ok(AnySignature::Ed25519ph(s)) => {
            Some(Value::String(AnySignature::Ed25519(s).to_string().into()))
        }
        _ => None,
    });
}

/// Applies `f` to each entry of every `signatures` map; `None` removes the entry.
fn map_signatures(value: &mut Value, f: &mut impl FnMut(&Value) -> Option<Value>) {
    match value {
        Value::Object(obj) => {
            for (k, v) in obj.iter_mut() {
                match (&**k, v) {
                    ("signatures", Value::Object(sigs)) => {
                        *sigs = sigs
                            .iter()
                            .filter_map(|(k, sig)| Some((k.clone(), f(sig)?)))
                            .collect();
                    }
                    (_, v) => map_signatures(v, f),
                }
            }
        }
        Value::Array(arr) => {
            for v in arr.iter_mut() {
                map_signatures(v, f);
            }
        }
        _ => (),
    }
}

#[cfg(test)]
mod tests {
    use imbl_value::json;

    use super::*;
    use crate::sign::ed25519::Ed25519ph;

    fn key() -> ed25519_dalek::SigningKey {
        ed25519_dalek::SigningKey::from_bytes(&[5; 32])
    }

    fn ph_signature() -> ed25519_dalek::Signature {
        let commitment = crate::sign::commitment::blake3::Blake3Commitment {
            hash: crate::util::serde::Base64([1; 32]),
            size: 7,
        };
        Ed25519ph
            .sign_commitment(&key(), &commitment, "startos")
            .unwrap()
    }

    fn asset(sig: &AnySignature) -> Value {
        json!({ "x86_64": { "commitment": {}, "signatures": { "signer": sig.to_string() } } })
    }

    #[test]
    fn legacy_db_signatures_become_ed25519ph() {
        let s = ph_signature();
        let mut db = json!({ "index": { "os": { "versions": { "0.4.0.2": { "squashfs": asset(&AnySignature::Ed25519(s)) } } } } });
        relabel_legacy_signatures(&mut db);
        assert_eq!(
            db["index"]["os"]["versions"]["0.4.0.2"]["squashfs"],
            asset(&AnySignature::Ed25519ph(s))
        );
    }

    #[test]
    fn downgrade_relabels_ed25519ph_and_drops_pure() {
        let s = ph_signature();
        let mut res = json!([[{}, {
            "signatures": {
                "file": AnySignature::Ed25519ph(s).to_string(),
                "agent": AnySignature::Ed25519(s).to_string(),
            }
        }]]);
        downgrade_signatures(&mut res);
        assert_eq!(
            res,
            json!([[{}, { "signatures": { "file": AnySignature::Ed25519(s).to_string() } }]])
        );
    }

    #[test]
    fn legacy_labelled_ed25519ph_verifies() {
        let commitment = crate::sign::commitment::blake3::Blake3Commitment {
            hash: crate::util::serde::Base64([1; 32]),
            size: 7,
        };
        let key = AnyVerifyingKey::Ed25519(key().verifying_key());
        let legacy = AnySignature::Ed25519(ph_signature());
        assert!(
            key.verify_commitment(&commitment, "startos", &legacy)
                .is_err()
        );
        verify_commitment(&key, &commitment, "startos", &legacy).unwrap();
    }
}
