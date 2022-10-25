use aes::cipher::{CipherKey, NewCipher, Nonce, StreamCipher};
use aes::Aes256Ctr;
use hmac::Hmac;
use josekit::jwk::Jwk;
use serde::{Deserialize, Serialize};
use sha2::Sha256;
use tracing::instrument;

pub fn pbkdf2(password: impl AsRef<[u8]>, salt: impl AsRef<[u8]>) -> CipherKey<Aes256Ctr> {
    let mut aeskey = CipherKey::<Aes256Ctr>::default();
    pbkdf2::pbkdf2::<Hmac<Sha256>>(
        password.as_ref(),
        salt.as_ref(),
        1000,
        aeskey.as_mut_slice(),
    );
    aeskey
}

pub fn encrypt_slice(input: impl AsRef<[u8]>, password: impl AsRef<[u8]>) -> Vec<u8> {
    let prefix: [u8; 32] = rand::random();
    let aeskey = pbkdf2(password.as_ref(), &prefix[16..]);
    let ctr = Nonce::<Aes256Ctr>::from_slice(&prefix[..16]);
    let mut aes = Aes256Ctr::new(&aeskey, ctr);
    let mut res = Vec::with_capacity(32 + input.as_ref().len());
    res.extend_from_slice(&prefix[..]);
    res.extend_from_slice(input.as_ref());
    aes.apply_keystream(&mut res[32..]);
    res
}

pub fn decrypt_slice(input: impl AsRef<[u8]>, password: impl AsRef<[u8]>) -> Vec<u8> {
    if input.as_ref().len() < 32 {
        return Vec::new();
    }
    let (prefix, rest) = input.as_ref().split_at(32);
    let aeskey = pbkdf2(password.as_ref(), &prefix[16..]);
    let ctr = Nonce::<Aes256Ctr>::from_slice(&prefix[..16]);
    let mut aes = Aes256Ctr::new(&aeskey, ctr);
    let mut res = rest.to_vec();
    aes.apply_keystream(&mut res);
    res
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct EncryptedWire {
    encrypted: serde_json::Value,
}
impl EncryptedWire {
    #[instrument(skip(current_secret))]
    pub fn decrypt(self, current_secret: impl AsRef<Jwk>) -> Option<String> {
        let current_secret = current_secret.as_ref();

        let decrypter = match josekit::jwe::alg::ecdh_es::EcdhEsJweAlgorithm::EcdhEs
            .decrypter_from_jwk(current_secret)
        {
            Ok(a) => a,
            Err(e) => {
                tracing::warn!("Could not setup awk");
                tracing::debug!("{:?}", e);
                return None;
            }
        };
        let encrypted = match serde_json::to_string(&self.encrypted) {
            Ok(a) => a,
            Err(e) => {
                tracing::warn!("Could not deserialize");
                tracing::debug!("{:?}", e);

                return None;
            }
        };
        let (decoded, _) = match josekit::jwe::deserialize_json(&encrypted, &decrypter) {
            Ok(a) => a,
            Err(e) => {
                tracing::warn!("Could not decrypt");
                tracing::debug!("{:?}", e);
                return None;
            }
        };
        match String::from_utf8(decoded) {
            Ok(a) => Some(a),
            Err(e) => {
                tracing::warn!("Could not decrypt into utf8");
                tracing::debug!("{:?}", e);
                return None;
            }
        }
    }
}

/// We created this test by first making the private key, then restoring from this private key for recreatability.
/// After this the frontend then encoded an password, then we are testing that the output that we got (hand coded)
/// will be the shape we want.
#[test]
fn test_gen_awk() {
    let private_key: Jwk = serde_json::from_str(
        r#"{
            "kty": "EC",
            "crv": "P-256",
            "d": "3P-MxbUJtEhdGGpBCRFXkUneGgdyz_DGZWfIAGSCHOU",
            "x": "yHTDYSfjU809fkSv9MmN4wuojf5c3cnD7ZDN13n-jz4",
            "y": "8Mpkn744A5KDag0DmX2YivB63srjbugYZzWc3JOpQXI"
          }"#,
    )
    .unwrap();
    let encrypted: EncryptedWire =  serde_json::from_str(r#"{
        "encrypted":     {        "protected": "eyJlbmMiOiJBMTI4Q0JDLUhTMjU2IiwiYWxnIjoiRUNESC1FUyIsImtpZCI6ImgtZnNXUVh2Tm95dmJEazM5dUNsQ0NUdWc5N3MyZnJockJnWUVBUWVtclUiLCJlcGsiOnsia3R5IjoiRUMiLCJjcnYiOiJQLTI1NiIsIngiOiJmRkF0LXNWYWU2aGNkdWZJeUlmVVdUd3ZvWExaTkdKRHZIWVhIckxwOXNNIiwieSI6IjFvVFN6b00teHlFZC1SLUlBaUFHdXgzS1dJZmNYZHRMQ0JHLUh6MVkzY2sifX0",        "iv": "NbwvfvWOdLpZfYRIZUrkcw",        "ciphertext": "Zc5Br5kYOlhPkIjQKOLMJw",        "tag": "EPoch52lDuCsbUUulzZGfg"    }
      }"#).unwrap();
    assert_eq!(
        "testing12345",
        &encrypted.decrypt(std::sync::Arc::new(private_key)).unwrap()
    );
}
