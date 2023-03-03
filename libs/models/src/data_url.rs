use std::borrow::Cow;
use std::path::Path;

use base64::Engine;
use color_eyre::eyre::eyre;
use reqwest::header::CONTENT_TYPE;
use serde::{Deserialize, Serialize};
use tokio::io::AsyncReadExt;
use yasi::InternedString;

use crate::{mime, Error, ResultExt};

pub const DEFAULT_MIME: &'static str = "application/octet-stream";

pub struct DataUrl {
    mime: InternedString,
    data: Cow<'static, [u8]>,
}
impl DataUrl {
    // data:{mime};base64,{data}
    pub fn to_string(&self) -> String {
        use std::fmt::Write;
        let mut res = String::with_capacity(self.data_url_len_without_mime() + self.mime.len());
        let _ = write!(res, "data:{};base64,", self.mime);
        base64::engine::general_purpose::URL_SAFE.encode_string(&self.data, &mut res);
        res
    }

    fn data_url_len_without_mime(&self) -> usize {
        5 + 8 + (4 * self.data.len() / 3) + 3
    }

    pub fn data_url_len(&self) -> usize {
        self.data_url_len_without_mime() + self.mime.len()
    }

    pub async fn from_path(path: impl AsRef<Path>) -> Result<Self, Error> {
        let path = path.as_ref();
        let mut f = tokio::fs::File::open(path).await?;
        let m = f.metadata().await?;
        if m.len() > 100 * 1024 {
            return Err(Error::new(
                eyre!("Data URLs must be smaller than 100KiB"),
                crate::ErrorKind::Filesystem,
            ));
        }
        let mut buf = Vec::with_capacity(m.len() as usize);
        f.read_to_end(&mut buf).await?;
        let mime = path
            .extension()
            .and_then(|s| s.to_str())
            .and_then(mime)
            .unwrap_or(DEFAULT_MIME);
        Ok(Self {
            mime: InternedString::intern(mime),
            data: Cow::Owned(buf),
        })
    }

    pub fn from_const(mime: &str, data: &'static [u8]) -> Self {
        Self {
            mime: InternedString::intern(mime),
            data: Cow::Borrowed(data),
        }
    }

    pub async fn from_response(res: reqwest::Response) -> Result<Self, Error> {
        let mime = InternedString::intern(
            res.headers()
                .get(CONTENT_TYPE)
                .and_then(|h| h.to_str().ok())
                .unwrap_or(DEFAULT_MIME),
        );
        let data = res
            .bytes()
            .await
            .with_kind(crate::ErrorKind::Network)?
            .to_vec();
        Ok(Self {
            mime,
            data: Cow::Owned(data),
        })
    }
}

impl std::fmt::Debug for DataUrl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for DataUrl {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct Visitor;
        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = DataUrl;
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(formatter, "a valid base64 data url")
            }
            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                v.strip_prefix("data:")
                    .and_then(|v| v.split_once(";base64"))
                    .and_then(|(mime, data)| {
                        Some(DataUrl {
                            mime: InternedString::intern(mime),
                            data: Cow::Owned(
                                base64::engine::general_purpose::URL_SAFE
                                    .decode(data)
                                    .ok()?,
                            ),
                        })
                    })
                    .ok_or_else(|| {
                        E::invalid_value(serde::de::Unexpected::Str(v), &"a valid base64 data url")
                    })
            }
        }
        deserializer.deserialize_str(Visitor)
    }
}

impl Serialize for DataUrl {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

#[test]
fn doesnt_reallocate() {
    let random: [u8; 10] = rand::random();
    for i in 0..10 {
        let icon = Icon {
            format: InternedString::intern("png"),
            data: random[..i].to_vec(),
        };
        assert_eq!(dbg!(icon.data_url()).capacity(), icon.data_url_len());
    }
}
