use std::borrow::Cow;
use std::path::Path;

use base64::Engine;
use color_eyre::eyre::eyre;
use reqwest::header::CONTENT_TYPE;
use serde::{Deserialize, Serialize};
use tokio::io::{AsyncRead, AsyncReadExt};
use yasi::InternedString;

use crate::{mime, Error, ErrorKind, ResultExt};

#[derive(Clone)]
pub struct DataUrl<'a> {
    mime: InternedString,
    data: Cow<'a, [u8]>,
}
impl<'a> DataUrl<'a> {
    pub const DEFAULT_MIME: &'static str = "application/octet-stream";
    pub const MAX_SIZE: u64 = 100 * 1024;

    // data:{mime};base64,{data}
    pub fn to_string(&self) -> String {
        use std::fmt::Write;
        let mut res = String::with_capacity(self.data_url_len_without_mime() + self.mime.len());
        let _ = write!(res, "data:{};base64,", self.mime);
        base64::engine::general_purpose::STANDARD.encode_string(&self.data, &mut res);
        res
    }

    fn data_url_len_without_mime(&self) -> usize {
        5 + 8 + (4 * self.data.len() / 3) + 3
    }

    pub fn data_url_len(&self) -> usize {
        self.data_url_len_without_mime() + self.mime.len()
    }

    pub fn from_slice(mime: &str, data: &'a [u8]) -> Self {
        Self {
            mime: InternedString::intern(mime),
            data: Cow::Borrowed(data),
        }
    }
}
impl DataUrl<'static> {
    pub async fn from_reader(
        mime: &str,
        rdr: impl AsyncRead + Unpin,
        size_est: Option<u64>,
    ) -> Result<Self, Error> {
        let check_size = |s| {
            if s > Self::MAX_SIZE {
                Err(Error::new(
                    eyre!("Data URLs must be smaller than 100KiB"),
                    ErrorKind::Filesystem,
                ))
            } else {
                Ok(s)
            }
        };
        let mut buf = size_est
            .map(check_size)
            .transpose()?
            .map(|s| Vec::with_capacity(s as usize))
            .unwrap_or_default();
        rdr.take(Self::MAX_SIZE + 1).read_to_end(&mut buf).await?;
        check_size(buf.len() as u64)?;

        Ok(Self {
            mime: InternedString::intern(mime),
            data: Cow::Owned(buf),
        })
    }

    pub async fn from_path(path: impl AsRef<Path>) -> Result<Self, Error> {
        let path = path.as_ref();
        let f = tokio::fs::File::open(path).await?;
        let m = f.metadata().await?;
        let mime = path
            .extension()
            .and_then(|s| s.to_str())
            .and_then(mime)
            .unwrap_or(Self::DEFAULT_MIME);
        Self::from_reader(mime, f, Some(m.len())).await
    }

    pub async fn from_response(res: reqwest::Response) -> Result<Self, Error> {
        let mime = InternedString::intern(
            res.headers()
                .get(CONTENT_TYPE)
                .and_then(|h| h.to_str().ok())
                .unwrap_or(Self::DEFAULT_MIME),
        );
        let data = res.bytes().await.with_kind(ErrorKind::Network)?.to_vec();
        Ok(Self {
            mime,
            data: Cow::Owned(data),
        })
    }

    pub fn from_vec(mime: &str, data: Vec<u8>) -> Self {
        Self {
            mime: InternedString::intern(mime),
            data: Cow::Owned(data),
        }
    }
}

impl<'a> std::fmt::Debug for DataUrl<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for DataUrl<'static> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct Visitor;
        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = DataUrl<'static>;
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(formatter, "a valid base64 data url")
            }
            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                v.strip_prefix("data:")
                    .and_then(|v| v.split_once(";base64,"))
                    .and_then(|(mime, data)| {
                        Some(DataUrl {
                            mime: InternedString::intern(mime),
                            data: Cow::Owned(
                                base64::engine::general_purpose::STANDARD
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
        deserializer.deserialize_any(Visitor)
    }
}

impl<'a> Serialize for DataUrl<'a> {
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
        let icon = DataUrl {
            mime: InternedString::intern("png"),
            data: Cow::Borrowed(&random[..i]),
        };
        assert_eq!(dbg!(icon.to_string()).capacity(), icon.data_url_len());
    }
}
