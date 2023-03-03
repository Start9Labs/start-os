use base64::Engine;
use serde::{Deserialize, Serialize};
use yasi::InternedString;

pub struct DataUrl {
    mime: InternedString,
    data: Vec<u8>,
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
                            data: base64::engine::general_purpose::URL_SAFE
                                .decode(data)
                                .ok()?,
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
