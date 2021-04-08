use std::ops::{Bound, RangeBounds, RangeInclusive};

use rand::distributions::Distribution;
use rand::Rng;
use serde_json::Value;

use super::Config;

pub const STATIC_NULL: Value = Value::Null;

#[derive(Clone, Debug)]
pub struct CharSet(pub Vec<(RangeInclusive<char>, usize)>, usize);
impl CharSet {
    pub fn contains(&self, c: &char) -> bool {
        self.0.iter().any(|r| r.0.contains(c))
    }
    pub fn gen<R: Rng>(&self, rng: &mut R) -> char {
        let mut idx = rng.gen_range(0..self.1);
        for r in &self.0 {
            if idx < r.1 {
                return std::convert::TryFrom::try_from(
                    rand::distributions::Uniform::new_inclusive(
                        u32::from(*r.0.start()),
                        u32::from(*r.0.end()),
                    )
                    .sample(rng),
                )
                .unwrap();
            } else {
                idx -= r.1;
            }
        }
        unreachable!()
    }
}
impl Default for CharSet {
    fn default() -> Self {
        CharSet(vec![('!'..='~', 94)], 94)
    }
}
impl<'de> serde::de::Deserialize<'de> for CharSet {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        let mut res = Vec::new();
        let mut len = 0;
        let mut a: Option<char> = None;
        let mut b: Option<char> = None;
        let mut in_range = false;
        for c in s.chars() {
            match c {
                ',' => match (a, b, in_range) {
                    (Some(start), Some(end), _) => {
                        if !end.is_ascii() {
                            return Err(serde::de::Error::custom("Invalid Character"));
                        }
                        if start >= end {
                            return Err(serde::de::Error::custom("Invalid Bounds"));
                        }
                        let l = u32::from(end) - u32::from(start) + 1;
                        res.push((start..=end, l as usize));
                        len += l as usize;
                        a = None;
                        b = None;
                        in_range = false;
                    }
                    (Some(start), None, false) => {
                        len += 1;
                        res.push((start..=start, 1));
                        a = None;
                    }
                    (Some(_), None, true) => {
                        b = Some(',');
                    }
                    (None, None, false) => {
                        a = Some(',');
                    }
                    _ => {
                        return Err(serde::de::Error::custom("Syntax Error"));
                    }
                },
                '-' => {
                    if a.is_none() {
                        a = Some('-');
                    } else if !in_range {
                        in_range = true;
                    } else if b.is_none() {
                        b = Some('-')
                    } else {
                        return Err(serde::de::Error::custom("Syntax Error"));
                    }
                }
                _ => {
                    if a.is_none() {
                        a = Some(c);
                    } else if in_range && b.is_none() {
                        b = Some(c);
                    } else {
                        return Err(serde::de::Error::custom("Syntax Error"));
                    }
                }
            }
        }
        match (a, b) {
            (Some(start), Some(end)) => {
                if !end.is_ascii() {
                    return Err(serde::de::Error::custom("Invalid Character"));
                }
                if start >= end {
                    return Err(serde::de::Error::custom("Invalid Bounds"));
                }
                let l = u32::from(end) - u32::from(start) + 1;
                res.push((start..=end, l as usize));
                len += l as usize;
            }
            (Some(c), None) => {
                len += 1;
                res.push((c..=c, 1));
            }
            _ => (),
        }

        Ok(CharSet(res, len))
    }
}
impl serde::ser::Serialize for CharSet {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::ser::Serializer,
    {
        <&str>::serialize(
            &self
                .0
                .iter()
                .map(|r| match r.1 {
                    1 => format!("{}", r.0.start()),
                    _ => format!("{}-{}", r.0.start(), r.0.end()),
                })
                .collect::<Vec<_>>()
                .join(",")
                .as_str(),
            serializer,
        )
    }
}

pub mod serde_regex {
    use regex::Regex;
    use serde::*;

    pub fn serialize<S>(regex: &Regex, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        <&str>::serialize(&regex.as_str(), serializer)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Regex, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Regex::new(&s).map_err(|e| de::Error::custom(e))
    }
}

#[derive(Clone, Debug)]
pub struct NumRange<T: std::str::FromStr + std::fmt::Display + std::cmp::PartialOrd>(
    pub (Bound<T>, Bound<T>),
);
impl<T> std::ops::Deref for NumRange<T>
where
    T: std::str::FromStr + std::fmt::Display + std::cmp::PartialOrd,
{
    type Target = (Bound<T>, Bound<T>);

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<'de, T> serde::de::Deserialize<'de> for NumRange<T>
where
    T: std::str::FromStr + std::fmt::Display + std::cmp::PartialOrd,
    <T as std::str::FromStr>::Err: std::fmt::Display,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        let mut split = s.split(",");
        let start = split
            .next()
            .map(|s| match s.get(..1) {
                Some("(") => match s.get(1..2) {
                    Some("*") => Ok(Bound::Unbounded),
                    _ => s[1..]
                        .trim()
                        .parse()
                        .map(Bound::Excluded)
                        .map_err(|e| serde::de::Error::custom(e)),
                },
                Some("[") => s[1..]
                    .trim()
                    .parse()
                    .map(Bound::Included)
                    .map_err(|e| serde::de::Error::custom(e)),
                _ => Err(serde::de::Error::custom(format!(
                    "Could not parse left bound: {}",
                    s
                ))),
            })
            .transpose()?
            .unwrap();
        let end = split
            .next()
            .map(|s| match s.get(s.len() - 1..) {
                Some(")") => match s.get(s.len() - 2..s.len() - 1) {
                    Some("*") => Ok(Bound::Unbounded),
                    _ => s[..s.len() - 1]
                        .trim()
                        .parse()
                        .map(Bound::Excluded)
                        .map_err(|e| serde::de::Error::custom(e)),
                },
                Some("]") => s[..s.len() - 1]
                    .trim()
                    .parse()
                    .map(Bound::Included)
                    .map_err(|e| serde::de::Error::custom(e)),
                _ => Err(serde::de::Error::custom(format!(
                    "Could not parse right bound: {}",
                    s
                ))),
            })
            .transpose()?
            .unwrap_or(Bound::Unbounded);

        Ok(NumRange((start, end)))
    }
}
impl<T> std::fmt::Display for NumRange<T>
where
    T: std::str::FromStr + std::fmt::Display + std::cmp::PartialOrd,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.start_bound() {
            Bound::Excluded(n) => write!(f, "({},", n)?,
            Bound::Included(n) => write!(f, "[{},", n)?,
            Bound::Unbounded => write!(f, "(*,")?,
        };
        match self.end_bound() {
            Bound::Excluded(n) => write!(f, "{})", n),
            Bound::Included(n) => write!(f, "{}]", n),
            Bound::Unbounded => write!(f, "*)"),
        }
    }
}
impl<T> serde::ser::Serialize for NumRange<T>
where
    T: std::str::FromStr + std::fmt::Display + std::cmp::PartialOrd,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::ser::Serializer,
    {
        <&str>::serialize(&format!("{}", self).as_str(), serializer)
    }
}

#[derive(Clone, Debug)]
pub enum UniqueBy {
    Any(Vec<UniqueBy>),
    All(Vec<UniqueBy>),
    Exactly(String),
    NotUnique,
}
impl UniqueBy {
    pub fn eq(&self, lhs: &Config, rhs: &Config) -> bool {
        match self {
            UniqueBy::Any(any) => any.iter().any(|u| u.eq(lhs, rhs)),
            UniqueBy::All(all) => all.iter().all(|u| u.eq(lhs, rhs)),
            UniqueBy::Exactly(key) => lhs.get(key) == rhs.get(key),
            UniqueBy::NotUnique => false,
        }
    }
}
impl Default for UniqueBy {
    fn default() -> Self {
        UniqueBy::NotUnique
    }
}
impl<'de> serde::de::Deserialize<'de> for UniqueBy {
    fn deserialize<D: serde::de::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct Visitor;
        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = UniqueBy;
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(formatter, "a key, an \"any\" object, or an \"all\" object")
            }
            fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Self::Value, E> {
                Ok(UniqueBy::Exactly(v.to_owned()))
            }
            fn visit_string<E: serde::de::Error>(self, v: String) -> Result<Self::Value, E> {
                Ok(UniqueBy::Exactly(v))
            }
            fn visit_map<A: serde::de::MapAccess<'de>>(
                self,
                mut map: A,
            ) -> Result<Self::Value, A::Error> {
                let mut variant = None;
                while let Some(key) = map.next_key()? {
                    match key {
                        "any" => {
                            return Ok(UniqueBy::Any(map.next_value()?));
                        }
                        "all" => {
                            return Ok(UniqueBy::All(map.next_value()?));
                        }
                        _ => {
                            variant = Some(key);
                        }
                    }
                }
                Err(serde::de::Error::unknown_variant(
                    variant.unwrap_or_default(),
                    &["any", "all"],
                ))
            }
            fn visit_unit<E: serde::de::Error>(self) -> Result<Self::Value, E> {
                Ok(UniqueBy::NotUnique)
            }
            fn visit_none<E: serde::de::Error>(self) -> Result<Self::Value, E> {
                Ok(UniqueBy::NotUnique)
            }
        }
        deserializer.deserialize_any(Visitor)
    }
}

impl serde::ser::Serialize for UniqueBy {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::ser::Serializer,
    {
        use serde::ser::SerializeMap;

        match self {
            UniqueBy::Any(any) => {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_key("any")?;
                map.serialize_value(any)?;
                map.end()
            }
            UniqueBy::All(all) => {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_key("all")?;
                map.serialize_value(all)?;
                map.end()
            }
            UniqueBy::Exactly(key) => serializer.serialize_str(key),
            UniqueBy::NotUnique => serializer.serialize_unit(),
        }
    }
}
