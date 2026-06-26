use std::borrow::Cow;
use std::sync::Arc;

use imbl::Vector;
use yasi::InternedString;

use crate::{InOMap, Value};

impl From<serde_json::Value> for Value {
    fn from(value: serde_json::Value) -> Self {
        match value {
            serde_json::Value::Array(a) => Self::Array(a.into_iter().map(|a| a.into()).collect()),
            serde_json::Value::Bool(a) => Self::Bool(a),
            serde_json::Value::Null => Self::Null,
            serde_json::Value::Number(a) => Self::Number(a),
            serde_json::Value::Object(a) => {
                Self::Object(a.into_iter().map(|(a, b)| (a.into(), b.into())).collect())
            }
            serde_json::Value::String(a) => Self::String(a.into()),
        }
    }
}

impl From<Value> for serde_json::Value {
    fn from(value: Value) -> Self {
        match value {
            Value::Array(a) => Self::Array(a.into_iter().map(|a| a.into()).collect()),
            Value::Bool(a) => Self::Bool(a),
            Value::Null => Self::Null,
            Value::Number(a) => Self::Number(a),
            Value::Object(a) => Self::Object(
                a.into_iter()
                    .map(|(a, b)| ((&*a).to_owned(), b.into()))
                    .collect(),
            ),
            Value::String(a) => Self::String((&*a).to_owned()),
        }
    }
}

impl<T: Clone + Into<Value>> From<&[T]> for Value {
    /// Convert a slice to `Value::Array`.
    fn from(value: &[T]) -> Self {
        Self::Array(value.into_iter().map(|t| t.clone().into()).collect())
    }
}

impl From<&str> for Value {
    /// Convert string slice to `Value::String`.
    fn from(value: &str) -> Self {
        Value::String(Arc::new(value.into()))
    }
}

impl<T: Into<Value>, const N: usize> From<[T; N]> for Value {
    /// Convert an array to `Value::Array`.
    fn from(value: [T; N]) -> Self {
        Self::Array(value.into_iter().map(|t| t.into()).collect())
    }
}

impl From<()> for Value {
    /// Convert `()` to `Value::Null`.
    fn from(_: ()) -> Self {
        Value::Null
    }
}

impl<'a> From<Cow<'a, str>> for Value {
    /// Convert copy-on-write string to `Value::String`.
    fn from(value: Cow<'a, str>) -> Self {
        Value::String(Arc::new(value.into_owned()))
    }
}

impl From<serde_json::Map<String, serde_json::Value>> for InOMap<InternedString, Value> {
    fn from(value: serde_json::Map<String, serde_json::Value>) -> Self {
        value
            .into_iter()
            .map(|(k, v)| (InternedString::intern(k), v.into()))
            .collect()
    }
}

impl From<InOMap<InternedString, Value>> for Value {
    fn from(value: InOMap<InternedString, Value>) -> Self {
        Self::Object(value)
    }
}

impl From<serde_json::Map<String, serde_json::Value>> for Value {
    fn from(value: serde_json::Map<String, serde_json::Value>) -> Self {
        Self::Object(value.into())
    }
}

impl From<serde_json::Number> for Value {
    /// Convert `serde_json::Number` to `Value::Number`.
    fn from(value: serde_json::Number) -> Self {
        Self::Number(value)
    }
}

impl<T: Into<Value>> From<Option<T>> for Value {
    /// Convert `Option` to `Value::Null` if none, or a corresponding `Value` if some
    fn from(value: Option<T>) -> Self {
        match value {
            None => Value::Null,
            Some(a) => a.into(),
        }
    }
}

impl From<String> for Value {
    /// Convert `String` to `Value::String`.
    fn from(value: String) -> Self {
        Value::String(Arc::new(value))
    }
}

impl From<Arc<String>> for Value {
    /// Convert `Arc<String>` to `Value::String`.
    fn from(value: Arc<String>) -> Self {
        Value::String(value)
    }
}

impl<T: Into<Value>> From<Vec<T>> for Value {
    /// Convert a `Vec` to `Value::Array`.
    fn from(value: Vec<T>) -> Self {
        Value::Array(value.into_iter().map(|v| v.into()).collect())
    }
}

impl<T: Clone + Into<Value>> From<Vector<T>> for Value {
    /// Convert a `Vector` to `Value::Array`.
    fn from(value: Vector<T>) -> Self {
        Value::Array(value.into_iter().map(|v| v.into()).collect())
    }
}

impl From<bool> for Value {
    /// Convert a `boolean` to `Value::Bool`.
    fn from(value: bool) -> Self {
        Value::Bool(value)
    }
}

impl From<f32> for Value {
    /// Convert 32-bit floating point number to `Value::Number`, or `Value::Null` if infinite or NaN.
    fn from(value: f32) -> Self {
        serde_json::Number::from_f64(value as f64).into()
    }
}
impl From<f64> for Value {
    /// Convert 64-bit floating point number to `Value::Number`, or `Value::Null` if infinite or NaN.
    fn from(value: f64) -> Self {
        serde_json::Number::from_f64(value).into()
    }
}
impl From<i8> for Value {
    fn from(value: i8) -> Self {
        Self::Number(value.into())
    }
}
impl From<i16> for Value {
    fn from(value: i16) -> Self {
        Self::Number(value.into())
    }
}
impl From<i32> for Value {
    fn from(value: i32) -> Self {
        Self::Number(value.into())
    }
}
impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Self::Number(value.into())
    }
}
impl From<isize> for Value {
    fn from(value: isize) -> Self {
        Self::Number(value.into())
    }
}
impl From<u8> for Value {
    fn from(value: u8) -> Self {
        Self::Number(value.into())
    }
}
impl From<u16> for Value {
    fn from(value: u16) -> Self {
        Self::Number(value.into())
    }
}
impl From<u32> for Value {
    fn from(value: u32) -> Self {
        Self::Number(value.into())
    }
}
impl From<u64> for Value {
    fn from(value: u64) -> Self {
        Self::Number(value.into())
    }
}
impl From<usize> for Value {
    fn from(value: usize) -> Self {
        Self::Number(value.into())
    }
}
