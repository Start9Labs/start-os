use serde_json::Value;

pub const STATIC_NULL: Value = Value::Null;

pub trait MergeWith {
    fn merge_with(&mut self, other: &serde_json::Value);
}

impl MergeWith for serde_json::Value {
    fn merge_with(&mut self, other: &serde_json::Value) {
        use serde_json::Value::Object;
        if let (Object(orig), Object(ref other)) = (self, other) {
            for (key, val) in other.into_iter() {
                match (orig.get_mut(key), val) {
                    (Some(new_orig @ Object(_)), other @ Object(_)) => {
                        new_orig.merge_with(other);
                    }
                    (None, _) => {
                        orig.insert(key.clone(), val.clone());
                    }
                    _ => (),
                }
            }
        }
    }
}

#[test]
fn merge_with_tests() {
    use serde_json::json;

    let mut a = json!(
        {"a": 1, "c": {"d": "123"}, "i": [1,2,3], "j": {}, "k":[1,2,3], "l": "test"}
    );
    a.merge_with(
        &json!({"a":"a", "b": "b", "c":{"d":"d", "e":"e"}, "f":{"g":"g"}, "h": [1,2,3], "i":"i", "j":[1,2,3], "k":{}}),
    );
    assert_eq!(
        a,
        json!({"a": 1, "c": {"d": "123", "e":"e"}, "b":"b", "f": {"g":"g"}, "h":[1,2,3], "i":[1,2,3], "j": {}, "k":[1,2,3], "l": "test"})
    )
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
