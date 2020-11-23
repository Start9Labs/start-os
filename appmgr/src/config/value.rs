use linear_map::LinearMap;

#[derive(Clone, Debug, Default, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct Config(pub LinearMap<String, Value>);

impl Config {
    pub fn merge_with(&mut self, other: Config) {
        for (key, val) in other.0.into_iter() {
            match (self.0.get_mut(&key), &val) {
                (Some(Value::Object(l_obj)), Value::Object(_)) => {
                    // gross, I know. https://github.com/rust-lang/rust/issues/45600
                    let r_obj = match val {
                        Value::Object(r_obj) => r_obj,
                        _ => unreachable!(),
                    };
                    l_obj.merge_with(r_obj)
                }
                (Some(Value::List(l_vec)), Value::List(_)) => {
                    let mut r_vec = match val {
                        Value::List(r_vec) => r_vec,
                        _ => unreachable!(),
                    };
                    l_vec.append(&mut r_vec);
                }
                _ => {
                    self.0.insert(key, val);
                }
            }
        }
    }
}

fn serialize_num<S: serde::Serializer>(num: &f64, serializer: S) -> Result<S::Ok, S::Error> {
    if *num < (1_i64 << f64::MANTISSA_DIGITS) as f64
        && *num > -(1_i64 << f64::MANTISSA_DIGITS) as f64
        && num.trunc() == *num
    {
        serializer.serialize_i64(*num as i64)
    } else {
        serializer.serialize_f64(*num)
    }
}

#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
pub enum Value {
    String(String),
    #[serde(serialize_with = "serialize_num")]
    Number(f64),
    Bool(bool),
    List(Vec<Value>),
    Object(Config),
    Null,
}
impl Value {
    pub fn type_of(&self) -> &'static str {
        match self {
            Value::String(_) => "string",
            Value::Number(_) => "number",
            Value::Bool(_) => "boolean",
            Value::List(_) => "list",
            Value::Object(_) => "object",
            Value::Null => "null",
        }
    }
}
