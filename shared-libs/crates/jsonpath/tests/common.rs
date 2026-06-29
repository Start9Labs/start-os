extern crate env_logger;
extern crate imbl_value;
extern crate jsonpath_lib as jsonpath;
extern crate serde_json;

use std::io::Read;

use imbl_value::imbl::Vector;
use imbl_value::Value;

use self::jsonpath::{JsonSelector, PathParser};

#[allow(dead_code)]
pub fn setup() {
    let _ = env_logger::try_init();
}

#[allow(dead_code)]
pub fn read_json(path: &str) -> Value {
    let mut f = std::fs::File::open(path).unwrap();
    let mut contents = String::new();
    f.read_to_string(&mut contents).unwrap();
    serde_json::from_str(&contents).unwrap()
}

#[allow(dead_code)]
pub fn read_contents(path: &str) -> String {
    let mut f = std::fs::File::open(path).unwrap();
    let mut contents = String::new();
    f.read_to_string(&mut contents).unwrap();
    contents
}

#[allow(dead_code)]
pub fn select_and_then_compare(path: &str, json: Value, target: Value) {
    let parser = PathParser::compile(path).unwrap();
    let mut selector = JsonSelector::new(parser);
    let result: Vector<_> = selector
        .value(&json)
        .select()
        .unwrap()
        .into_iter()
        .cloned()
        .collect();
    assert_eq!(
        result,
        match target {
            Value::Array(vec) => vec,
            _ => panic!("Give me the Array!"),
        },
        "{}",
        path
    );
}

#[allow(dead_code)]
pub fn compare_result(result: Vector<&Value>, target: Value) {
    let result = imbl_value::to_value(&result).unwrap();
    assert_eq!(result, target);
}
