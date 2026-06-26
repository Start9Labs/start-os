#[macro_use]
extern crate imbl_value;
extern crate jsonpath_lib;
extern crate serde_json;

use std::sync::Arc;

use common::setup;
use imbl_value::imbl::vector;
use imbl_value::Value;
use jsonpath_lib::PathCompiled;

mod common;

#[test]
fn precompile_test() {
    setup();

    let json = json!({
        "foo": {"bar": "baz"}
    });

    // compile once

    let compiled = PathCompiled::compile("$.foo.bar");

    assert!(compiled.is_ok());

    let compiled = compiled.unwrap();

    // re-use

    //let result = compiled(&json).unwrap();
    let baz = Value::String(Arc::new("baz".into()));
    assert_eq!(compiled.select(&json).unwrap().clone(), vector![&baz]);
    assert_eq!(compiled.select(&json).unwrap().clone(), vector![&baz]);
}

#[test]
fn precompile_failure() {
    setup();

    let compiled = PathCompiled::compile("");

    assert!(compiled.is_err());
}
