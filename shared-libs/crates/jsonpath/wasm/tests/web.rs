/**
 * 실행 : `wasm-pack test`
 * 그러나,, 잘안돼서 안씀
 */
#![cfg(target_arch = "wasm32")]

extern crate core;
extern crate js_sys;
extern crate jsonpath_wasm as jsonpath;
#[macro_use]
extern crate serde_json;
extern crate wasm_bindgen;
extern crate wasm_bindgen_test;

use imbl_value::Value;
use wasm_bindgen::*;
use wasm_bindgen::prelude::*;
use wasm_bindgen_test::*;

wasm_bindgen_test_configure!(run_in_browser);

fn json_str() -> &'static str {
    r#"
    {
      "store": {
        "book": [
          {
            "category": "reference",
            "author": "Nigel Rees",
            "title": "Sayings of the Century",
            "price": 8.95
          },
          {
            "category": "fiction",
            "author": "Evelyn Waugh",
            "title": "Sword of Honour",
            "price": 12.99
          },
          {
            "category": "fiction",
            "author": "Herman Melville",
            "title": "Moby Dick",
            "isbn": "0-553-21311-3",
            "price": 8.99
          },
          {
            "category": "fiction",
            "author": "J. R. R. Tolkien",
            "title": "The Lord of the Rings",
            "isbn": "0-395-19395-8",
            "price": 22.99
          }
        ],
        "bicycle": {
          "color": "red",
          "price": 19.95
        }
      },
      "expensive": 10
    }
    "#
}

fn target_json() -> Value {
    json!([{
        "category" : "fiction",
        "author" : "Herman Melville",
        "title" : "Moby Dick",
        "isbn" : "0-553-21311-3",
        "price" : 8.99
    }])
}

#[wasm_bindgen_test]
fn select() {
    let json: Value = jsonpath::select(JsValue::from_str(json_str()), "$..book[2]").into_serde().unwrap();
    assert_eq!(json, target_json());
}

#[wasm_bindgen_test]
fn compile() {
    let js_value = jsonpath::compile("$..book[2]");
    assert_eq!(js_value.is_function(), true);

    let cb: &js_sys::Function = JsCast::unchecked_ref(js_value.as_ref());
    let cb_result: JsValue = cb.call1(&js_value, &JsValue::from_str(json_str())).unwrap();
    let json: Value = cb_result.into_serde().unwrap();
    assert_eq!(json, target_json());
}

#[wasm_bindgen_test]
fn selector() {
    let js_value = jsonpath::selector(JsValue::from_str(json_str()));
    assert_eq!(js_value.is_function(), true);

    let cb: &js_sys::Function = JsCast::unchecked_ref(js_value.as_ref());
    let cb_result: JsValue = cb.call1(&js_value, &JsValue::from_str("$..book[2]")).unwrap();
    let json: Value = cb_result.into_serde().unwrap();
    assert_eq!(json, target_json());
}

#[wasm_bindgen_test]
fn selector_struct() {
    let mut selector = jsonpath::Selector::new();
    selector.path("$..book[2]").unwrap();
    selector.value(JsValue::from_str(json_str())).unwrap();

    let json: Value = selector.select_as().unwrap().into_serde().unwrap();
    assert_eq!(json, target_json());

    let cb = Closure::wrap(Box::new(|js_value: JsValue| {
        match js_value.into_serde().unwrap() {
            Value::Array(mut vec) => {
                match vec.pop().unwrap() {
                    Value::Object(mut map) => {
                        map.clear();
                        map.insert("key".to_string(), Value::String("value".to_string()));
                        JsValue::from_serde(&Value::Object(map)).unwrap()
                    }
                    _ => return JsValue::NULL
                }
            }
            _ => return JsValue::NULL
        }
    }) as Box<Fn(JsValue) -> JsValue>);

    selector.map(cb.as_ref().clone()).unwrap();
    let js_value = selector.get().unwrap();
    assert_eq!(js_value.into_serde::<Value>().unwrap(), json!({ "key": "value" }));
}