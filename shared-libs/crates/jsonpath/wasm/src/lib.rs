extern crate cfg_if;
extern crate js_sys;
extern crate jsonpath_lib as jsonpath;
extern crate serde_json;
extern crate wasm_bindgen;

use cfg_if::cfg_if;
use imbl_value::Value;
#[allow(deprecated)]
use jsonpath::Selector as _Selector;
#[allow(deprecated)]
use jsonpath::SelectorMut as _SelectorMut;
#[allow(deprecated)]
use jsonpath::{JsonPathError, Parser};
use wasm_bindgen::prelude::*;

cfg_if! {
    if #[cfg(feature = "wee_alloc")] {
        extern crate wee_alloc;
        #[global_allocator]
        static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;
    }
}

cfg_if! {
    if #[cfg(feature = "console_error_panic_hook")] {
        extern crate console_error_panic_hook;
        pub use self::console_error_panic_hook::set_once as set_panic_hook;
    } else {
        #[inline]
        pub fn set_panic_hook() {}
    }
}

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn error(s: &str);
}

macro_rules! console_error {
    ($($t:tt)*) => (error(&format_args!($($t)*).to_string()))
}

fn into_serde_json<D>(js_value: &JsValue) -> Result<D, String>
where
    D: for<'a> serde::de::Deserialize<'a>,
{
    if js_value.is_string() {
        match serde_json::from_str(js_value.as_string().unwrap().as_str()) {
            Ok(json) => Ok(json),
            Err(e) => Err(e.to_string()),
        }
    } else {
        match js_value.into_serde() {
            Ok(json) => Ok(json),
            Err(e) => Err(e.to_string()),
        }
    }
}

#[allow(clippy::unnecessary_wraps)]
fn replace_fun(v: Value, fun: &js_sys::Function) -> Option<Value> {
    match JsValue::from_serde(&v) {
        Ok(js_v) => match fun.call1(&JsValue::NULL, &js_v) {
            Ok(result) => match into_serde_json(&result) {
                Ok(json) => Some(json),
                Err(e) => {
                    console_error!("replace_with - closure returned a invalid JSON: {:?}", e);
                    Some(Value::Null)
                }
            },
            Err(e) => {
                console_error!("replace_with - fail to call closure: {:?}", e);
                Some(Value::Null)
            }
        },
        Err(e) => {
            console_error!("replace_with - invalid JSON object: {:?}", e);
            Some(Value::Null)
        }
    }
}

#[wasm_bindgen]
pub fn compile(path: &str) -> JsValue {
    #[allow(deprecated)]
    let node = Parser::compile(path);

    if let Err(e) = &node {
        return JsValue::from_str(&format!("{:?}", JsonPathError::Path(e.clone())));
    };

    let cb = Closure::wrap(Box::new(move |js_value: JsValue| {
        let json = match into_serde_json(&js_value) {
            Ok(json) => json,
            Err(e) => return JsValue::from_str(&format!("{:?}", JsonPathError::Serde(e))),
        };

        #[allow(deprecated)]
        let mut selector = _Selector::new();

        match &node {
            Ok(node) => selector.compiled_path(node),
            Err(e) => return JsValue::from_str(&format!("{:?}", JsonPathError::Path(e.clone()))),
        };

        match selector.value(&json).select() {
            Ok(ret) => match JsValue::from_serde(&ret) {
                Ok(ret) => ret,
                Err(e) => JsValue::from_str(&format!("{:?}", JsonPathError::Serde(e.to_string()))),
            },
            Err(e) => JsValue::from_str(&format!("{:?}", e)),
        }
    }) as Box<dyn Fn(JsValue) -> JsValue>);

    let ret = cb.as_ref().clone();
    cb.forget();
    ret
}

#[wasm_bindgen]
pub fn selector(js_value: JsValue) -> JsValue {
    let json: Value = match JsValue::into_serde(&js_value) {
        Ok(json) => json,
        Err(e) => return JsValue::from_str(&format!("{:?}", JsonPathError::Serde(e.to_string()))),
    };

    #[allow(deprecated)]
    let cb = Closure::wrap(
        Box::new(move |path: String| match Parser::compile(path.as_str()) {
            Ok(node) => {
                let mut selector = _Selector::new();
                let _ = selector.compiled_path(&node);
                match selector.value(&json).select() {
                    Ok(ret) => match JsValue::from_serde(&ret) {
                        Ok(ret) => ret,
                        Err(e) => {
                            JsValue::from_str(&format!("{:?}", JsonPathError::Serde(e.to_string())))
                        }
                    },
                    Err(e) => JsValue::from_str(&format!("{:?}", e)),
                }
            }
            Err(e) => JsValue::from_str(&format!("{:?}", JsonPathError::Path(e))),
        }) as Box<dyn Fn(String) -> JsValue>,
    );

    let ret = cb.as_ref().clone();
    cb.forget();
    ret
}

#[wasm_bindgen]
pub fn select(js_value: JsValue, path: &str) -> JsValue {
    let json = match into_serde_json(&js_value) {
        Ok(json) => json,
        Err(e) => return JsValue::from_str(&format!("{:?}", JsonPathError::Serde(e))),
    };

    match jsonpath::select(&json, path) {
        Ok(ret) => match JsValue::from_serde(&ret) {
            Ok(ret) => ret,
            Err(e) => JsValue::from_str(&format!("{:?}", JsonPathError::Serde(e.to_string()))),
        },
        Err(e) => JsValue::from_str(&format!("{:?}", e)),
    }
}

#[wasm_bindgen(catch, js_name = "deleteValue")]
pub fn delete(js_value: JsValue, path: &str) -> JsValue {
    let json = match into_serde_json(&js_value) {
        Ok(json) => json,
        Err(e) => return JsValue::from_str(&format!("{:?}", JsonPathError::Serde(e))),
    };

    match jsonpath::delete(json, path) {
        Ok(ret) => match JsValue::from_serde(&ret) {
            Ok(ret) => ret,
            Err(e) => JsValue::from_str(&format!("{:?}", JsonPathError::Serde(e.to_string()))),
        },
        Err(e) => JsValue::from_str(&format!("{:?}", e)),
    }
}

#[wasm_bindgen(catch, js_name = "replaceWith")]
pub fn replace_with(js_value: JsValue, path: &str, fun: js_sys::Function) -> JsValue {
    let json = match into_serde_json(&js_value) {
        Ok(json) => json,
        Err(e) => return JsValue::from_str(&format!("{:?}", JsonPathError::Serde(e))),
    };

    match jsonpath::replace_with(json, path, &mut |v| replace_fun(v, &fun)) {
        Ok(ret) => match JsValue::from_serde(&ret) {
            Ok(ret) => ret,
            Err(e) => JsValue::from_str(&format!("{:?}", JsonPathError::Serde(e.to_string()))),
        },
        Err(e) => JsValue::from_str(&format!("{:?}", e)),
    }
}

///
/// `wasm_bindgen` 제약으로 builder-pattern을 구사 할 수 없다.
/// lifetime 제약으로 Selector를 사용 할 수 없다.
///
#[wasm_bindgen]
#[derive(Default)]
pub struct Selector {
    path: Option<String>,
    value: Option<Value>,
}

#[wasm_bindgen]
impl Selector {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Selector::default()
    }

    #[wasm_bindgen(catch)]
    pub fn path(&mut self, path: &str) -> Result<(), JsValue> {
        self.path = Some(path.to_string());
        Ok(())
    }

    #[wasm_bindgen(catch)]
    pub fn value(&mut self, value: JsValue) -> Result<(), JsValue> {
        let json = into_serde_json(&value)
            .map_err(|e| JsValue::from_str(&format!("{:?}", JsonPathError::Serde(e))))?;
        self.value = Some(json);
        Ok(())
    }

    #[wasm_bindgen(catch, js_name = select)]
    pub fn select(&mut self) -> Result<JsValue, JsValue> {
        #[allow(deprecated)]
        let mut selector = _Selector::new();

        if let Some(path) = &self.path {
            let _ = selector
                .str_path(path)
                .map_err(|e| JsValue::from_str(&format!("{:?}", e)))?;
        } else {
            return Err(JsValue::from_str(&format!(
                "{:?}",
                JsonPathError::EmptyPath
            )));
        }

        if let Some(value) = &self.value {
            let _ = selector.value(value);
        } else {
            return Err(JsValue::from_str(&format!(
                "{:?}",
                JsonPathError::EmptyValue
            )));
        }

        match selector.select() {
            Ok(ret) => match JsValue::from_serde(&ret) {
                Ok(ret) => Ok(ret),
                Err(e) => Err(JsValue::from_str(&format!(
                    "{:?}",
                    JsonPathError::Serde(e.to_string())
                ))),
            },
            Err(e) => Err(JsValue::from_str(&format!("{:?}", e))),
        }
    }
}

///
/// `wasm_bindgen` 제약으로 builder-pattern을 구사 할 수 없다.
///
#[wasm_bindgen]
#[derive(Default)]
pub struct SelectorMut {
    path: Option<String>,
    value: Option<Value>,
}

#[wasm_bindgen]
impl SelectorMut {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        SelectorMut::default()
    }

    #[wasm_bindgen(catch)]
    pub fn path(&mut self, path: &str) -> Result<(), JsValue> {
        self.path = Some(path.to_string());
        Ok(())
    }

    #[wasm_bindgen(catch)]
    pub fn value(&mut self, value: JsValue) -> Result<(), JsValue> {
        let json = into_serde_json(&value)
            .map_err(|e| JsValue::from_str(&format!("{:?}", JsonPathError::Serde(e))))?;
        self.value = Some(json);
        Ok(())
    }

    #[wasm_bindgen(catch, js_name = "deleteValue")]
    pub fn delete(&mut self) -> Result<(), JsValue> {
        #[allow(deprecated)]
        let mut selector = _SelectorMut::new();

        if let Some(path) = &self.path {
            let _ = selector.str_path(path);
        } else {
            return Err(JsValue::from_str(&format!(
                "{:?}",
                JsonPathError::EmptyPath
            )));
        };

        if let Some(value) = self.value.take() {
            selector.value(value);
        } else {
            return Err(JsValue::from_str(&format!(
                "{:?}",
                JsonPathError::EmptyValue
            )));
        };

        match selector.delete() {
            Err(e) => Err(JsValue::from_str(&format!("{:?}", e))),
            _ => {
                self.value = selector.take();
                Ok(())
            }
        }
    }

    #[wasm_bindgen(catch, js_name = replaceWith)]
    pub fn replace_with(&mut self, fun: js_sys::Function) -> Result<(), JsValue> {
        #[allow(deprecated)]
        let mut selector = _SelectorMut::new();

        if let Some(path) = &self.path {
            let _ = selector.str_path(path);
        } else {
            return Err(JsValue::from_str(&format!(
                "{:?}",
                JsonPathError::EmptyPath
            )));
        };

        if let Some(value) = self.value.take() {
            selector.value(value);
        } else {
            return Err(JsValue::from_str(&format!(
                "{:?}",
                JsonPathError::EmptyValue
            )));
        };

        match selector.replace_with(&mut |v| replace_fun(v, &fun)) {
            Err(e) => Err(JsValue::from_str(&format!("{:?}", e))),
            _ => {
                self.value = selector.take();
                Ok(())
            }
        }
    }

    #[wasm_bindgen(catch)]
    pub fn take(&mut self) -> Result<JsValue, JsValue> {
        match self.value.take() {
            Some(ret) => match JsValue::from_serde(&ret) {
                Ok(ret) => Ok(ret),
                Err(e) => Err(JsValue::from_str(&format!("{:?}", e))),
            },
            None => Err(JsValue::from_str(&format!(
                "{:?}",
                JsonPathError::EmptyValue
            ))),
        }
    }
}
