use std::time::Duration;

use imbl_value::Value;
use models::ProcedureName;
use rpc_toolkit::yajrc::RpcMethod;
use rpc_toolkit::Empty;
use ts_rs::TS;

use crate::prelude::*;

#[derive(Clone)]
pub struct Init;
impl RpcMethod for Init {
    type Params = Empty;
    type Response = ();
    fn as_str<'a>(&'a self) -> &'a str {
        "init"
    }
}
impl serde::Serialize for Init {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(self.as_str())
    }
}

#[derive(Clone)]
pub struct Exit;
impl RpcMethod for Exit {
    type Params = Empty;
    type Response = ();
    fn as_str<'a>(&'a self) -> &'a str {
        "exit"
    }
}
impl serde::Serialize for Exit {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(self.as_str())
    }
}

#[derive(Clone, serde::Deserialize, serde::Serialize, TS)]
pub struct ExecuteParams {
    procedure: String,
    #[ts(type = "any")]
    input: Value,
    timeout: Option<u128>,
}
impl ExecuteParams {
    pub fn new(procedure: ProcedureName, input: Value, timeout: Option<Duration>) -> Self {
        Self {
            procedure: procedure.js_function_name(),
            input,
            timeout: timeout.map(|d| d.as_millis()),
        }
    }
}

#[derive(Clone)]
pub struct Execute;
impl RpcMethod for Execute {
    type Params = ExecuteParams;
    type Response = Value;
    fn as_str<'a>(&'a self) -> &'a str {
        "execute"
    }
}
impl serde::Serialize for Execute {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(self.as_str())
    }
}

#[derive(Clone)]
pub struct Sandbox;
impl RpcMethod for Sandbox {
    type Params = ExecuteParams;
    type Response = Value;
    fn as_str<'a>(&'a self) -> &'a str {
        "sandbox"
    }
}
impl serde::Serialize for Sandbox {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(self.as_str())
    }
}
