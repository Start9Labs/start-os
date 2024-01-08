use std::time::Duration;

use imbl_value::Value;
use models::ProcedureName;
use rpc_toolkit::yajrc::{RpcError, RpcMethod};

use crate::prelude::*;

#[derive(Clone)]
pub struct Init;
impl RpcMethod for Init {
    type Params = ();
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
    type Params = ();
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

#[derive(Clone)]
pub struct Start;
impl RpcMethod for Start {
    type Params = ();
    type Response = ();
    fn as_str<'a>(&'a self) -> &'a str {
        "start"
    }
}
impl serde::Serialize for Start {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(self.as_str())
    }
}

#[derive(Clone, serde::Deserialize, serde::Serialize)]
pub struct StopParams {
    timeout: Option<u128>,
}
impl StopParams {
    pub fn new(timeout: Option<Duration>) -> Self {
        Self {
            timeout: timeout.map(|d| d.as_millis()),
        }
    }
}
#[derive(Clone)]
pub struct Stop;
impl RpcMethod for Stop {
    type Params = StopParams;
    type Response = ();
    fn as_str<'a>(&'a self) -> &'a str {
        "stop"
    }
}
impl serde::Serialize for Stop {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(self.as_str())
    }
}

#[derive(Clone, serde::Deserialize, serde::Serialize)]
pub struct ExecuteParams {
    procedure: String,
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

pub(super) fn convert_rpc_error(e: RpcError) -> Error {
    Error::new(
        eyre!("{}: {} ({:?})", e.code, e.message, e.data),
        ErrorKind::Unknown, // TODO
    )
}
