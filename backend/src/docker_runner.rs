use std::str::FromStr;

use color_eyre::Report;
use serde::{ser::SerializeMap, Deserialize, Serialize, Serializer};
use tracing::instrument;

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(untagged)]
pub enum RpcId {
    Int(i64),
    Float(f64),
    String(String),
}

#[derive(Debug, Clone, Deserialize)]
pub struct OutputRpcId {
    id: Option<RpcId>,
    output: Output,
}

impl Serialize for OutputRpcId {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(3))?;
        map.serialize_entry("jsonrpc", "2.0")?;
        map.serialize_entry("id", &self.id)?;
        map.serialize_entry("result", &self.output)?;
        map.end()
    }
}

impl OutputRpcId {
    #[instrument]
    fn maybe_serialize(&self) -> Option<String> {
        tracing::trace!("Should be serializing");
        match serde_json::to_string(self) {
            Ok(x) => Some(x),
            Err(e) => {
                tracing::warn!("Could not stringify and skipping");
                tracing::debug!("{:?}", e);
                None
            }
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(tag = "jsonrpc", rename_all = "camelCase")]
pub enum OutputRpc {
    #[serde(rename = "2.0")]
    Two(Output),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum Output {
    Line(String),
    Error(String),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct InputRpcId {
    id: Option<RpcId>,
    #[serde(flatten)]
    input_rpc: InputRpc,
}

impl InputRpcId {
    fn new_cmd(count: i64, command: String, args: Vec<String>) -> Self {
        InputRpcId {
            id: Some(RpcId::Int(count)),
            input_rpc: InputRpc::Two(Input::Command { command, args }),
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(tag = "jsonrpc", rename_all = "camelCase")]
pub enum InputRpc {
    #[serde(rename = "2.0")]
    Two(Input),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(tag = "method", content = "params", rename_all = "camelCase")]
pub enum Input {
    Command { command: String, args: Vec<String> },
}

impl FromStr for InputRpcId {
    type Err = Report;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let input: Self = serde_json::from_str(s)?;
        Ok(input)
    }
}
impl InputRpcId {
    fn maybe_parse(s: &str) -> Option<Self> {
        match serde_json::from_str::<Self>(s) {
            Ok(a) => Some(a),
            Err(e) => {
                tracing::warn!("Could not parse and skipping: {}", s);
                tracing::debug!("{:?}", e);
                None
            }
        }
    }
}
