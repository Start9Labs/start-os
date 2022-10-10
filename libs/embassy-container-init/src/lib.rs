use serde::{Deserialize, Serialize};
use tracing::instrument;

pub type InputJsonRpc = JsonRpc<Input>;
pub type OutputJsonRpc = JsonRpc<Output>;

// BLUJ_TODO: Input::Stop process Id
// BLUJ_TODO Need to have a better long running startup, maybe make it create dockerfile in temp?

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(untagged)]
pub enum RpcId {
    Int(i64),
    Float(f64),
    String(String),
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq)]
pub struct JsonRpc<T> {
    id: Option<RpcId>,
    #[serde(flatten)]
    pub version_rpc: VersionRpc<T>,
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq)]
#[serde(tag = "jsonrpc", rename_all = "camelCase")]
pub enum VersionRpc<T> {
    #[serde(rename = "2.0")]
    Two(T),
}

impl<T> JsonRpc<T>
where
    T: Serialize + for<'de> serde::Deserialize<'de> + std::fmt::Debug,
{
    pub fn new(id: Option<RpcId>, body: T) -> Self {
        JsonRpc {
            id,
            version_rpc: VersionRpc::Two(body),
        }
    }
    pub fn into_pair(self) -> (Option<RpcId>, T) {
        let Self { id, version_rpc } = self;
        let VersionRpc::Two(body) = version_rpc;
        (id, body)
    }
    #[instrument]
    pub fn maybe_serialize(&self) -> Option<String> {
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
    #[instrument]
    pub fn maybe_parse(s: &str) -> Option<Self> {
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

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(tag = "method", content = "params", rename_all = "camelCase")]
pub enum Output {
    Line(String),
    Error(String),
    Done(),
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(tag = "method", content = "params", rename_all = "camelCase")]
pub enum Input {
    Command { command: String, args: Vec<String> },
}

#[test]
fn example_echo_line() {
    let input = r#"{"id":"test","jsonrpc":"2.0","method":"command","params":{"command":"echo","args":["world I am here"]}}"#;
    let new_input = JsonRpc::<Input>::maybe_parse(input);
    assert!(new_input.is_some());
    assert_eq!(input, &serde_json::to_string(&new_input.unwrap()).unwrap());
}

#[test]
fn example_input_line() {
    let output = JsonRpc::new(
        Some(RpcId::String("test".to_string())),
        Output::Line("world I am here".to_string()),
    );
    let output_str = output.maybe_serialize();
    assert!(output_str.is_some());
    let output_str = output_str.unwrap();
    assert_eq!(
        &output_str,
        r#"{"id":"test","jsonrpc":"2.0","method":"line","params":"world I am here"}"#
    );
    assert_eq!(output, serde_json::from_str(&output_str).unwrap());
}
