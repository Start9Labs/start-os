use serde::{Deserialize, Serialize, Serializer};
use yajrc::RpcMethod;

/// Know what the process is called
#[derive(Debug, Serialize, Deserialize, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProcessId(pub u32);

#[derive(Debug, Serialize, Deserialize, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProcessGroupId(pub u32);

#[derive(Debug, Clone, Copy)]
pub struct RunCommand;
impl Serialize for RunCommand {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        Serialize::serialize(Self.as_str(), serializer)
    }
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RunCommandParams {
    pub gid: Option<ProcessGroupId>,
    pub command: String,
    pub args: Vec<String>,
}
impl RpcMethod for RunCommand {
    type Params = RunCommandParams;
    type Response = ProcessId;
    fn as_str<'a>(&'a self) -> &'a str {
        "command"
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ReadLineStdout;
impl Serialize for ReadLineStdout {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        Serialize::serialize(Self.as_str(), serializer)
    }
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReadLineStdoutParams {
    pub pid: ProcessId,
}
impl RpcMethod for ReadLineStdout {
    type Params = ReadLineStdoutParams;
    type Response = String;
    fn as_str<'a>(&'a self) -> &'a str {
        "read-line-stdout"
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ReadLineStderr;
impl Serialize for ReadLineStderr {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        Serialize::serialize(Self.as_str(), serializer)
    }
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReadLineStderrParams {
    pub pid: ProcessId,
}
impl RpcMethod for ReadLineStderr {
    type Params = ReadLineStderrParams;
    type Response = String;
    fn as_str<'a>(&'a self) -> &'a str {
        "read-line-stderr"
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Output;
impl Serialize for Output {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        Serialize::serialize(Self.as_str(), serializer)
    }
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OutputParams {
    pub pid: ProcessId,
}
impl RpcMethod for Output {
    type Params = OutputParams;
    type Response = String;
    fn as_str<'a>(&'a self) -> &'a str {
        "output"
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SendSignal;
impl Serialize for SendSignal {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        Serialize::serialize(Self.as_str(), serializer)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SendSignalParams {
    pub pid: ProcessId,
    pub signal: u32,
}
impl RpcMethod for SendSignal {
    type Params = SendSignalParams;
    type Response = ();
    fn as_str<'a>(&'a self) -> &'a str {
        "signal"
    }
}

#[derive(Debug, Clone, Copy)]
pub struct KillGroup;
impl Serialize for KillGroup {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        Serialize::serialize(Self.as_str(), serializer)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KillGroupParams {
    pub gid: ProcessGroupId,
}
impl RpcMethod for KillGroup {
    type Params = KillGroupParams;
    type Response = ();
    fn as_str<'a>(&'a self) -> &'a str {
        "kill-group"
    }
}

#[test]
fn example_echo_line() {
    let input = r#"{"id":0,"jsonrpc":"2.0","method":"command","params":{"command":"echo","args":["world I am here"]}}"#;
    let new_input = JsonRpc::<Input>::maybe_parse(input);
    assert!(new_input.is_some());
    assert_eq!(input, &serde_json::to_string(&new_input.unwrap()).unwrap());
}

#[test]
fn example_input_line() {
    let output = JsonRpc::new(RpcId::UInt(0), Output::Line("world I am here".to_string()));
    let output_str = output.maybe_serialize();
    assert!(output_str.is_some());
    let output_str = output_str.unwrap();
    assert_eq!(
        &output_str,
        r#"{"id":0,"jsonrpc":"2.0","method":"line","params":"world I am here"}"#
    );
    assert_eq!(output, serde_json::from_str(&output_str).unwrap());
}
