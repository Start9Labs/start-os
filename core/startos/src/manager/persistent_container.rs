use std::sync::Arc;
use std::time::Duration;

use helpers::UnixRpcClient;
use models::ProcedureName;
use nix::sys::signal::Signal;
use serde::de::DeserializeOwned;
use tokio::sync::Mutex;
use tracing::instrument;

use super::manager_seed::ManagerSeed;
use crate::lxc::{LxcConfig, LxcContainer};
use crate::prelude::*;

const RPC_CONNECT_TIMEOUT: Duration = Duration::from_secs(10);

mod rpc {
    use std::time::Duration;

    use imbl_value::Value;
    use models::ProcedureName;
    use rpc_toolkit::yajrc::RpcMethod;

    #[derive(Clone, serde::Deserialize, serde::Serialize)]
    pub struct ExecuteParams {
        procedure: String,
        input: Value,
        timeout: Option<Duration>,
    }
    impl ExecuteParams {
        pub fn new(procedure: ProcedureName, input: Value, timeout: Option<Duration>) -> Self {
            Self {
                procedure: procedure.js_function_name(),
                input,
                timeout,
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
}

struct ProcedureId(u64);

// @DRB Need to have a way of starting the the procudures and getting the information back
// @DRB On top of this we need to also have  the procedures to have the effects and get the results back for them, maybe lock them to the running instance?
/// Persistant container are the old containers that need to run all the time
/// The goal is that all services will be persistent containers, waiting to run the main system.
pub struct PersistentContainer {
    lxc_container: LxcContainer,
    // TODO: Drb: Implement to spec https://github.com/Start9Labs/start-sdk/blob/master/lib/types.ts#L223
    rpc_client: UnixRpcClient,
    manager_seed: Arc<ManagerSeed>,
    procedures: Mutex<Vec<(ProcedureName, ProcedureId)>>,
}

impl PersistentContainer {
    #[instrument(skip_all)]
    pub async fn init(seed: &Arc<ManagerSeed>) -> Result<Self, Error> {
        let lxc_container = seed.ctx.lxc_manager.create(LxcConfig::default()).await?;
        // TODO: mount images
        let rpc_client = lxc_container.connect_rpc(Some(RPC_CONNECT_TIMEOUT)).await?;
        rpc_client.request(rpc::Init, ()).await.map_err(|e| {
            Error::new(
                eyre!("{}: {} ({:?})", e.code, e.message, e.data),
                ErrorKind::Unknown, // TODO
            )
        })?;

        Ok(Self {
            lxc_container,
            rpc_client,
            manager_seed: seed.clone(),
            procedures: Default::default(),
        })
    }

    pub async fn execute<O>(
        &self,
        name: ProcedureName,
        input: Value,
        timeout: Option<Duration>,
    ) -> Result<Result<O, (i32, String)>, Error>
    where
        O: DeserializeOwned,
    {
        match self._execute(name, input, timeout).await {
            Ok(Ok(a)) => Ok(Ok(from_value(a)?)),
            Ok(Err(e)) => Ok(Err(e)),
            Err(e) => Err(e),
        }
    }

    pub async fn sanboxed<O>(
        &self,
        name: ProcedureName,
        input: Value,
        timeout: Option<Duration>,
    ) -> Result<Result<O, (i32, String)>, Error>
    where
        O: DeserializeOwned,
    {
        match self._sandboxed(name, input, timeout).await {
            Ok(Ok(a)) => Ok(Ok(from_value(a)?)),
            Ok(Err(e)) => Ok(Err(e)),
            Err(e) => Err(e),
        }
    }

    async fn _execute(
        &self,
        name: ProcedureName,
        input: Value,
        timeout: Option<Duration>,
    ) -> Result<Result<Value, (i32, String)>, Error> {
        let fut = self
            .rpc_client
            .request(rpc::Execute, rpc::ExecuteParams::new(name, input, timeout));

        Ok(if let Some(timeout) = timeout {
            tokio::time::timeout(timeout, fut)
                .await
                .with_kind(ErrorKind::Timeout)?
        } else {
            fut.await
        }
        .map_err(|e| (e.code, e.message.into_owned())))
    }

    async fn _sandboxed(
        &self,
        name: ProcedureName,
        input: Value,
        timeout: Option<Duration>,
    ) -> Result<Result<Value, (i32, String)>, Error> {
        let fut = self
            .rpc_client
            .request(rpc::Sandbox, rpc::ExecuteParams::new(name, input, timeout));

        Ok(if let Some(timeout) = timeout {
            tokio::time::timeout(timeout, fut)
                .await
                .with_kind(ErrorKind::Timeout)?
        } else {
            fut.await
        }
        .map_err(|e| (e.code, e.message.into_owned())))
    }

    pub async fn send_signal(&self, gid: Arc<super::Gid>, signal: Signal) -> Result<(), Error> {
        todo!("DRB")
    }
}
