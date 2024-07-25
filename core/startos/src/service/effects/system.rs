use crate::service::effects::callbacks::CallbackHandler;
use crate::service::effects::prelude::*;
use crate::service::rpc::CallbackId;
use crate::system::SmtpValue;

#[derive(Debug, Clone, Serialize, Deserialize, TS, Parser)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
pub struct GetSystemSmtpParams {
    #[arg(skip)]
    callback: Option<CallbackId>,
}
pub async fn get_system_smtp(
    context: EffectContext,
    GetSystemSmtpParams { callback }: GetSystemSmtpParams,
) -> Result<Option<SmtpValue>, Error> {
    let context = context.deref()?;
    let res = context
        .seed
        .ctx
        .db
        .peek()
        .await
        .into_public()
        .into_server_info()
        .into_smtp()
        .de()?;

    if let Some(callback) = callback {
        let callback = callback.register(&context.seed.persistent_container);
        context
            .seed
            .ctx
            .callbacks
            .add_get_system_smtp(CallbackHandler::new(&context, callback));
    }

    Ok(res)
}
