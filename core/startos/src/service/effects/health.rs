use models::HealthCheckId;

use crate::service::effects::prelude::*;
use crate::status::health_check::NamedHealthCheckResult;
use crate::status::MainStatus;

#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SetHealth {
    id: HealthCheckId,
    #[serde(flatten)]
    result: NamedHealthCheckResult,
}
pub async fn set_health(
    context: EffectContext,
    SetHealth { id, result }: SetHealth,
) -> Result<(), Error> {
    let context = context.deref()?;

    let package_id = &context.seed.id;
    context
        .seed
        .ctx
        .db
        .mutate(move |db| {
            db.as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(package_id)
                .or_not_found(package_id)?
                .as_status_mut()
                .mutate(|main| {
                    match main {
                        MainStatus::Running { ref mut health, .. }
                        | MainStatus::Starting { ref mut health } => {
                            health.insert(id, result);
                        }
                        _ => (),
                    }
                    Ok(())
                })
        })
        .await?;
    Ok(())
}
