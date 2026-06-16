use crate::progress::Progress;
use crate::service::effects::prelude::*;

#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SetInitProgress {
    pub progress: Progress,
}

pub async fn set_init_progress(
    context: EffectContext,
    SetInitProgress { progress }: SetInitProgress,
) -> Result<(), Error> {
    let context = context.deref()?;
    context.seed.init_phase.mutate(|slot| {
        if let Some(handle) = slot.as_mut() {
            handle.set_phase_value(progress);
        }
    });
    Ok(())
}
