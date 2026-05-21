use crate::progress::PhaseProgress;
use crate::service::effects::prelude::*;

#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SetBackupProgress {
    pub progress: PhaseProgress,
}

pub async fn set_backup_progress(
    context: EffectContext,
    SetBackupProgress { progress }: SetBackupProgress,
) -> Result<(), Error> {
    let context = context.deref()?;
    context.seed.backup_phase.mutate(|slot| {
        if let Some(handle) = slot.as_mut() {
            handle.set_phase_value(progress);
        }
    });
    Ok(())
}
