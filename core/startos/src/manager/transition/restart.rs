use crate::manager::ServiceActor;
use crate::util::actor::BackgroundJobs;

impl TransitionState {
    pub fn restart(actor: &mut ServiceActor, jobs: &mut BackgroundJobs) -> Self {
        actor.persistent_container.t
    }
}
