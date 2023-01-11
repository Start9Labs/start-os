use color_eyre::{eyre::eyre, Report};
use futures::future::BoxFuture;
use helpers::NonDetachingJoinHandle;
use tokio::sync::{mpsc, oneshot};
use tracing::instrument;

pub const ACTOR_MAX_BUFFER: usize = 1024;
type MessageHandler<State> = Box<dyn (FnOnce(State) -> BoxFuture<'static, State>) + Send>;
type MessageHandlerSender<State> = mpsc::Sender<MessageHandler<State>>;

pub struct Actor<State: 'static + Send> {
    thread: NonDetachingJoinHandle<()>,
    sender: MessageHandlerSender<State>,
}

impl<State> Actor<State>
where
    State: 'static + Send,
{
    pub fn new(initial_state: State) -> Self {
        let (sender, receiver) = mpsc::channel::<MessageHandler<State>>(ACTOR_MAX_BUFFER);
        let thread = Self::create_thread(receiver, initial_state);

        Self { sender, thread }
    }

    #[instrument(skip(self, next_event))]
    pub async fn event<F, R>(&self, next_event: F) -> Result<R, Report>
    where
        F: 'static + Send + FnOnce(&mut State) -> R,
        R: 'static + Send,
    {
        let (send, receive) = oneshot::channel::<R>();
        self.sender.send(Box::new(move |mut state| {
            if send.send(next_event(&mut state)).is_err() {
                tracing::warn!("Sending oneshot message failed, ignoring");
            }
            Box::pin(async { state })
        }));

        receive
            .await
            .map_err(|e| eyre!("Have received a oneshot recverror: {e:?}"))
    }

    #[instrument(skip(self, next_event))]
    pub async fn async_event<F, R>(&self, next_event: F) -> Result<R, Report>
    where
        F: 'static + Send + FnOnce(&mut State) -> BoxFuture<R>,
        R: 'static + Send,
    {
        let (send, receive) = oneshot::channel::<R>();
        self.sender.send(Box::new(move |mut state| {
            Box::pin(async move {
                if send.send(next_event(&mut state).await).is_err() {
                    tracing::warn!("Sending oneshot message failed, ignoring");
                }
                state
            })
        }));

        receive
            .await
            .map_err(|e| eyre!("Have received a oneshot recverror: {e:?}"))
    }

    fn create_thread(
        receiver: mpsc::Receiver<MessageHandler<State>>,
        state: State,
    ) -> NonDetachingJoinHandle<()> {
        let spawned = tokio::spawn(async move {
            let mut state = state;
            let mut receiver = receiver;
            while let Some(handler) = receiver.recv().await {
                state = handler(state).await;
            }
        });
        spawned.into()
    }
}
