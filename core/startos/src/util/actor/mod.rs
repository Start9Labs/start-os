use std::any::{Any, TypeId};
use std::collections::BTreeMap;
use std::sync::Arc;
use std::time::Duration;

use futures::future::BoxFuture;
use futures::{Future, FutureExt};
use tokio::sync::oneshot;

#[allow(unused_imports)]
use crate::prelude::*;
use crate::util::actor::background::BackgroundJobQueue;

pub mod background;
pub mod concurrent;
pub mod simple;

pub trait Actor: Sized + Send + 'static {
    #[allow(unused_variables)]
    fn init(&mut self, jobs: &BackgroundJobQueue) {}
}

pub trait Handler<M: Any + Send>: Actor {
    type Response: Any + Send;
    /// DRAGONS: this must be correctly implemented bi-directionally in order to work as expected
    fn conflicts_with(#[allow(unused_variables)] msg: &M) -> ConflictBuilder<Self> {
        ConflictBuilder::everything()
    }
    fn handle(
        &mut self,
        msg: M,
        jobs: &BackgroundJobQueue,
    ) -> impl Future<Output = Self::Response> + Send;
}

type ConflictFn<A> = dyn Fn(&dyn Message<A>) -> bool + Send + Sync;

trait Message<A>: Send + Any {
    fn conflicts_with(&self) -> Arc<ConflictFn<A>>;
    fn handle_with<'a>(
        self: Box<Self>,
        actor: &'a mut A,
        jobs: &'a BackgroundJobQueue,
    ) -> BoxFuture<'a, Box<dyn Any + Send>>;
}
impl<M: Send + Any, A: Actor> Message<A> for M
where
    A: Handler<M>,
{
    fn conflicts_with(&self) -> Arc<ConflictFn<A>> {
        A::conflicts_with(self).build()
    }
    fn handle_with<'a>(
        self: Box<Self>,
        actor: &'a mut A,
        jobs: &'a BackgroundJobQueue,
    ) -> BoxFuture<'a, Box<dyn Any + Send>> {
        async move { Box::new(actor.handle(*self, jobs).await) as Box<dyn Any + Send> }.boxed()
    }
}
impl<A: Actor> dyn Message<A> {
    #[inline]
    pub fn is<M: Message<A>>(&self) -> bool {
        let t = TypeId::of::<M>();
        let concrete = self.type_id();
        t == concrete
    }
    #[inline]
    pub unsafe fn downcast_ref_unchecked<M: Message<A>>(&self) -> &M {
        debug_assert!(self.is::<M>());
        unsafe { &*(self as *const dyn Message<A> as *const M) }
    }
    #[inline]
    fn downcast_ref<M: Message<A>>(&self) -> Option<&M> {
        if self.is::<M>() {
            unsafe { Some(self.downcast_ref_unchecked()) }
        } else {
            None
        }
    }
}

type Request<A> = (Box<dyn Message<A>>, oneshot::Sender<Box<dyn Any + Send>>);

pub enum PendingMessageStrategy {
    CancelAll,
    FinishCurrentCancelPending { timeout: Option<Duration> },
    FinishAll { timeout: Option<Duration> },
}

pub struct ConflictBuilder<A> {
    base: bool,
    except: BTreeMap<TypeId, Option<Box<dyn Fn(&dyn Message<A>) -> bool + Send + Sync>>>,
}
impl<A: Actor> ConflictBuilder<A> {
    pub const fn everything() -> Self {
        Self {
            base: true,
            except: BTreeMap::new(),
        }
    }
    pub const fn nothing() -> Self {
        Self {
            base: false,
            except: BTreeMap::new(),
        }
    }
    pub fn except<M: Any + Send>(mut self) -> Self
    where
        A: Handler<M>,
    {
        self.except.insert(TypeId::of::<M>(), None);
        self
    }
    pub fn except_if<M: Any + Send, F: Fn(&M) -> bool + Send + Sync + 'static>(
        mut self,
        f: F,
    ) -> Self
    where
        A: Handler<M>,
    {
        self.except.insert(
            TypeId::of::<M>(),
            Some(Box::new(move |m| {
                if let Some(m) = m.downcast_ref() {
                    f(m)
                } else {
                    false
                }
            })),
        );
        self
    }
    fn build(self) -> Arc<ConflictFn<A>> {
        Arc::new(move |m| {
            self.base
                ^ if let Some(entry) = self.except.get(&m.type_id()) {
                    if let Some(f) = entry {
                        f(m)
                    } else {
                        true
                    }
                } else {
                    false
                }
        })
    }
}
