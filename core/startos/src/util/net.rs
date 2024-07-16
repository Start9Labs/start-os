use std::borrow::Cow;
use std::sync::Mutex;

use axum::extract::ws::{self, CloseFrame};
use futures::{Future, Stream, StreamExt};

use crate::prelude::*;

pub trait WebSocketExt {
    fn normal_close(
        self,
        msg: impl Into<Cow<'static, str>>,
    ) -> impl Future<Output = Result<(), Error>>;
}

impl WebSocketExt for ws::WebSocket {
    async fn normal_close(mut self, msg: impl Into<Cow<'static, str>>) -> Result<(), Error> {
        self.send(ws::Message::Close(Some(CloseFrame {
            code: 1000,
            reason: msg.into(),
        })))
        .await
        .with_kind(ErrorKind::Network)
    }
}

pub struct SyncBody(Mutex<axum::body::BodyDataStream>);
impl From<axum::body::Body> for SyncBody {
    fn from(value: axum::body::Body) -> Self {
        SyncBody(Mutex::new(value.into_data_stream()))
    }
}
impl Stream for SyncBody {
    type Item = <axum::body::BodyDataStream as Stream>::Item;
    fn poll_next(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Self::Item>> {
        self.0.lock().unwrap().poll_next_unpin(cx)
    }
}
