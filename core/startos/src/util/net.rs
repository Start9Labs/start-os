use std::borrow::Cow;

use axum::extract::ws::{self, CloseFrame};
use futures::Future;

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
