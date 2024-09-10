use core::fmt;
use std::borrow::Cow;
use std::sync::Mutex;
use std::time::Duration;

use axum::extract::ws::{self, CloseFrame};
use futures::{Future, Stream, StreamExt};

use crate::prelude::*;

pub trait WebSocketExt {
    fn normal_close(
        self,
        msg: impl Into<Cow<'static, str>> + Send,
    ) -> impl Future<Output = Result<(), Error>> + Send;
    fn close_result(
        self,
        result: Result<impl Into<Cow<'static, str>> + Send, impl fmt::Display + Send>,
    ) -> impl Future<Output = Result<(), Error>> + Send;
}

impl WebSocketExt for ws::WebSocket {
    async fn normal_close(self, msg: impl Into<Cow<'static, str>> + Send) -> Result<(), Error> {
        self.close_result(Ok::<_, Error>(msg)).await
    }
    async fn close_result(
        mut self,
        result: Result<impl Into<Cow<'static, str>> + Send, impl fmt::Display + Send>,
    ) -> Result<(), Error> {
        match result {
            Ok(msg) => self
                .send(ws::Message::Close(Some(CloseFrame {
                    code: 1000,
                    reason: msg.into(),
                })))
                .await
                .with_kind(ErrorKind::Network)?,
            Err(e) => self
                .send(ws::Message::Close(Some(CloseFrame {
                    code: 1011,
                    reason: e.to_string().into(),
                })))
                .await
                .with_kind(ErrorKind::Network)?,
        }
        tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                if let Some(ws::Message::Close(_)) = self
                    .next()
                    .await
                    .transpose()
                    .with_kind(ErrorKind::Network)?
                {
                    break;
                }
            }
            Ok::<_, Error>(())
        })
        .await
        .with_kind(ErrorKind::Timeout)??;
        Ok(())
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
