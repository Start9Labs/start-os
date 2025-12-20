use core::fmt;
use std::pin::Pin;
use std::sync::Mutex;
use std::task::{Context, Poll, ready};
use std::time::Duration;

use axum::extract::ws::{CloseFrame, Message, Utf8Bytes, WebSocket as AxumWebSocket};
use futures::{Sink, SinkExt, Stream, StreamExt};
use tokio::time::{Instant, Sleep};

use crate::prelude::*;

const PING_INTERVAL: Duration = Duration::from_secs(30);
const PING_TIMEOUT: Duration = Duration::from_secs(300);

/// A wrapper around axum's WebSocket that automatically sends ping frames
/// to keep the connection alive during HTTP/2.
///
/// HTTP/2 streams can timeout if idle, even when the underlying connection
/// has keep-alive enabled. This wrapper sends a ping frame if no data has
/// been sent within the ping interval while waiting to receive a message.
pub struct WebSocket {
    inner: AxumWebSocket,
    ping_state: Option<(bool, u64)>,
    next_ping: Pin<Box<Sleep>>,
    fused: bool,
}

impl WebSocket {
    pub fn new(ws: AxumWebSocket) -> Self {
        Self {
            inner: ws,
            ping_state: None,
            next_ping: Box::pin(tokio::time::sleep(PING_INTERVAL)),
            fused: false,
        }
    }

    pub fn into_inner(self) -> AxumWebSocket {
        self.inner
    }

    pub async fn send(&mut self, msg: Message) -> Result<(), axum::Error> {
        if self.ping_state.is_none() {
            self.next_ping
                .as_mut()
                .reset(Instant::now() + PING_INTERVAL);
        }
        self.inner.send(msg).await
    }

    pub fn poll_recv(
        &mut self,
        cx: &mut Context<'_>,
    ) -> Poll<Option<Result<Message, axum::Error>>> {
        if self.fused {
            return Poll::Ready(None);
        }
        let mut inner = Pin::new(&mut self.inner);

        loop {
            if let Poll::Ready(msg) = inner.as_mut().poll_next(cx) {
                if self.ping_state.is_none() {
                    self.next_ping
                        .as_mut()
                        .reset(Instant::now() + PING_INTERVAL);
                }

                if let Some(Ok(Message::Pong(x))) = &msg {
                    if let Some((true, id)) = self.ping_state {
                        if &u64::to_be_bytes(id)[..] == &**x {
                            self.ping_state.take();
                            self.next_ping
                                .as_mut()
                                .reset(Instant::now() + PING_INTERVAL);
                            continue;
                        }
                    }
                }

                break Poll::Ready(msg);
            }

            if let Some((sent, id)) = &mut self.ping_state {
                if !*sent {
                    ready!(inner.as_mut().poll_ready(cx))?;
                    inner
                        .as_mut()
                        .start_send(Message::Ping((u64::to_be_bytes(*id).to_vec()).into()))?;
                    self.next_ping.as_mut().reset(Instant::now() + PING_TIMEOUT);
                    *sent = true;
                }
                ready!(inner.as_mut().poll_flush(cx))?;
            }

            ready!(self.next_ping.as_mut().poll(cx));
            if self.ping_state.is_some() {
                self.fused = true;
                break Poll::Ready(Some(Err(axum::Error::new(eyre!(
                    "Timeout: WebSocket did not respond to ping within {PING_TIMEOUT:?}"
                )))));
            }
            self.ping_state = Some((false, rand::random()));
        }
    }

    /// Receive a message from the websocket, automatically sending ping frames
    /// if the connection is idle for too long.
    ///
    /// Ping and Pong frames are handled internally and not returned to the caller.
    pub async fn recv(&mut self) -> Option<Result<Message, axum::Error>> {
        futures::future::poll_fn(|cx| self.poll_recv(cx)).await
    }

    /// Close the websocket connection normally.
    pub async fn normal_close(mut self, msg: impl Into<Utf8Bytes>) -> Result<(), Error> {
        self.inner
            .send(Message::Close(Some(CloseFrame {
                code: 1000,
                reason: msg.into(),
            })))
            .await
            .with_kind(ErrorKind::Network)?;
        while !matches!(
            self.inner
                .recv()
                .await
                .transpose()
                .with_kind(ErrorKind::Network)?,
            Some(Message::Close(_)) | None
        ) {}
        Ok(())
    }

    /// Close the websocket connection with a result.
    pub async fn close_result(
        mut self,
        result: Result<impl Into<Utf8Bytes> + Send, impl fmt::Display + Send>,
    ) -> Result<(), Error> {
        match result {
            Ok(msg) => self
                .inner
                .send(Message::Close(Some(CloseFrame {
                    code: 1000,
                    reason: msg.into(),
                })))
                .await
                .with_kind(ErrorKind::Network)?,
            Err(e) => self
                .inner
                .send(Message::Close(Some(CloseFrame {
                    code: 1011,
                    reason: e.to_string().into(),
                })))
                .await
                .with_kind(ErrorKind::Network)?,
        }
        while !matches!(
            self.inner
                .recv()
                .await
                .transpose()
                .with_kind(ErrorKind::Network)?,
            Some(Message::Close(_)) | None
        ) {}
        Ok(())
    }
}

impl From<AxumWebSocket> for WebSocket {
    fn from(ws: AxumWebSocket) -> Self {
        Self::new(ws)
    }
}

impl Stream for WebSocket {
    type Item = Result<Message, axum::Error>;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        self.get_mut().poll_recv(cx)
    }
}

impl Sink<Message> for WebSocket {
    type Error = axum::Error;

    fn poll_ready(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.get_mut().inner.poll_ready_unpin(cx)
    }

    fn start_send(self: Pin<&mut Self>, item: Message) -> Result<(), Self::Error> {
        self.get_mut().inner.start_send_unpin(item)
    }

    fn poll_flush(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.get_mut().inner.poll_flush_unpin(cx)
    }

    fn poll_close(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.get_mut().inner.poll_close_unpin(cx)
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
