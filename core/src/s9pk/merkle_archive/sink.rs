use tokio::io::{AsyncSeek, AsyncWrite};

use crate::prelude::*;

#[async_trait::async_trait]
pub trait Sink: AsyncWrite + Unpin + Send {
    async fn current_position(&mut self) -> Result<u64, Error>;
}

#[async_trait::async_trait]
impl<S: AsyncWrite + AsyncSeek + Unpin + Send> Sink for S {
    async fn current_position(&mut self) -> Result<u64, Error> {
        use tokio::io::AsyncSeekExt;

        Ok(self.stream_position().await?)
    }
}

#[async_trait::async_trait]
impl<W: AsyncWrite + Unpin + Send> Sink for TrackingWriter<W> {
    async fn current_position(&mut self) -> Result<u64, Error> {
        Ok(self.position)
    }
}

#[pin_project::pin_project]
pub struct TrackingWriter<W> {
    position: u64,
    #[pin]
    writer: W,
}
impl<W> TrackingWriter<W> {
    pub fn new(start: u64, w: W) -> Self {
        Self {
            position: start,
            writer: w,
        }
    }
    pub fn into_inner(self) -> W {
        self.writer
    }
}
impl<W: AsyncWrite + Unpin + Send> AsyncWrite for TrackingWriter<W> {
    fn poll_write(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> std::task::Poll<Result<usize, std::io::Error>> {
        let this = self.project();
        match this.writer.poll_write(cx, buf) {
            std::task::Poll::Ready(Ok(written)) => {
                *this.position += written as u64;
                std::task::Poll::Ready(Ok(written))
            }
            a => a,
        }
    }
    fn poll_flush(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), std::io::Error>> {
        self.project().writer.poll_flush(cx)
    }
    fn poll_shutdown(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), std::io::Error>> {
        self.project().writer.poll_shutdown(cx)
    }
}
