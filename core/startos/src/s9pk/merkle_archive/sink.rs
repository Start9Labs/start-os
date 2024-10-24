use tokio::io::{AsyncSeek, AsyncWrite};

use crate::prelude::*;
use crate::util::io::TrackingIO;

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
impl<W: AsyncWrite + Unpin + Send> Sink for TrackingIO<W> {
    async fn current_position(&mut self) -> Result<u64, Error> {
        Ok(self.position())
    }
}
