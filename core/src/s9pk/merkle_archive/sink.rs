use tokio::io::{AsyncSeek, AsyncWrite};

use crate::prelude::*;
use crate::util::io::TrackingIO;

pub trait Sink: AsyncWrite + Unpin + Send {
    fn current_position(&mut self) -> impl Future<Output = Result<u64, Error>> + Send + '_;
}

impl<S: AsyncWrite + AsyncSeek + Unpin + Send> Sink for S {
    async fn current_position(&mut self) -> Result<u64, Error> {
        use tokio::io::AsyncSeekExt;

        Ok(self.stream_position().await?)
    }
}

impl<W: AsyncWrite + Unpin + Send> Sink for TrackingIO<W> {
    async fn current_position(&mut self) -> Result<u64, Error> {
        Ok(self.position())
    }
}
