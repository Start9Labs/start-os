pub use blake3::Hash;
use tokio::io::AsyncWrite;
use tokio_util::either::Either;

use crate::prelude::*;
use crate::util::io::{ParallelBlake3Writer, TeeWriter};

pub const BUFFER_CAPACITY: usize = 10 * 1024 * 1024; // 10MiB

#[pin_project::pin_project]
pub struct VerifyingWriter<W> {
    verify: Option<Hash>,
    #[pin]
    writer: Either<TeeWriter<W, ParallelBlake3Writer>, W>,
}
impl<W: AsyncWrite> VerifyingWriter<W> {
    pub fn new(w: W, verify: Option<Hash>) -> Self {
        Self {
            writer: if verify.is_some() {
                Either::Left(TeeWriter::new(
                    w,
                    ParallelBlake3Writer::new(BUFFER_CAPACITY),
                    BUFFER_CAPACITY,
                ))
            } else {
                Either::Right(w)
            },
            verify,
        }
    }
}
impl<W: AsyncWrite + Unpin> VerifyingWriter<W> {
    pub async fn verify(self) -> Result<W, Error> {
        match self.writer {
            Either::Left(writer) => {
                let (writer, actual) = writer.into_inner().await?;
                if let Some(expected) = self.verify {
                    ensure_code!(
                        actual.finalize().await? == expected,
                        ErrorKind::InvalidSignature,
                        "hash sum does not match"
                    );
                }
                Ok(writer)
            }
            Either::Right(writer) => Ok(writer),
        }
    }
}
impl<W: AsyncWrite> AsyncWrite for VerifyingWriter<W> {
    fn poll_write(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> std::task::Poll<Result<usize, std::io::Error>> {
        self.project().writer.poll_write(cx, buf)
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
