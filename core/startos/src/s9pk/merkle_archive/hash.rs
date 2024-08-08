use std::task::Poll;

use blake3::Hash;
use tokio::io::AsyncWrite;
use tokio_util::either::Either;

use crate::prelude::*;
use crate::util::io::{ParallelBlake3Writer, TeeWriter};
use crate::CAP_10_MiB;

#[pin_project::pin_project]
pub struct VerifyingWriter<W> {
    verify: Option<(Hash, u64)>,
    #[pin]
    writer: Either<TeeWriter<W, ParallelBlake3Writer>, W>,
}
impl<W: AsyncWrite> VerifyingWriter<W> {
    pub fn new(w: W, verify: Option<(Hash, u64)>) -> Self {
        Self {
            writer: if verify.is_some() {
                Either::Left(TeeWriter::new(
                    w,
                    ParallelBlake3Writer::new(CAP_10_MiB),
                    CAP_10_MiB,
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
                if let Some((expected, remaining)) = self.verify {
                    ensure_code!(
                        actual.finalize().await? == expected,
                        ErrorKind::InvalidSignature,
                        "hash sum mismatch"
                    );
                    ensure_code!(
                        remaining == 0,
                        ErrorKind::InvalidSignature,
                        "file size mismatch"
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
    ) -> Poll<Result<usize, std::io::Error>> {
        let this = self.project();
        if let Some((_, remaining)) = this.verify {
            if *remaining < buf.len() as u64 {
                return Poll::Ready(Err(std::io::Error::other(eyre!(
                    "attempted to write more bytes than signed"
                ))));
            }
        }
        match this.writer.poll_write(cx, buf)? {
            Poll::Pending => Poll::Pending,
            Poll::Ready(n) => {
                if let Some((_, remaining)) = this.verify {
                    *remaining -= n as u64;
                }
                Poll::Ready(Ok(n))
            }
        }
    }
    fn poll_flush(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Result<(), std::io::Error>> {
        self.project().writer.poll_flush(cx)
    }
    fn poll_shutdown(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Result<(), std::io::Error>> {
        self.project().writer.poll_shutdown(cx)
    }
}
