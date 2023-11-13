pub use blake3::Hash;
use blake3::Hasher;
use tokio::io::AsyncWrite;

use crate::prelude::*;

#[pin_project::pin_project]
pub struct HashWriter {
    hasher: Hasher,
}
impl HashWriter {
    pub fn new() -> Self {
        Self {
            hasher: Hasher::new(),
        }
    }
    pub fn finalize(self) -> Hash {
        self.hasher.finalize()
    }
}
impl AsyncWrite for HashWriter {
    fn poll_write(
        self: std::pin::Pin<&mut Self>,
        _cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> std::task::Poll<Result<usize, std::io::Error>> {
        self.project().hasher.update(buf);
        std::task::Poll::Ready(Ok(buf.len()))
    }
    fn poll_flush(
        self: std::pin::Pin<&mut Self>,
        _cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), std::io::Error>> {
        std::task::Poll::Ready(Ok(()))
    }
    fn poll_shutdown(
        self: std::pin::Pin<&mut Self>,
        _cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), std::io::Error>> {
        std::task::Poll::Ready(Ok(()))
    }
}

#[pin_project::pin_project]
pub struct VerifyingWriter<W> {
    verify: Option<(Hasher, Hash)>,
    #[pin]
    writer: W,
}
impl<W: AsyncWrite> VerifyingWriter<W> {
    pub fn new(w: W, verify: Option<Hash>) -> Self {
        Self {
            verify: verify.map(|v| (Hasher::new(), v)),
            writer: w,
        }
    }
    pub fn verify(self) -> Result<W, Error> {
        if let Some((actual, expected)) = self.verify {
            ensure_code!(
                actual.finalize() == expected,
                ErrorKind::InvalidSignature,
                "hash sum does not match"
            );
        }
        Ok(self.writer)
    }
}
impl<W: AsyncWrite> AsyncWrite for VerifyingWriter<W> {
    fn poll_write(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> std::task::Poll<Result<usize, std::io::Error>> {
        let this = self.project();
        match this.writer.poll_write(cx, buf) {
            std::task::Poll::Ready(Ok(written)) => {
                if let Some((h, _)) = this.verify {
                    h.update(&buf[..written]);
                }
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
