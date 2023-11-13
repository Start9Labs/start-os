use std::task::Poll;

use tokio::io::{AsyncRead, ReadBuf};

#[pin_project::pin_project]
pub struct ByteReplacementReader<R> {
    pub replace: u8,
    pub with: u8,
    #[pin]
    pub inner: R,
}
impl<R: AsyncRead> AsyncRead for ByteReplacementReader<R> {
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> std::task::Poll<std::io::Result<()>> {
        let this = self.project();
        match this.inner.poll_read(cx, buf) {
            Poll::Ready(Ok(())) => {
                for idx in 0..buf.filled().len() {
                    if buf.filled()[idx] == *this.replace {
                        buf.filled_mut()[idx] = *this.with;
                    }
                }
                Poll::Ready(Ok(()))
            }
            a => a,
        }
    }
}
