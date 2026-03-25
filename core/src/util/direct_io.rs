use std::alloc::Layout;
use std::io::Write;
use std::os::fd::AsRawFd;
use std::pin::Pin;
use std::task::{Context, Poll};

use tokio::io::AsyncWrite;
use tokio::task::JoinHandle;

const BLOCK_SIZE: usize = 4096;
const BUF_CAP: usize = 1024 * 1024; // 1MiB

/// Aligned buffer for O_DIRECT I/O.
struct AlignedBuf {
    ptr: *mut u8,
    len: usize,
}

// SAFETY: We have exclusive ownership of the allocation.
unsafe impl Send for AlignedBuf {}

impl AlignedBuf {
    fn new() -> Self {
        let layout = Layout::from_size_align(BUF_CAP, BLOCK_SIZE).unwrap();
        // SAFETY: layout has non-zero size
        let ptr = unsafe { std::alloc::alloc(layout) };
        if ptr.is_null() {
            std::alloc::handle_alloc_error(layout);
        }
        Self { ptr, len: 0 }
    }

    fn as_slice(&self) -> &[u8] {
        // SAFETY: ptr is valid for len bytes, properly aligned, exclusively owned
        unsafe { std::slice::from_raw_parts(self.ptr, self.len) }
    }

    fn push(&mut self, data: &[u8]) -> usize {
        let n = data.len().min(BUF_CAP - self.len);
        // SAFETY: src and dst don't overlap, both valid for n bytes
        unsafe {
            std::ptr::copy_nonoverlapping(data.as_ptr(), self.ptr.add(self.len), n);
        }
        self.len += n;
        n
    }

    fn aligned_len(&self) -> usize {
        self.len & !(BLOCK_SIZE - 1)
    }

    fn drain_front(&mut self, n: usize) {
        debug_assert!(n <= self.len);
        let remaining = self.len - n;
        if remaining > 0 {
            // SAFETY: regions may overlap, so we use copy (memmove)
            unsafe {
                std::ptr::copy(self.ptr.add(n), self.ptr, remaining);
            }
        }
        self.len = remaining;
    }

    /// Extract aligned data into a new buffer for flushing, leaving remainder.
    fn take_aligned(&mut self) -> Option<(AlignedBuf, u64)> {
        let aligned = self.aligned_len();
        if aligned == 0 {
            return None;
        }
        let mut flush_buf = AlignedBuf::new();
        flush_buf.push(&self.as_slice()[..aligned]);
        self.drain_front(aligned);
        Some((flush_buf, aligned as u64))
    }
}

impl Drop for AlignedBuf {
    fn drop(&mut self) {
        let layout = Layout::from_size_align(BUF_CAP, BLOCK_SIZE).unwrap();
        // SAFETY: ptr was allocated with this layout in new()
        unsafe { std::alloc::dealloc(self.ptr, layout) };
    }
}

enum FileState {
    Idle(std::fs::File),
    Flushing(JoinHandle<std::io::Result<(std::fs::File, u64)>>),
    Done,
}

/// A file writer that uses O_DIRECT to bypass the kernel page cache.
///
/// Buffers writes in an aligned buffer and flushes to disk in the background.
/// New writes can proceed while a flush is in progress (double-buffering).
/// Progress is tracked via [`bytes_synced`](Self::bytes_synced), which reflects
/// bytes actually written to disk.
pub struct DirectIoFile {
    file_state: FileState,
    buf: AlignedBuf,
    synced: u64,
}

impl DirectIoFile {
    fn new(file: std::fs::File) -> Self {
        Self {
            file_state: FileState::Idle(file),
            buf: AlignedBuf::new(),
            synced: 0,
        }
    }

    /// Convert an existing tokio File into a DirectIoFile by adding O_DIRECT.
    pub async fn from_tokio_file(file: tokio::fs::File) -> std::io::Result<Self> {
        let std_file = file.into_std().await;
        let fd = std_file.as_raw_fd();
        // SAFETY: fd is valid, F_GETFL/F_SETFL are standard fcntl ops
        unsafe {
            let flags = libc::fcntl(fd, libc::F_GETFL);
            if flags == -1 {
                return Err(std::io::Error::last_os_error());
            }
            #[cfg(target_os = "linux")]
            if libc::fcntl(fd, libc::F_SETFL, flags | libc::O_DIRECT) == -1 {
                return Err(std::io::Error::last_os_error());
            }
        }
        Ok(Self::new(std_file))
    }

    /// Number of bytes confirmed written to disk.
    pub fn bytes_synced(&self) -> u64 {
        self.synced
    }

    /// Flush any remaining buffered data and sync to disk.
    ///
    /// Removes the O_DIRECT flag for the final partial-block write, then
    /// calls fsync. Updates `bytes_synced` to the final total.
    pub async fn sync_all(&mut self) -> std::io::Result<()> {
        // Wait for any in-flight flush
        self.await_flush().await?;

        let FileState::Idle(file) = std::mem::replace(&mut self.file_state, FileState::Done) else {
            return Ok(());
        };

        let mut buf = std::mem::replace(&mut self.buf, AlignedBuf::new());
        let remaining = buf.len as u64;

        tokio::task::spawn_blocking(move || {
            let mut file = file;

            // Write any aligned portion
            let aligned = buf.aligned_len();
            if aligned > 0 {
                let slice = unsafe { std::slice::from_raw_parts(buf.ptr, aligned) };
                file.write_all(slice)?;
                buf.drain_front(aligned);
            }

            // Write remainder with O_DIRECT disabled
            if buf.len > 0 {
                let fd = file.as_raw_fd();
                // SAFETY: fd is valid, F_GETFL/F_SETFL are standard fcntl ops
                #[cfg(target_os = "linux")]
                unsafe {
                    let flags = libc::fcntl(fd, libc::F_GETFL);
                    libc::fcntl(fd, libc::F_SETFL, flags & !libc::O_DIRECT);
                }
                file.write_all(buf.as_slice())?;
            }

            file.sync_all()
        })
        .await
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))??;

        self.synced += remaining;
        Ok(())
    }

    async fn await_flush(&mut self) -> std::io::Result<()> {
        if let FileState::Flushing(handle) = &mut self.file_state {
            let (file, flushed) = handle
                .await
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))??;
            self.synced += flushed;
            self.file_state = FileState::Idle(file);
        }
        Ok(())
    }

    /// Non-blocking poll: try to complete a pending flush.
    /// Returns Ready(Ok(())) if idle (or just became idle), Pending if still flushing.
    fn poll_complete_flush(&mut self, cx: &mut Context<'_>) -> Poll<std::io::Result<()>> {
        if let FileState::Flushing(handle) = &mut self.file_state {
            match Pin::new(handle).poll(cx) {
                Poll::Ready(Ok(Ok((file, flushed)))) => {
                    self.synced += flushed;
                    self.file_state = FileState::Idle(file);
                }
                Poll::Ready(Ok(Err(e))) => {
                    self.file_state = FileState::Done;
                    return Poll::Ready(Err(e));
                }
                Poll::Ready(Err(e)) => {
                    self.file_state = FileState::Done;
                    return Poll::Ready(Err(std::io::Error::new(std::io::ErrorKind::Other, e)));
                }
                Poll::Pending => return Poll::Pending,
            }
        }
        Poll::Ready(Ok(()))
    }

    /// Start a background flush of aligned data if the file is idle.
    fn maybe_start_flush(&mut self) {
        if !matches!(self.file_state, FileState::Idle(_)) {
            return;
        }
        let Some((flush_buf, count)) = self.buf.take_aligned() else {
            return;
        };
        let FileState::Idle(file) = std::mem::replace(&mut self.file_state, FileState::Done) else {
            unreachable!()
        };
        let handle = tokio::task::spawn_blocking(move || {
            let mut file = file;
            file.write_all(flush_buf.as_slice())?;
            Ok((file, count))
        });
        self.file_state = FileState::Flushing(handle);
    }
}

impl AsyncWrite for DirectIoFile {
    fn poll_write(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &[u8],
    ) -> Poll<std::io::Result<usize>> {
        // Try to complete any pending flush (non-blocking, registers waker)
        match self.poll_complete_flush(cx) {
            Poll::Ready(Err(e)) => return Poll::Ready(Err(e)),
            _ => {} // Pending is fine — we can still accept writes into the buffer
        }

        // If file just became idle and buffer has aligned data, start a flush
        // to free buffer space before accepting new data
        self.maybe_start_flush();

        // Accept data into the buffer
        let n = self.buf.push(buf);
        if n == 0 {
            // Buffer full, must wait for flush to complete and free space.
            // Waker was already registered by poll_complete_flush above.
            return Poll::Pending;
        }

        // If file is idle and we now have aligned data, start flushing
        self.maybe_start_flush();

        Poll::Ready(Ok(n))
    }

    fn poll_flush(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<std::io::Result<()>> {
        match self.poll_complete_flush(cx) {
            Poll::Pending => return Poll::Pending,
            Poll::Ready(Err(e)) => return Poll::Ready(Err(e)),
            Poll::Ready(Ok(())) => {}
        }

        if self.buf.aligned_len() > 0 {
            self.maybe_start_flush();
            // Poll the just-started flush
            return self.poll_complete_flush(cx).map(|r| r.map(|_| ()));
        }

        Poll::Ready(Ok(()))
    }

    fn poll_shutdown(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<std::io::Result<()>> {
        match self.poll_complete_flush(cx) {
            Poll::Pending => return Poll::Pending,
            Poll::Ready(Err(e)) => return Poll::Ready(Err(e)),
            Poll::Ready(Ok(())) => {}
        }

        self.file_state = FileState::Done;
        Poll::Ready(Ok(()))
    }
}
