use std::io::SeekFrom;
use std::os::fd::{AsRawFd, RawFd};
use std::path::Path;
use std::sync::Arc;

use tokio::fs::File;
use tokio::io::AsyncRead;
use tokio::sync::{Mutex, OwnedMutexGuard};

use crate::prelude::*;
use crate::s9pk::merkle_archive::source::ArchiveSource;

#[derive(Clone)]
pub struct MultiCursorFile {
    fd: RawFd,
    file: Arc<Mutex<File>>,
}
impl From<File> for MultiCursorFile {
    fn from(value: File) -> Self {
        Self {
            fd: value.as_raw_fd(),
            file: Arc::new(Mutex::new(value)),
        }
    }
}

#[pin_project::pin_project]
pub struct FileSection {
    #[pin]
    file: OwnedMutexGuard<File>,
    remaining: u64,
}
impl AsyncRead for FileSection {
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut tokio::io::ReadBuf<'_>,
    ) -> std::task::Poll<std::io::Result<()>> {
        let this = self.project();
        if *this.remaining == 0 {
            return std::task::Poll::Ready(Ok(()));
        }
        let before = buf.filled().len() as u64;
        let res = std::pin::Pin::new(&mut **this.file.get_mut())
            .poll_read(cx, &mut buf.take(*this.remaining as usize));
        *this.remaining = this
            .remaining
            .saturating_sub(buf.filled().len() as u64 - before);
        res
    }
}

#[async_trait::async_trait]
impl ArchiveSource for MultiCursorFile {
    const TRUSTED: bool = true;
    type Reader = FileSection;
    async fn fetch(&self, position: u64, size: u64) -> Result<Self::Reader, Error> {
        use tokio::io::AsyncSeekExt;

        let mut file = if let Ok(file) = self.file.clone().try_lock_owned() {
            file
        } else {
            Arc::new(Mutex::new(
                File::open(Path::new("/proc/self/fd").join(self.fd.to_string())).await?,
            ))
            .try_lock_owned()
            .expect("freshly created")
        };
        file.seek(SeekFrom::Start(position)).await?;
        Ok(Self::Reader {
            file,
            remaining: size,
        })
    }
}
