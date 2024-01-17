use std::io::SeekFrom;
use std::os::fd::{AsRawFd, RawFd};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use tokio::fs::File;
use tokio::io::AsyncRead;
use tokio::sync::{Mutex, OwnedMutexGuard};

use crate::disk::mount::filesystem::loop_dev::LoopDev;
use crate::prelude::*;
use crate::s9pk::merkle_archive::source::{ArchiveSource, Section};

fn path_from_fd(fd: RawFd) -> PathBuf {
    Path::new("/proc/self/fd").join(fd.to_string())
}

#[derive(Clone)]
pub struct MultiCursorFile {
    fd: RawFd,
    file: Arc<Mutex<File>>,
}
impl MultiCursorFile {
    fn path(&self) -> PathBuf {
        path_from_fd(self.fd)
    }
    pub async fn open(fd: &impl AsRawFd) -> Result<Self, Error> {
        let fd = fd.as_raw_fd();
        Ok(Self {
            fd,
            file: Arc::new(Mutex::new(File::open(path_from_fd(fd)).await?)),
        })
    }
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
pub struct FileSectionReader {
    #[pin]
    file: OwnedMutexGuard<File>,
    remaining: u64,
}
impl AsyncRead for FileSectionReader {
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
    type Reader = FileSectionReader;
    async fn size(&self) -> Option<u64> {
        tokio::fs::metadata(self.path()).await.ok().map(|m| m.len())
    }
    async fn fetch(&self, position: u64, size: u64) -> Result<Self::Reader, Error> {
        use tokio::io::AsyncSeekExt;

        let mut file = if let Ok(file) = self.file.clone().try_lock_owned() {
            file
        } else {
            Arc::new(Mutex::new(File::open(self.path()).await?))
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

impl From<&Section<MultiCursorFile>> for LoopDev<PathBuf> {
    fn from(value: &Section<MultiCursorFile>) -> Self {
        LoopDev::new(value.source.path(), value.position, value.size)
    }
}
