use std::io::{Seek, Write};
use std::path::Path;
use std::task::Poll;

use async_compression::codecs::{Encode, ZstdEncoder};
use async_compression::core::util::PartialBuffer;
use futures::{ready, TryStreamExt};
use tokio::io::{AsyncSeek, AsyncWrite};
use visit_rs::{Visit, VisitAsync, VisitFields, VisitFieldsAsync, Visitor};

use crate::prelude::*;
use crate::registry::os::asset::add;

struct SquashfsSerializer<W> {
    writer: W,
}
impl<W> Visitor for SquashfsSerializer<W> {
    type Result = Result<(), Error>;
}

macro_rules! impl_visit_le {
    ($($ty:ty),*) => {
        $(
            impl<W: AsyncWrite + Unpin + Send> VisitAsync<SquashfsSerializer<W>> for $ty {
                async fn visit_async(&self, visitor: &mut SquashfsSerializer<W>) -> Result<(), Error> {
                    use tokio::io::AsyncWriteExt;
                    visitor.writer.write_all(&self.to_le_bytes()).await?;
                    Ok(())
                }
            }
            impl<W: Write> Visit<SquashfsSerializer<W>> for $ty {
                fn visit(&self, visitor: &mut SquashfsSerializer<W>) -> Result<(), Error> {
                    visitor.writer.write_all(&self.to_le_bytes())?;
                    Ok(())
                }
            }
        )*
    };
}

impl_visit_le!(u8, u16, u32, u64, i8, i16, i32, i64, f32, f64);

#[derive(VisitFields)]
struct Superblock {
    magic: u32, // 0x73717368
    inode_count: u32,
    modification_time: u32, // 0
    block_size: u32,
    fragment_entry_count: u32,
    compression_id: u16, // 6 = zstd
    block_log: u16,      // log2(block_size)
    flags: u16,          // 0x0440
    id_count: u16,
    version_major: u16, // 4
    version_minor: u16, // 0
    root_inode_ref: u64,
    bytes_used: u64,
    id_table_start: u64,
    xattr_id_table_start: u64,
    inode_table_start: u64,
    directory_table_start: u64,
    fragment_table_start: u64,
    export_table_start: u64,
}
impl Default for Superblock {
    fn default() -> Self {
        Self {
            magic: 0x73717368,
            inode_count: 0,
            modification_time: 0,
            block_size: 0,
            fragment_entry_count: 0,
            compression_id: 6,
            block_log: 0,
            flags: 0x0440,
            id_count: 0,
            version_major: 4,
            version_minor: 0,
            root_inode_ref: 0,
            bytes_used: 0,
            id_table_start: 0,
            xattr_id_table_start: 0,
            inode_table_start: 0,
            directory_table_start: 0,
            fragment_table_start: 0,
            export_table_start: 0,
        }
    }
}
impl<W: AsyncWrite + Unpin + Send> VisitAsync<SquashfsSerializer<W>> for Superblock {
    async fn visit_async(&self, visitor: &mut SquashfsSerializer<W>) -> Result<(), Error> {
        self.visit_fields_async(visitor).try_collect().await
    }
}
impl<W: Write> Visit<SquashfsSerializer<W>> for Superblock {
    fn visit(&self, visitor: &mut SquashfsSerializer<W>) -> Result<(), Error> {
        self.visit_fields(visitor).collect()
    }
}

#[pin_project::pin_project]
pub struct MetadataBlocks<W> {
    input: [u8; 8192],
    input_flushed: usize,
    size: usize,
    size_addr: Option<u64>,
    end_addr: Option<u64>,
    zstd: Option<ZstdEncoder>,
    output: PartialBuffer<[u8; 4096]>,
    output_flushed: usize,
    #[pin]
    writer: W,
}
impl<W: Write + Seek> Write for MetadataBlocks<W> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let n = buf.len().min(self.input.len() - self.size);
        self.input[self.size..self.size + n].copy_from_slice(&buf[..n]);
        if n < buf.len() {
            self.flush()?;
        }
        Ok(n)
    }
    fn flush(&mut self) -> std::io::Result<()> {
        if self.size > 0 {
            if self.size_addr.is_none() {
                self.size_addr = Some(self.writer.stream_position()?);
                self.output.unwritten_mut()[..2].copy_from_slice(&[0; 2]);
                self.output.advance(2);
            }
            if self.output.written().len() > self.output_flushed {
                let n = self
                    .writer
                    .write(&self.output.written()[self.output_flushed..])?;
                self.output_flushed += n;
            }
            if self.output.written().len() == self.output_flushed {
                self.output_flushed = 0;
                self.output.reset();
            }
            if self.input_flushed < self.size {
                if !self.output.unwritten().is_empty() {
                    let mut input = PartialBuffer::new(&self.input[self.input_flushed..self.size]);
                    self.zstd
                        .get_or_insert_with(|| ZstdEncoder::new(22))
                        .encode(&mut input, &mut self.output)?;
                    self.input_flushed += input.written().len();
                }
            } else {
                if !self.output.unwritten().is_empty() {
                    if self.zstd.as_mut().unwrap().finish(&mut self.output)? {
                        self.zstd = None;
                    }
                }
                if self.zstd.is_none() && self.output.written().len() == self.output_flushed {
                    self.output_flushed = 0;
                    self.output.reset();
                    if let Some(addr) = self.size_addr {
                        let end_addr = if let Some(end_addr) = self.end_addr {
                            end_addr
                        } else {
                            let end_addr = self.writer.stream_position()?;
                            self.end_addr = Some(end_addr);
                            end_addr
                        };
                        self.writer.seek(std::io::SeekFrom::Start(addr))?;
                        self.output.unwritten_mut()[..2]
                            .copy_from_slice(&((end_addr - addr - 2) as u16).to_le_bytes());
                        self.output.advance(2);
                        self.size_addr = None;
                    }
                    if let Some(end_addr) = self.end_addr {
                        self.writer.seek(std::io::SeekFrom::Start(end_addr))?;
                        self.end_addr = None;
                        self.input_flushed = 0;
                        self.size = 0;
                    }
                }
            }
        }
        Ok(())
    }
}
impl<W: AsyncWrite + AsyncSeek> AsyncWrite for MetadataBlocks<W> {
    fn poll_write(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> std::task::Poll<std::io::Result<usize>> {
        let this = self.as_mut().project();
        let n = buf.len().min(this.input.len() - *this.size);
        this.input[*this.size..*this.size + n].copy_from_slice(&buf[..n]);
        if n < buf.len() {
            ready!(self.poll_flush(cx)?);
        }
        Poll::Ready(Ok(n))
    }

    fn poll_flush(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<std::io::Result<()>> {
        // let this = self.as_mut();
        // if self.size > 0 {
        //     if self.size_addr.is_none() {
        //         self.size_addr = Some(self.writer.stream_position()?);
        //         self.output.unwritten_mut()[..2].copy_from_slice(&[0; 2]);
        //         self.output.advance(2);
        //     }
        //     if self.output.written().len() > self.output_flushed {
        //         let n = self
        //             .writer
        //             .write(&self.output.written()[self.output_flushed..])?;
        //         self.output_flushed += n;
        //     }
        //     if self.output.written().len() == self.output_flushed {
        //         self.output_flushed = 0;
        //         self.output.reset();
        //     }
        //     if self.input_flushed < self.size {
        //         if !self.output.unwritten().is_empty() {
        //             let mut input = PartialBuffer::new(&self.input[self.input_flushed..self.size]);
        //             self.zstd
        //                 .get_or_insert_with(|| ZstdEncoder::new(22))
        //                 .encode(&mut input, &mut self.output)?;
        //             self.input_flushed += input.written().len();
        //         }
        //     } else {
        //         if !self.output.unwritten().is_empty() {
        //             if self.zstd.as_mut().unwrap().finish(&mut self.output)? {
        //                 self.zstd = None;
        //             }
        //         }
        //         if self.zstd.is_none() && self.output.written().len() == self.output_flushed {
        //             self.output_flushed = 0;
        //             self.output.reset();
        //             if let Some(addr) = self.size_addr {
        //                 let end_addr = if let Some(end_addr) = self.end_addr {
        //                     end_addr
        //                 } else {
        //                     let end_addr = self.writer.stream_position()?;
        //                     self.end_addr = Some(end_addr);
        //                     end_addr
        //                 };
        //                 self.writer.seek(std::io::SeekFrom::Start(addr))?;
        //                 self.output.unwritten_mut()[..2]
        //                     .copy_from_slice(&((end_addr - addr - 2) as u16).to_le_bytes());
        //                 self.output.advance(2);
        //                 self.size_addr = None;
        //             }
        //             if let Some(end_addr) = self.end_addr {
        //                 self.writer.seek(std::io::SeekFrom::Start(end_addr))?;
        //                 self.end_addr = None;
        //                 self.input_flushed = 0;
        //                 self.size = 0;
        //             }
        //         }
        //     }
        // }
        Poll::Ready(Ok(()))
    }

    fn poll_shutdown(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<std::io::Result<()>> {
        std::task::Poll::Ready(Ok(()))
    }
}
