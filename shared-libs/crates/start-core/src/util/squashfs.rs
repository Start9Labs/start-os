use std::io::{Seek, SeekFrom, Write};
use std::task::Poll;

use async_compression::codecs::{Encode, ZstdEncoder};
use async_compression::core::util::PartialBuffer;
use futures::{TryStreamExt, ready};
use tokio::io::{AsyncSeek, AsyncWrite};
use visit_rs::{Visit, VisitAsync, VisitFields, VisitFieldsAsync, Visitor};

use crate::prelude::*;

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
pub struct MetadataBlocksWriter<W> {
    input: PartialBuffer<[u8; 8192]>,
    size_addr: Option<u64>,
    output: PartialBuffer<[u8; 8192]>,
    output_flushed: usize,
    zstd: Option<ZstdEncoder>,
    seek_state: SeekState,
    write_state: WriteState,
    #[pin]
    writer: W,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SeekState {
    Idle,
    GettingPosition,
    Seeking(u64),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum WriteState {
    Idle,
    WritingSizeHeader(u16),
    WritingOutput(Box<Self>),
    EncodingInput,
    SeekingToEnd(u64),
}

fn poll_seek_helper<W: AsyncSeek>(
    mut writer: std::pin::Pin<&mut W>,
    seek_state: &mut SeekState,
    cx: &mut std::task::Context<'_>,
    pos: u64,
) -> std::task::Poll<std::io::Result<u64>> {
    match *seek_state {
        SeekState::Idle => {
            writer.as_mut().start_seek(std::io::SeekFrom::Start(pos))?;
            *seek_state = SeekState::Seeking(pos);
            match writer.as_mut().poll_complete(cx)? {
                Poll::Ready(result) => {
                    *seek_state = SeekState::Idle;
                    Poll::Ready(Ok(result))
                }
                Poll::Pending => Poll::Pending,
            }
        }
        SeekState::Seeking(target) if target == pos => {
            let result = ready!(writer.as_mut().poll_complete(cx))?;
            *seek_state = SeekState::Idle;
            Poll::Ready(Ok(result))
        }
        SeekState::Seeking(old_target) => {
            tracing::warn!(
                "poll_seek({}) called while seeking to {}, canceling previous seek",
                pos,
                old_target
            );
            writer.as_mut().start_seek(std::io::SeekFrom::Start(pos))?;
            *seek_state = SeekState::Seeking(pos);
            match writer.as_mut().poll_complete(cx)? {
                Poll::Ready(result) => {
                    *seek_state = SeekState::Idle;
                    Poll::Ready(Ok(result))
                }
                Poll::Pending => Poll::Pending,
            }
        }
        SeekState::GettingPosition => {
            tracing::warn!(
                "poll_seek({}) called while getting stream position, canceling",
                pos
            );
            writer.as_mut().start_seek(std::io::SeekFrom::Start(pos))?;
            *seek_state = SeekState::Seeking(pos);
            match writer.as_mut().poll_complete(cx)? {
                Poll::Ready(result) => {
                    *seek_state = SeekState::Idle;
                    Poll::Ready(Ok(result))
                }
                Poll::Pending => Poll::Pending,
            }
        }
    }
}

fn poll_stream_position_helper<W: AsyncSeek>(
    mut writer: std::pin::Pin<&mut W>,
    seek_state: &mut SeekState,
    cx: &mut std::task::Context<'_>,
) -> std::task::Poll<std::io::Result<u64>> {
    match *seek_state {
        SeekState::Idle => {
            writer.as_mut().start_seek(std::io::SeekFrom::Current(0))?;
            *seek_state = SeekState::GettingPosition;
            match writer.as_mut().poll_complete(cx)? {
                Poll::Ready(result) => {
                    *seek_state = SeekState::Idle;
                    Poll::Ready(Ok(result))
                }
                Poll::Pending => Poll::Pending,
            }
        }
        SeekState::GettingPosition => {
            let result = ready!(writer.as_mut().poll_complete(cx))?;
            *seek_state = SeekState::Idle;
            Poll::Ready(Ok(result))
        }
        SeekState::Seeking(target) => {
            tracing::warn!(
                "poll_stream_position called while seeking to {}, canceling",
                target
            );
            writer.as_mut().start_seek(std::io::SeekFrom::Current(0))?;
            *seek_state = SeekState::GettingPosition;
            match writer.as_mut().poll_complete(cx)? {
                Poll::Ready(result) => {
                    *seek_state = SeekState::Idle;
                    Poll::Ready(Ok(result))
                }
                Poll::Pending => Poll::Pending,
            }
        }
    }
}

impl<W: Write + Seek> Write for MetadataBlocksWriter<W> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let n = self.input.copy_unwritten_from(&mut PartialBuffer::new(buf));
        if n < buf.len() {
            self.flush()?;
        }
        Ok(n)
    }
    fn flush(&mut self) -> std::io::Result<()> {
        loop {
            match &self.write_state {
                WriteState::Idle => {
                    if self.input.written().is_empty() {
                        return Ok(());
                    }
                    self.write_state = WriteState::WritingSizeHeader(0);
                }

                WriteState::WritingSizeHeader(size) => {
                    let done = if let Some(size_addr) = self.size_addr {
                        self.writer.seek(SeekFrom::Start(size_addr))?;
                        Some(size_addr + 2 + *size as u64)
                    } else {
                        self.size_addr = Some(self.writer.stream_position()?);
                        None
                    };
                    self.output.unwritten_mut()[..2].copy_from_slice(&u16::to_le_bytes(*size)[..]);
                    self.output.advance(2);
                    self.write_state =
                        WriteState::WritingOutput(Box::new(if let Some(end) = done {
                            WriteState::SeekingToEnd(end)
                        } else {
                            WriteState::EncodingInput
                        }));
                }

                WriteState::WritingOutput(next) => {
                    if self.output.written().len() > self.output_flushed {
                        let n = self
                            .writer
                            .write(&self.output.written()[self.output_flushed..])?;
                        self.output_flushed += n;
                    } else {
                        self.output.reset();
                        self.output_flushed = 0;
                        self.write_state = *next.clone();
                    }
                }

                WriteState::EncodingInput => {
                    let encoder = self.zstd.get_or_insert_with(|| ZstdEncoder::new(22));
                    encoder.encode(
                        &mut PartialBuffer::new(&self.input.written()),
                        &mut self.output,
                    )?;
                    let compressed = if !encoder.finish(&mut self.output)? {
                        std::mem::swap(&mut self.output, &mut self.input);
                        false
                    } else {
                        true
                    };
                    self.zstd = None;
                    self.input.reset();
                    self.write_state =
                        WriteState::WritingOutput(Box::new(WriteState::WritingSizeHeader(
                            self.output.written().len() as u16
                                | if compressed { 0 } else { 0x8000 },
                        )));
                }

                WriteState::SeekingToEnd(end_addr) => {
                    self.writer.seek(std::io::SeekFrom::Start(*end_addr))?;
                    self.size_addr = None;
                    self.write_state = WriteState::Idle;
                    return Ok(());
                }
            }
        }
    }
}

impl<W: AsyncWrite + AsyncSeek> AsyncWrite for MetadataBlocksWriter<W> {
    fn poll_write(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> std::task::Poll<std::io::Result<usize>> {
        let this = self.as_mut().project();
        let n = this.input.copy_unwritten_from(&mut PartialBuffer::new(buf));
        if n < buf.len() {
            ready!(self.poll_flush(cx))?;
        }
        Poll::Ready(Ok(n))
    }

    fn poll_flush(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<std::io::Result<()>> {
        loop {
            let mut this = self.as_mut().project();
            match this.write_state.clone() {
                WriteState::Idle => {
                    if this.input.written().is_empty() {
                        return Poll::Ready(Ok(()));
                    }
                    *this.write_state = WriteState::WritingSizeHeader(0);
                }

                WriteState::WritingSizeHeader(size) => {
                    let done = if let Some(size_addr) = *this.size_addr {
                        ready!(poll_seek_helper(
                            this.writer.as_mut(),
                            this.seek_state,
                            cx,
                            size_addr
                        ))?;
                        Some(size_addr + 2 + size as u64)
                    } else {
                        let pos = ready!(poll_stream_position_helper(
                            this.writer.as_mut(),
                            this.seek_state,
                            cx
                        ))?;
                        *this.size_addr = Some(pos);
                        None
                    };
                    this.output.unwritten_mut()[..2]
                        .copy_from_slice(&u16::to_le_bytes(size)[..]);
                    this.output.advance(2);
                    *this.write_state = WriteState::WritingOutput(Box::new(if let Some(end) = done {
                        WriteState::SeekingToEnd(end)
                    } else {
                        WriteState::EncodingInput
                    }));
                }

                WriteState::WritingOutput(next) => {
                    if this.output.written().len() > *this.output_flushed {
                        let n = ready!(this
                            .writer
                            .as_mut()
                            .poll_write(cx, &this.output.written()[*this.output_flushed..]))?;
                        *this.output_flushed += n;
                    } else {
                        this.output.reset();
                        *this.output_flushed = 0;
                        *this.write_state = *next;
                    }
                }

                WriteState::EncodingInput => {
                    let encoder = this.zstd.get_or_insert_with(|| ZstdEncoder::new(22));
                    encoder.encode(
                        &mut PartialBuffer::new(this.input.written()),
                        this.output,
                    )?;
                    let compressed = if !encoder.finish(this.output)? {
                        std::mem::swap(this.output, this.input);
                        false
                    } else {
                        true
                    };
                    *this.zstd = None;
                    this.input.reset();
                    *this.write_state = WriteState::WritingOutput(Box::new(
                        WriteState::WritingSizeHeader(
                            this.output.written().len() as u16
                                | if compressed { 0 } else { 0x8000 },
                        ),
                    ));
                }

                WriteState::SeekingToEnd(end_addr) => {
                    ready!(poll_seek_helper(
                        this.writer.as_mut(),
                        this.seek_state,
                        cx,
                        end_addr
                    ))?;
                    *this.size_addr = None;
                    *this.write_state = WriteState::Idle;
                    return Poll::Ready(Ok(()));
                }
            }
        }
    }

    fn poll_shutdown(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<std::io::Result<()>> {
        self.poll_flush(cx)
    }
}

impl<W> MetadataBlocksWriter<W> {
    pub fn new(writer: W) -> Self {
        Self {
            input: PartialBuffer::new([0; 8192]),
            size_addr: None,
            output: PartialBuffer::new([0; 8192]),
            output_flushed: 0,
            zstd: None,
            seek_state: SeekState::Idle,
            write_state: WriteState::Idle,
            writer,
        }
    }
}

use async_compression::codecs::{Decode, ZstdDecoder};
use tokio::io::AsyncRead;

#[pin_project::pin_project]
pub struct MetadataBlocksReader<R> {
    #[pin]
    reader: R,
    size_buf: PartialBuffer<[u8; 2]>,
    compressed: PartialBuffer<[u8; 8192]>,
    compressed_size: usize,
    is_compressed: bool,
    output: PartialBuffer<[u8; 8192]>,
    output_pos: usize,
    zstd: Option<ZstdDecoder>,
    state: ReadState,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ReadState {
    ReadingSize,
    ReadingData,
    Decompressing,
    Outputting,
    Eof,
}

impl<R> MetadataBlocksReader<R> {
    pub fn new(reader: R) -> Self {
        Self {
            reader,
            size_buf: PartialBuffer::new([0; 2]),
            compressed: PartialBuffer::new([0; 8192]),
            compressed_size: 0,
            is_compressed: false,
            output: PartialBuffer::new([0; 8192]),
            output_pos: 0,
            zstd: None,
            state: ReadState::ReadingSize,
        }
    }
}

use std::io::Read;

impl<R: Read> Read for MetadataBlocksReader<R> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        loop {
            match self.state {
                ReadState::ReadingSize => {
                    let n = self.reader.read(self.size_buf.unwritten_mut())?;
                    if n == 0 {
                        if self.size_buf.written().is_empty() {
                            self.state = ReadState::Eof;
                            return Ok(0);
                        } else {
                            return Err(std::io::Error::new(
                                std::io::ErrorKind::UnexpectedEof,
                                "Unexpected EOF reading size header",
                            ));
                        }
                    }

                    self.size_buf.advance(n);

                    if self.size_buf.written().len() < 2 {
                        continue;
                    }

                    let size_header = u16::from_le_bytes([
                        self.size_buf.written()[0],
                        self.size_buf.written()[1],
                    ]);
                    self.is_compressed = (size_header & 0x8000) == 0;
                    self.compressed_size = (size_header & 0x7FFF) as usize;

                    if self.compressed_size == 0 || self.compressed_size > 8192 {
                        return Err(std::io::Error::new(
                            std::io::ErrorKind::InvalidData,
                            format!("Invalid metadata block size: {}", self.compressed_size),
                        ));
                    }

                    self.compressed.reset();
                    self.size_buf.reset();
                    self.state = ReadState::ReadingData;
                    continue;
                }

                ReadState::ReadingData => {
                    let n = self.reader.read(self.compressed.unwritten_mut())?;
                    if n == 0 {
                        return Err(std::io::Error::new(
                            std::io::ErrorKind::UnexpectedEof,
                            "Unexpected EOF reading data",
                        ));
                    }

                    self.compressed.advance(n);

                    if !self.compressed.unwritten().is_empty() {
                        continue;
                    }

                    self.output_pos = 0;
                    self.output.reset();
                    if self.is_compressed {
                        self.zstd = Some(ZstdDecoder::new());
                        self.state = ReadState::Decompressing;
                    } else {
                        self.output
                            .copy_unwritten_from(&mut PartialBuffer::new(self.compressed.written()));
                        self.state = ReadState::Outputting;
                    }
                    continue;
                }

                ReadState::Decompressing => {
                    if self.output.unwritten().is_empty() {
                        self.state = ReadState::Outputting;
                        continue;
                    }

                    let mut input = PartialBuffer::new(self.compressed.written());
                    let decoder = self.zstd.as_mut().unwrap();

                    if decoder.decode(&mut input, &mut self.output)? {
                        self.zstd = None;
                        self.state = ReadState::Outputting;
                    }
                    continue;
                }

                ReadState::Outputting => {
                    let available = self.output.written().len() - self.output_pos;
                    if available == 0 {
                        if self.zstd.is_none() {
                            self.state = ReadState::ReadingSize;
                            continue;
                        } else {
                            self.output.reset();
                            self.output_pos = 0;
                            self.state = ReadState::Decompressing;
                            continue;
                        }
                    }

                    let to_copy = available.min(buf.len());
                    buf[..to_copy].copy_from_slice(
                        &self.output.written()[self.output_pos..self.output_pos + to_copy],
                    );
                    self.output_pos += to_copy;
                    return Ok(to_copy);
                }

                ReadState::Eof => {
                    return Ok(0);
                }
            }
        }
    }
}

impl<R: AsyncRead> AsyncRead for MetadataBlocksReader<R> {
    fn poll_read(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut tokio::io::ReadBuf<'_>,
    ) -> Poll<std::io::Result<()>> {
        loop {
            let mut this = self.as_mut().project();

            match *this.state {
                ReadState::ReadingSize => {
                    let mut read_buf = tokio::io::ReadBuf::new(this.size_buf.unwritten_mut());
                    let before = read_buf.filled().len();
                    ready!(this.reader.as_mut().poll_read(cx, &mut read_buf))?;
                    let n = read_buf.filled().len() - before;

                    if n == 0 {
                        if this.size_buf.written().is_empty() {
                            *this.state = ReadState::Eof;
                            return Poll::Ready(Ok(()));
                        } else {
                            return Poll::Ready(Err(std::io::Error::new(
                                std::io::ErrorKind::UnexpectedEof,
                                "Unexpected EOF reading size header",
                            )));
                        }
                    }

                    this.size_buf.advance(n);

                    if this.size_buf.written().len() < 2 {
                        continue;
                    }

                    let size_header = u16::from_le_bytes(*this.size_buf.written());
                    *this.is_compressed = (size_header & 0x8000) == 0;
                    let size = (size_header & 0x7FFF) as usize;

                    if size == 0 || size > 8192 {
                        return Poll::Ready(Err(std::io::Error::new(
                            std::io::ErrorKind::InvalidData,
                            format!("Invalid metadata block size: {}", size),
                        )));
                    }

                    this.compressed.reset();
                    this.compressed.reserve(size);
                    this.size_buf.reset();
                    *this.state = ReadState::ReadingData;
                    continue;
                }

                ReadState::ReadingData => {
                    let mut read_buf = tokio::io::ReadBuf::new(this.compressed.unwritten_mut());
                    let before = read_buf.filled().len();
                    ready!(this.reader.as_mut().poll_read(cx, &mut read_buf))?;
                    let n = read_buf.filled().len() - before;

                    if n == 0 {
                        return Poll::Ready(Err(std::io::Error::new(
                            std::io::ErrorKind::UnexpectedEof,
                            "Unexpected EOF reading data",
                        )));
                    }

                    this.compressed.advance(n);

                    if !this.compressed.unwritten().is_empty() {
                        continue;
                    }

                    *this.output_pos = 0;
                    this.output.reset();
                    if *this.is_compressed {
                        *this.zstd = Some(ZstdDecoder::new());
                        *this.state = ReadState::Decompressing;
                    } else {
                        this.output
                            .copy_unwritten_from(&mut PartialBuffer::new(this.compressed.written()));
                        *this.state = ReadState::Outputting;
                    }
                    continue;
                }

                ReadState::Decompressing => {
                    if this.output.unwritten().is_empty() {
                        *this.state = ReadState::Outputting;
                        continue;
                    }

                    let mut input = PartialBuffer::new(this.compressed.written());
                    let decoder = this.zstd.as_mut().unwrap();

                    if decoder.decode(&mut input, this.output)? {
                        *this.zstd = None;
                        *this.state = ReadState::Outputting;
                    }
                    continue;
                }

                ReadState::Outputting => {
                    let available = this.output.written().len() - *this.output_pos;
                    if available == 0 {
                        if this.zstd.is_none() {
                            *this.state = ReadState::ReadingSize;
                            continue;
                        } else {
                            this.output.reset();
                            *this.output_pos = 0;
                            *this.state = ReadState::Decompressing;
                            continue;
                        }
                    }

                    let to_copy = available.min(buf.remaining());
                    buf.put_slice(
                        &this.output.written()[*this.output_pos..*this.output_pos + to_copy],
                    );
                    *this.output_pos += to_copy;
                    return Poll::Ready(Ok(()));
                }

                ReadState::Eof => {
                    return Poll::Ready(Ok(()));
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::io::{Cursor, Seek, SeekFrom};

    use proptest::prelude::*;

    use super::*;

    #[test]
    fn test_sync_roundtrip_empty() {
        use std::io::{Read, Write};
        let mut buffer = Cursor::new(Vec::new());
        let mut writer = MetadataBlocksWriter::new(&mut buffer);
        writer.flush().unwrap();

        buffer.seek(SeekFrom::Start(0)).unwrap();
        let mut reader = MetadataBlocksReader::new(buffer);
        let mut output = Vec::new();
        reader.read_to_end(&mut output).unwrap();

        assert_eq!(output, Vec::<u8>::new());
    }

    #[test]
    fn test_sync_roundtrip_small() {
        use std::io::{Read, Write};
        let input = b"Hello, World!";
        let mut buffer = Cursor::new(Vec::new());
        let mut writer = MetadataBlocksWriter::new(&mut buffer);
        writer.write_all(input).unwrap();
        writer.flush().unwrap();

        buffer.seek(SeekFrom::Start(0)).unwrap();
        let mut reader = MetadataBlocksReader::new(buffer);
        let mut output = Vec::new();
        reader.read_to_end(&mut output).unwrap();

        assert_eq!(output, input);
    }

    #[test]
    fn test_sync_roundtrip_exact_block_size() {
        use std::io::{Read, Write};
        let input = vec![0x42u8; 8192];
        let mut buffer = Cursor::new(Vec::new());
        let mut writer = MetadataBlocksWriter::new(&mut buffer);
        writer.write_all(&input).unwrap();
        writer.flush().unwrap();

        buffer.seek(SeekFrom::Start(0)).unwrap();
        let mut reader = MetadataBlocksReader::new(buffer);
        let mut output = Vec::new();
        reader.read_to_end(&mut output).unwrap();

        assert_eq!(output, input);
    }

    #[test]
    fn test_sync_roundtrip_larger_than_block() {
        use std::io::{Read, Write};
        let input = vec![0x55u8; 16384];
        let mut buffer = Cursor::new(Vec::new());
        let mut writer = MetadataBlocksWriter::new(&mut buffer);
        writer.write_all(&input).unwrap();
        writer.flush().unwrap();

        buffer.seek(SeekFrom::Start(0)).unwrap();
        let mut reader = MetadataBlocksReader::new(buffer);
        let mut output = Vec::new();
        reader.read_to_end(&mut output).unwrap();

        assert_eq!(output, input);
    }

    #[test]
    fn test_sync_roundtrip_multiple_blocks() {
        use std::io::{Read, Write};
        let input = vec![0xAAu8; 24576];
        let mut buffer = Cursor::new(Vec::new());
        let mut writer = MetadataBlocksWriter::new(&mut buffer);
        writer.write_all(&input).unwrap();
        writer.flush().unwrap();

        buffer.seek(SeekFrom::Start(0)).unwrap();
        let mut reader = MetadataBlocksReader::new(buffer);
        let mut output = Vec::new();
        reader.read_to_end(&mut output).unwrap();

        assert_eq!(output, input);
    }

    #[test]
    fn test_sync_roundtrip_incremental_writes() {
        use std::io::{Read, Write};
        let input = b"ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        let mut buffer = Cursor::new(Vec::new());
        let mut writer = MetadataBlocksWriter::new(&mut buffer);

        for chunk in input.chunks(5) {
            writer.write_all(chunk).unwrap();
        }
        writer.flush().unwrap();

        buffer.seek(SeekFrom::Start(0)).unwrap();
        let mut reader = MetadataBlocksReader::new(buffer);
        let mut output = Vec::new();
        reader.read_to_end(&mut output).unwrap();

        assert_eq!(output, input);
    }

    #[tokio::test]
    async fn test_async_roundtrip_empty() {
        use tokio::io::{AsyncReadExt, AsyncSeekExt, AsyncWriteExt};
        let mut buffer = Cursor::new(Vec::new());
        let mut writer = MetadataBlocksWriter::new(&mut buffer);
        AsyncWriteExt::flush(&mut writer).await.unwrap();

        AsyncSeekExt::seek(&mut buffer, SeekFrom::Start(0))
            .await
            .unwrap();
        let mut reader = MetadataBlocksReader::new(buffer);
        let mut output = Vec::new();
        AsyncReadExt::read_to_end(&mut reader, &mut output)
            .await
            .unwrap();

        assert_eq!(output, Vec::<u8>::new());
    }

    #[tokio::test]
    async fn test_async_roundtrip_small() {
        use tokio::io::{AsyncReadExt, AsyncSeekExt, AsyncWriteExt};
        let input = b"Hello, World!";
        let mut buffer = Cursor::new(Vec::new());
        let mut writer = MetadataBlocksWriter::new(&mut buffer);
        AsyncWriteExt::write_all(&mut writer, input).await.unwrap();
        AsyncWriteExt::flush(&mut writer).await.unwrap();

        AsyncSeekExt::seek(&mut buffer, SeekFrom::Start(0))
            .await
            .unwrap();
        let mut reader = MetadataBlocksReader::new(buffer);
        let mut output = Vec::new();
        AsyncReadExt::read_to_end(&mut reader, &mut output)
            .await
            .unwrap();

        assert_eq!(output, input);
    }

    #[tokio::test]
    async fn test_async_roundtrip_exact_block_size() {
        use tokio::io::{AsyncReadExt, AsyncSeekExt, AsyncWriteExt};
        let input = vec![0x42u8; 8192];
        let mut buffer = Cursor::new(Vec::new());
        let mut writer = MetadataBlocksWriter::new(&mut buffer);
        AsyncWriteExt::write_all(&mut writer, &input).await.unwrap();
        AsyncWriteExt::flush(&mut writer).await.unwrap();

        AsyncSeekExt::seek(&mut buffer, SeekFrom::Start(0))
            .await
            .unwrap();
        let mut reader = MetadataBlocksReader::new(buffer);
        let mut output = Vec::new();
        AsyncReadExt::read_to_end(&mut reader, &mut output)
            .await
            .unwrap();

        assert_eq!(output, input);
    }

    #[tokio::test]
    async fn test_async_roundtrip_larger_than_block() {
        use tokio::io::{AsyncReadExt, AsyncSeekExt, AsyncWriteExt};
        let input = vec![0x55u8; 16384];
        let mut buffer = Cursor::new(Vec::new());
        let mut writer = MetadataBlocksWriter::new(&mut buffer);
        AsyncWriteExt::write_all(&mut writer, &input).await.unwrap();
        AsyncWriteExt::flush(&mut writer).await.unwrap();

        AsyncSeekExt::seek(&mut buffer, SeekFrom::Start(0))
            .await
            .unwrap();
        let mut reader = MetadataBlocksReader::new(buffer);
        let mut output = Vec::new();
        AsyncReadExt::read_to_end(&mut reader, &mut output)
            .await
            .unwrap();

        assert_eq!(output, input);
    }

    #[tokio::test]
    async fn test_async_roundtrip_multiple_blocks() {
        use tokio::io::{AsyncReadExt, AsyncSeekExt, AsyncWriteExt};
        let input = vec![0xAAu8; 24576];
        let mut buffer = Cursor::new(Vec::new());
        let mut writer = MetadataBlocksWriter::new(&mut buffer);
        AsyncWriteExt::write_all(&mut writer, &input).await.unwrap();
        AsyncWriteExt::flush(&mut writer).await.unwrap();

        AsyncSeekExt::seek(&mut buffer, SeekFrom::Start(0))
            .await
            .unwrap();
        let mut reader = MetadataBlocksReader::new(buffer);
        let mut output = Vec::new();
        AsyncReadExt::read_to_end(&mut reader, &mut output)
            .await
            .unwrap();

        assert_eq!(output, input);
    }

    proptest! {
        #[test]
        fn test_sync_roundtrip_proptest(input in prop::collection::vec(any::<u8>(), 0..50000)) {
            use std::io::{Read, Write};
            let mut buffer = Cursor::new(Vec::new());
            let mut writer = MetadataBlocksWriter::new(&mut buffer);
            writer.write_all(&input).unwrap();
            writer.flush().unwrap();

            buffer.seek(SeekFrom::Start(0)).unwrap();
            let mut reader = MetadataBlocksReader::new(buffer);
            let mut output = Vec::new();
            reader.read_to_end(&mut output).unwrap();

            prop_assert_eq!(output, input);
        }

        #[test]
        fn test_sync_roundtrip_chunked_writes(
            input in prop::collection::vec(any::<u8>(), 0..50000),
            chunk_size in 1usize..1000
        ) {
            use std::io::{Read, Write};
            let mut buffer = Cursor::new(Vec::new());
            let mut writer = MetadataBlocksWriter::new(&mut buffer);

            for chunk in input.chunks(chunk_size) {
                writer.write_all(chunk).unwrap();
            }
            writer.flush().unwrap();

            buffer.seek(SeekFrom::Start(0)).unwrap();
            let mut reader = MetadataBlocksReader::new(buffer);
            let mut output = Vec::new();
            reader.read_to_end(&mut output).unwrap();

            prop_assert_eq!(output, input);
        }

        #[test]
        fn test_sync_roundtrip_chunked_reads(
            input in prop::collection::vec(any::<u8>(), 0..50000),
            chunk_size in 1usize..1000
        ) {
            use std::io::{Read, Write};
            let mut buffer = Cursor::new(Vec::new());
            let mut writer = MetadataBlocksWriter::new(&mut buffer);
            writer.write_all(&input).unwrap();
            writer.flush().unwrap();

            buffer.seek(SeekFrom::Start(0)).unwrap();
            let mut reader = MetadataBlocksReader::new(buffer);
            let mut output = Vec::new();
            let mut chunk = vec![0u8; chunk_size];

            loop {
                let n = reader.read(&mut chunk).unwrap();
                if n == 0 {
                    break;
                }
                output.extend_from_slice(&chunk[..n]);
            }

            prop_assert_eq!(output, input);
        }
    }

    #[test]
    fn test_sync_multiple_flush_cycles() {
        use std::io::{Read, Write};
        let input1 = b"First block of data";
        let input2 = b"Second block of data";
        let input3 = b"Third block of data";

        let mut buffer = Cursor::new(Vec::new());
        let mut writer = MetadataBlocksWriter::new(&mut buffer);

        writer.write_all(input1).unwrap();
        writer.flush().unwrap();

        writer.write_all(input2).unwrap();
        writer.flush().unwrap();

        writer.write_all(input3).unwrap();
        writer.flush().unwrap();

        buffer.seek(SeekFrom::Start(0)).unwrap();
        let mut reader = MetadataBlocksReader::new(buffer);
        let mut output = Vec::new();
        reader.read_to_end(&mut output).unwrap();

        let expected: Vec<u8> = input1
            .iter()
            .chain(input2.iter())
            .chain(input3.iter())
            .copied()
            .collect();
        assert_eq!(output, expected);
    }

    #[tokio::test]
    async fn test_async_multiple_flush_cycles() {
        use tokio::io::{AsyncReadExt, AsyncSeekExt, AsyncWriteExt};
        let input1 = b"First block of data";
        let input2 = b"Second block of data";
        let input3 = b"Third block of data";

        let mut buffer = Cursor::new(Vec::new());
        let mut writer = MetadataBlocksWriter::new(&mut buffer);

        AsyncWriteExt::write_all(&mut writer, input1).await.unwrap();
        AsyncWriteExt::flush(&mut writer).await.unwrap();

        AsyncWriteExt::write_all(&mut writer, input2).await.unwrap();
        AsyncWriteExt::flush(&mut writer).await.unwrap();

        AsyncWriteExt::write_all(&mut writer, input3).await.unwrap();
        AsyncWriteExt::flush(&mut writer).await.unwrap();

        AsyncSeekExt::seek(&mut buffer, SeekFrom::Start(0))
            .await
            .unwrap();
        let mut reader = MetadataBlocksReader::new(buffer);
        let mut output = Vec::new();
        AsyncReadExt::read_to_end(&mut reader, &mut output)
            .await
            .unwrap();

        let expected: Vec<u8> = input1
            .iter()
            .chain(input2.iter())
            .chain(input3.iter())
            .copied()
            .collect();
        assert_eq!(output, expected);
    }

    #[test]
    fn test_sync_size_header_format() {
        use std::io::{Read, Write};
        let input = b"test";
        let mut buffer = Cursor::new(Vec::new());
        let mut writer = MetadataBlocksWriter::new(&mut buffer);
        writer.write_all(input).unwrap();
        writer.flush().unwrap();

        buffer.seek(SeekFrom::Start(0)).unwrap();
        let mut size_header = [0u8; 2];
        buffer.read_exact(&mut size_header).unwrap();

        let size_value = u16::from_le_bytes(size_header);
        let is_compressed = (size_value & 0x8000) == 0;
        let size = size_value & 0x7FFF;

        assert!(is_compressed, "Data should be compressed");
        assert!(size > 0, "Compressed size should be greater than 0");
        assert!(size <= 8192, "Compressed size should not exceed 8192");
    }
}
