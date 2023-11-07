use integer_encoding::VarInt;
use tokio::io::{AsyncRead, AsyncWrite};

use crate::prelude::*;

/// Most-significant byte, == 0x80
pub const MSB: u8 = 0b1000_0000;

pub fn serialized_varint_size(n: u64) -> u64 {
    VarInt::required_space(n) as u64
}

pub async fn serialize_varint<W: AsyncWrite + Unpin + Send>(
    n: u64,
    w: &mut W,
) -> Result<(), Error> {
    use tokio::io::AsyncWriteExt;

    let mut buf = [0 as u8; 10];
    let b = n.encode_var(&mut buf);
    w.write_all(&buf[0..b]).await?;

    Ok(())
}

pub fn serialized_varstring_size(s: &str) -> u64 {
    serialized_varint_size(s.len() as u64) + s.len() as u64
}

pub async fn serialize_varstring<W: AsyncWrite + Unpin + Send>(
    s: &str,
    w: &mut W,
) -> Result<(), Error> {
    use tokio::io::AsyncWriteExt;
    serialize_varint(s.len() as u64, w).await?;
    w.write_all(s.as_bytes()).await?;
    Ok(())
}

#[derive(Default)]
struct VarIntProcessor {
    buf: [u8; 10],
    maxsize: usize,
    i: usize,
}

impl VarIntProcessor {
    fn new() -> VarIntProcessor {
        VarIntProcessor {
            maxsize: (std::mem::size_of::<u64>() * 8 + 7) / 7,
            ..VarIntProcessor::default()
        }
    }
    fn push(&mut self, b: u8) -> Result<(), Error> {
        if self.i >= self.maxsize {
            return Err(Error::new(
                eyre!("Unterminated varint"),
                ErrorKind::ParseS9pk,
            ));
        }
        self.buf[self.i] = b;
        self.i += 1;
        Ok(())
    }
    fn finished(&self) -> bool {
        self.i > 0 && (self.buf[self.i - 1] & MSB == 0)
    }
    fn decode(&self) -> Option<u64> {
        Some(u64::decode_var(&self.buf[0..self.i])?.0)
    }
}

pub async fn deserialize_varint<R: AsyncRead + Unpin>(r: &mut R) -> Result<u64, Error> {
    use tokio::io::AsyncReadExt;

    let mut buf = [0 as u8; 1];
    let mut p = VarIntProcessor::new();

    while !p.finished() {
        r.read_exact(&mut buf).await?;

        p.push(buf[0])?;
    }

    p.decode()
        .ok_or_else(|| Error::new(eyre!("Reached EOF"), ErrorKind::ParseS9pk))
}

pub async fn deserialize_varstring<R: AsyncRead + Unpin>(r: &mut R) -> Result<String, Error> {
    use tokio::io::AsyncReadExt;

    let len = deserialize_varint(r).await?;
    let mut buf = vec![0u8; len as usize];
    r.read_exact(&mut buf).await?;
    Ok(String::from_utf8(buf)?)
}
