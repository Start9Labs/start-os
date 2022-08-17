use std::cmp::min;
use std::convert::TryFrom;
use std::fmt::Display;
use std::io::Error as StdIOError;
use std::pin::Pin;
use std::task::{Context, Poll};

use color_eyre::eyre::eyre;
use futures::future::BoxFuture;
use futures::FutureExt;
use http::header::{ACCEPT_RANGES, CONTENT_LENGTH, RANGE};
use pin_project::pin_project;
use reqwest::{Client, Url};
use tokio::io::{AsyncRead, AsyncSeek};

use crate::{Error, ResultExt};

#[pin_project]
pub struct HttpReader {
    http_url: Url,
    cursor_pos: usize,
    http_client: Client,
    total_bytes: usize,
    range_unit: Option<RangeUnit>,
    read_in_progress: Option<BoxFuture<'static, Result<Vec<u8>, Error>>>,
}

// If we want to add support for units other than Accept-Ranges: bytes, we can use this enum
#[derive(Clone, Copy)]
enum RangeUnit {
    Bytes,
}

impl Display for RangeUnit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RangeUnit::Bytes => write!(f, "bytes"),
        }
    }
}

impl HttpReader {
    pub async fn new(http_url: Url) -> Result<Self, Error> {
        let http_client = Client::builder()
            .build()
            .with_kind(crate::ErrorKind::TLSInit)?;

        // Make a head request so that we can get the file size and check for http range support.
        let head_request = http_client
            .head(http_url.clone())
            .send()
            .await
            .with_kind(crate::ErrorKind::InvalidRequest)?;

        // https://developer.mozilla.org/en-US/docs/Web/HTTP/Range_requests

        let accept_ranges = head_request.headers().get(ACCEPT_RANGES);

        let range_unit = match accept_ranges {
            Some(range_type) => {
                // as per rfc, header will contain data but not always UTF8 characters.
                // Reject?
                let value = range_type.to_str().unwrap();

                match value {
                    "bytes" => Some(RangeUnit::Bytes),
                    _ => {
                        return Err(Error::new(
                            eyre!(
                                "{} HTTP range downloading not supported with this unit {value}",
                                http_url
                            ),
                            crate::ErrorKind::HttpRange,
                        ));
                    }
                }
            }
            // None can mean just get entire contents, but we currently error out
            None => {
                return Err(Error::new(
                    eyre!(
                        "{} HTTP range downloading not supported with this url",
                        http_url
                    ),
                    crate::ErrorKind::HttpRange,
                ))
            }
        };

        let total_bytes_option = head_request.headers().get(CONTENT_LENGTH);

        let total_bytes = match total_bytes_option {
            Some(bytes) => bytes.to_str().unwrap().parse::<usize>().unwrap(),
            None => {
                return Err(Error::new(
                    eyre!("No content length headers for {}", http_url),
                    crate::ErrorKind::ContentLength,
                ))
            }
        };

        Ok(HttpReader {
            http_url,
            cursor_pos: 0,
            http_client,
            total_bytes,
            range_unit,
            read_in_progress: None,
        })
    }

    async fn get_range(
        range_unit: Option<RangeUnit>,
        http_client: Client,
        http_url: Url,
        start: usize,
        len: usize,
        total_bytes: usize,
    ) -> Result<Vec<u8>, Error> {
        let mut data = Vec::new();
        let end = min((start + len) - 1, total_bytes);

        match range_unit {
            Some(unit) => {
                let data_range = format!("{}={}-{} ", unit, start, end);
                println!("get range alive? {}", data_range);

                let data_resp = http_client
                    .get(http_url)
                    .header(RANGE, data_range)
                    .send()
                    .await
                    .with_kind(crate::ErrorKind::InvalidRequest)?;

                let status_code = data_resp.status();
                //let data_res = data_resp.bytes().await;
                if status_code.is_success() {
                    data = data_resp
                        .bytes()
                        .await
                        .with_kind(crate::ErrorKind::BytesError)?
                        .to_vec();
                }
            }

            None => unreachable!(),
        }

        Ok(data)
    }
}

impl AsyncRead for HttpReader {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut tokio::io::ReadBuf<'_>,
    ) -> Poll<std::io::Result<()>> {
        let this = self.project();

        let mut fut = if let Some(fut) = this.read_in_progress.take() {
            fut
        } else {
            HttpReader::get_range(
                *this.range_unit,
                this.http_client.clone(),
                this.http_url.clone(),
                *this.cursor_pos,
                buf.remaining(),
                *this.total_bytes,
            )
            .boxed()
        };

        let res_poll = fut.as_mut().poll(cx);
        println!("polling");
        println!("buf remaining: {}", buf.remaining());
        match res_poll {
            Poll::Ready(result) => match result {
                Ok(data_chunk) => {
                    println!("data chunk: len: {}", data_chunk.len());
                    println!("buf filled len: {}", buf.filled().len());
                    if data_chunk.len() <= buf.remaining() {
                        buf.put_slice(&data_chunk);
                        *this.cursor_pos += data_chunk.len();

                        Poll::Ready(Ok(()))
                    } else {
                        println!("data chunk: len: {}", data_chunk.len());
                        println!("buf filled len: {}", buf.filled().len());
                        buf.put_slice(&data_chunk);

                        Poll::Ready(Ok(()))
                    }
                }
                Err(err) => Poll::Ready(Err(StdIOError::new(
                    std::io::ErrorKind::Interrupted,
                    Box::<dyn std::error::Error + Send + Sync>::from(err.source),
                ))),
            },
            Poll::Pending => {
                println!("Pending...");
                *this.read_in_progress = Some(fut);

                Poll::Pending
            }
        }
    }
}

impl AsyncSeek for HttpReader {
    fn start_seek(self: Pin<&mut Self>, position: std::io::SeekFrom) -> std::io::Result<()> {
        let this = self.project();

        match position {
            std::io::SeekFrom::Start(pos) => {
                let pos_res = usize::try_from(pos);

                match pos_res {
                    Ok(pos) => {
                        if pos > *this.total_bytes {
                            StdIOError::new(
                                std::io::ErrorKind::InvalidInput,
                                format!(
                                    "position: {} cannot be greater than {} bytes",
                                    pos, *this.total_bytes
                                ),
                            );
                        }
                        *this.cursor_pos = pos;
                    }
                    Err(err) => return Err(StdIOError::new(std::io::ErrorKind::InvalidInput, err)),
                }
                Ok(())
            }
            std::io::SeekFrom::Current(pos) => {
                let original_pos = pos;
                let pos_res = usize::try_from(pos);

                match pos_res {
                    Ok(pos) => {
                        if pos > *this.total_bytes {
                            StdIOError::new(
                                std::io::ErrorKind::InvalidInput,
                                format!(
                                    "position: {} cannot be greater than {} bytes",
                                    pos, *this.total_bytes
                                ),
                            );
                        }
                        let total_bytes_res = i64::try_from(*this.total_bytes);

                        match total_bytes_res {
                            Ok(total_bytes) => {
                                let new_pos_res = usize::try_from(total_bytes - original_pos);

                                match new_pos_res {
                                    Ok(pos) => {
                                        *this.cursor_pos = pos;
                                    }
                                    Err(err) => {
                                        return Err(StdIOError::new(
                                            std::io::ErrorKind::InvalidInput,
                                            err,
                                        ))
                                    }
                                }
                            }
                            Err(err) => {
                                return Err(StdIOError::new(
                                    std::io::ErrorKind::InvalidInput,
                                    err))
                            }
                        }
                    }
                    Err(err) => { 
                        return Err(StdIOError::new(
                            std::io::ErrorKind::InvalidInput,
                            err)) 
                        },
                }

                Ok(())
            }

            std::io::SeekFrom::End(pos) => {
                let pos_res = usize::try_from(pos);

                match pos_res {
                    Ok(pos) => {
                        if pos > *this.total_bytes {
                            StdIOError::new(
                                std::io::ErrorKind::InvalidInput,
                                format!(
                                    "position: {} cannot be greater than {} bytes",
                                    pos, *this.total_bytes
                                ),
                            );
                        }
                        *this.cursor_pos += pos;
                    }
                    Err(err) => return Err(StdIOError::new(std::io::ErrorKind::InvalidInput, err)),
                }
                Ok(())
            }
        }
    }

    fn poll_complete(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<std::io::Result<u64>> {
        Poll::Ready(Ok(self.cursor_pos as u64))
    }
}

#[tokio::test]
async fn test() {
    use tokio::io::AsyncReadExt;
    let http_url = Url::parse("https://start9.com/latest/_static/css/main.css").unwrap();

    println!("{}", http_url);
    let mut test_reader = HttpReader::new(http_url).await.unwrap();

    let mut buf = vec![0; test_reader.total_bytes];
    let bytes_read = test_reader.read(&mut buf).await.unwrap();

    println!("bytes read: {}", bytes_read);

    //println!("{}", String::from_utf8(buf).unwrap());
}
