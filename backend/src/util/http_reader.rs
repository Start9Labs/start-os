use std::fmt::Display;
use std::pin::Pin;
use std::task::{Context, Poll};

use color_eyre::eyre::eyre;
use http::header::{ACCEPT_RANGES, CONTENT_LENGTH, RANGE};
use num::iter::Range;
use pin_project::pin_project;
use reqwest::{Client, Url};
use tokio::io::AsyncRead;

use crate::setup::status;
use crate::{Error, ResultExt};

pub const DEFAULT_BUF_SIZE: usize = 1000 * 1024;

#[pin_project]
pub struct HttpReader {
    #[pin]
    internal_buf: Vec<u8>,
    http_url: Url,
    cursor_pos: usize,
    http_client: Client,
    total_bytes: usize,
    range_unit: Option<RangeUnit>,
}

// If we want to add support for units other than Accept-Ranges: bytes, we can use this enum
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
    pub async fn new(http_url: Url, buf_size: Option<usize>) -> Result<Self, Error> {
        let internal_buf = Vec::with_capacity(buf_size.unwrap_or(DEFAULT_BUF_SIZE));

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
        // TODO: We should decide if the web request should be rejected if http_ranges is not supported. Or we could download the entire contents otherwise.
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
                                "{} HTTP range downloading not supported with this unit",
                                http_url
                            ),
                            crate::ErrorKind::HttpRange,
                        ));
                    }
                }
            }
            // None can mean just get entire contents.
            None => None,
        };

        let total_bytes = head_request
            .headers()
            .get(CONTENT_LENGTH)
            .unwrap()
            .to_str()
            .unwrap()
            .parse::<usize>()
            .unwrap();

        Ok(HttpReader {
            http_url,
            internal_buf,
            cursor_pos: 0,
            http_client,
            total_bytes,
            range_unit,
        })
    }

    pub async fn get_range(&self, start: usize, end: usize) -> Result<Vec<u8>, Error> {
        let data = Vec::new();
        match &self.range_unit {
            Some(unit) => {
                let data_range = format!("{}={}-{} ", unit, start, end);

                let data_resp = self
                    .http_client
                    .get(self.http_url.to_owned())
                    .header(RANGE, data_range)
                    .send()
                    .await
                    .with_kind(crate::ErrorKind::InvalidRequest)?;

                let status_code = data_resp.status();
                if status_code.is_success() {
                    data = data_resp.bytes().await?.to_vec();
                }
            }

            None => todo!(),
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
        //let this = self.project();

        //let prev = buf.filled().len() as u64;

        //buf.put_slice(buf);

        AsyncRead::poll_read(self, cx, buf)
    }
}

#[tokio::test]
async fn test() {}
