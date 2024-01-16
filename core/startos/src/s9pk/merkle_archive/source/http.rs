use bytes::Bytes;
use futures::stream::BoxStream;
use futures::{StreamExt, TryStreamExt};
use reqwest::header::{ACCEPT_RANGES, CONTENT_LENGTH, RANGE};
use reqwest::{Client, Url};
use tokio::io::AsyncRead;
use tokio_util::io::StreamReader;

use crate::prelude::*;
use crate::s9pk::merkle_archive::source::ArchiveSource;

#[derive(Clone)]
pub struct HttpSource {
    url: Url,
    client: Client,
    size: Option<u64>,
    range_support: Result<
        (),
        (), // Arc<Mutex<Option<RangelessReader>>>
    >,
}
impl HttpSource {
    pub async fn new(client: Client, url: Url) -> Result<Self, Error> {
        let head = client
            .head(url.clone())
            .send()
            .await
            .with_kind(ErrorKind::Network)?
            .error_for_status()
            .with_kind(ErrorKind::Network)?;
        let range_support = head
            .headers()
            .get(ACCEPT_RANGES)
            .and_then(|s| s.to_str().ok())
            == Some("bytes");
        let size = head
            .headers()
            .get(CONTENT_LENGTH)
            .and_then(|s| s.to_str().ok())
            .and_then(|s| s.parse().ok());
        Ok(Self {
            url,
            client,
            size,
            range_support: if range_support {
                Ok(())
            } else {
                Err(()) // Err(Arc::new(Mutex::new(None)))
            },
        })
    }
}
#[async_trait::async_trait]
impl ArchiveSource for HttpSource {
    type Reader = HttpReader;
    async fn fetch(&self, position: u64, size: u64) -> Result<Self::Reader, Error> {
        match self.range_support {
            Ok(_) => Ok(HttpReader::Range(StreamReader::new(if size > 0 {
                self.client
                    .get(self.url.clone())
                    .header(RANGE, format!("bytes={}-{}", position, position + size - 1))
                    .send()
                    .await
                    .with_kind(ErrorKind::Network)?
                    .error_for_status()
                    .with_kind(ErrorKind::Network)?
                    .bytes_stream()
                    .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))
                    .boxed()
            } else {
                futures::stream::empty().boxed()
            }))),
            _ => todo!(),
        }
    }
}

#[pin_project::pin_project(project = HttpReaderProj)]
pub enum HttpReader {
    Range(#[pin] StreamReader<BoxStream<'static, Result<Bytes, std::io::Error>>, Bytes>),
    // Rangeless(#[pin] RangelessReader),
}
impl AsyncRead for HttpReader {
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut tokio::io::ReadBuf<'_>,
    ) -> std::task::Poll<std::io::Result<()>> {
        match self.project() {
            HttpReaderProj::Range(r) => r.poll_read(cx, buf),
            // HttpReaderProj::Rangeless(r) => r.poll_read(cx, buf),
        }
    }
}

// type RangelessReader = StreamReader<BoxStream<'static, Bytes>, Bytes>;
