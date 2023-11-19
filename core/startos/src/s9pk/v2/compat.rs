use tokio::io::{AsyncRead, AsyncSeek};

use crate::prelude::*;
use crate::s9pk::v1::reader::S9pkReader;
use crate::s9pk::v2::S9pk;

struct CompatSource;

impl S9pk<CompatSource> {
    pub async fn from_v1<R: AsyncRead + AsyncSeek + Unpin + Send + Sync>(
        reader: S9pkReader<R>,
    ) -> Result<Self, Error> {
        todo!()
    }
}
