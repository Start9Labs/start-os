use tokio::fs::File;

pub mod merkle_archive;
pub mod v1;
pub mod v2;

pub use v1::{manifest, pack, reader, verify}; // TODO: remove

pub enum S9pkFile {
    V1(v1::reader::S9pkReader<File>),
    V2(v2::S9pk<File>),
}
