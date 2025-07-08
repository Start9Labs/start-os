use std::collections::VecDeque;

use crate::prelude::*;
use crate::s9pk::merkle_archive::sink::Sink;
use crate::s9pk::merkle_archive::source::FileSource;
use crate::s9pk::merkle_archive::{Entry, EntryContents};

pub struct WriteQueue<'a, S> {
    next_available_position: u64,
    queue: VecDeque<&'a Entry<S>>,
}

impl<'a, S> WriteQueue<'a, S> {
    pub fn new(next_available_position: u64) -> Self {
        Self {
            next_available_position,
            queue: VecDeque::new(),
        }
    }
}
impl<'a, S: FileSource> WriteQueue<'a, S> {
    pub async fn add(&mut self, entry: &'a Entry<S>) -> Result<u64, Error> {
        let res = self.next_available_position;
        let size = match entry.as_contents() {
            EntryContents::Missing => return Ok(0),
            EntryContents::File(f) => f.size().await?,
            EntryContents::Directory(d) => d.toc_size(),
        };
        self.next_available_position += size;
        self.queue.push_back(entry);
        Ok(res)
    }
}
impl<'a, S: FileSource + Clone> WriteQueue<'a, S> {
    pub async fn serialize<W: Sink>(&mut self, w: &mut W, verify: bool) -> Result<(), Error> {
        loop {
            let Some(next) = self.queue.pop_front() else {
                break;
            };
            match next.as_contents() {
                EntryContents::Missing => (),
                EntryContents::File(f) => f.serialize_body(w, next.hash.filter(|_| verify)).await?,
                EntryContents::Directory(d) => d.serialize_toc(self, w).await?,
            }
        }
        Ok(())
    }
}
