
use std::ffi::OsStr;
use std::path::Path;

use crate::prelude::*;
use crate::s9pk::merkle_archive::directory_contents::DirectoryContents;
use crate::s9pk::merkle_archive::source::FileSource;
use crate::s9pk::merkle_archive::Entry;

pub struct Expected<'a, T> {
    keep: DirectoryContents<()>,
    dir: &'a DirectoryContents<T>,
}
impl<'a, T> Expected<'a, T> {
    pub fn new(dir: &'a DirectoryContents<T>,) -> Self {
        Self {
            keep: DirectoryContents::new(),
            dir
        }
    }
}
impl<'a, T: Clone> Expected<'a, T> {
    pub fn check_file(&mut self, path: impl AsRef<Path>) -> Result<(), Error> {
        if self
            .dir
            .get_path(path.as_ref())
            .and_then(|e| e.as_file())
            .is_some()
        {
            self.keep.insert_path(path, Entry::file(()));
            Ok(())
        } else {
            Err(Error::new(
                eyre!("file {} missing from archive", path.as_ref().display()),
                ErrorKind::ParseS9pk,
            ))
        }
    }
    pub fn check_stem(
        &mut self,
        path: impl AsRef<Path>,
        mut valid_extension: impl FnMut(Option<&OsStr>) -> bool,
    ) -> Result<(), Error> {
        let (dir, stem) = if let Some(parent) = path.as_ref().parent() {
            (
                self.dir
                    .get_path(parent)
                    .and_then(|e| e.as_directory())
                    .ok_or_else(|| {
                        Error::new(
                            eyre!("directory {} missing from archive", parent.display()),
                            ErrorKind::ParseS9pk,
                        )
                    })?,
                path.as_ref().strip_prefix(parent).unwrap(),
            )
        } else {
            (self.dir, path.as_ref())
        };
        let name = dir
            .with_stem(&stem.as_os_str().to_string_lossy())
            .filter(|(_, e)| e.as_file().is_some())
            .try_fold(
                Err(Error::new(
                    eyre!(
                        "file {} with valid extension missing from archive",
                        path.as_ref().display()
                    ),
                    ErrorKind::ParseS9pk,
                )),
                |acc, (name, _)| 
                    if valid_extension(Path::new(&*name).extension()) {
                        match acc {
                            Ok(_) => Err(Error::new(
                                eyre!(
                                    "more than one file matching {} with valid extension in archive",
                                    path.as_ref().display()
                                ),
                                ErrorKind::ParseS9pk,
                            )),
                            Err(_) => Ok(Ok(name))
                        }
                    } else {
                        Ok(acc)
                    }
            )??;
        self.keep
            .insert_path(path.as_ref().with_file_name(name), Entry::file(()));
        Ok(())
    }
    pub fn into_filter(self) -> Filter {
        Filter(self.keep)
    }
}

pub struct Filter(DirectoryContents<()>);
impl Filter {
    pub fn keep_checked<T: FileSource + Clone>(&self, dir: &mut DirectoryContents<T>) -> Result<(), Error> {
        dir.filter(|path| self.0.get_path(path).is_some())
    }
}

