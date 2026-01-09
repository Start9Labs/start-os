use crate::utils::HandlerExtSerde;
use crate::{CliContext, Error, ServerContext};
use chrono::{DateTime, Utc};
use clap::Parser;
use rpc_toolkit::{Context, HandlerExt, ParentHandler, from_fn};
use serde::{Deserialize, Serialize};
use std::fs::{self, File};
use std::io::Read as _;
use std::os::unix::fs::MetadataExt;
use std::path::PathBuf;

// === File namespace (file.get, file.set) ===

pub fn file<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "get",
            from_fn(get)
                .with_display_serializable()
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "set",
            from_fn(set)
                .with_display_serializable()
                .with_call_remote::<CliContext>(),
        )
}

#[derive(Debug, Serialize, Deserialize)]
pub struct FileContents {
    pub contents: String,
    pub modified: DateTime<Utc>,
}

#[derive(Parser, Serialize, Deserialize)]
pub struct GetFileArgs {
    pub path: PathBuf,
}

#[derive(Parser, Serialize, Deserialize)]
pub struct SetFileArgs {
    pub path: PathBuf,
    pub contents: String,
    pub modified: Option<DateTime<Utc>>,
}

fn get_modified_time(path: &PathBuf) -> Result<DateTime<Utc>, Error> {
    fs::metadata(path)?
        .modified()
        .map(DateTime::<Utc>::from)
        .map_err(Into::into)
}

pub fn get(_ctx: ServerContext, GetFileArgs { path }: GetFileArgs) -> Result<FileContents, Error> {
    let mut file = File::open(&path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let modified = get_modified_time(&path)?;

    Ok(FileContents { contents, modified })
}

pub fn set(
    _ctx: ServerContext,
    SetFileArgs {
        path,
        contents,
        modified,
    }: SetFileArgs,
) -> Result<(), Error> {
    // Check if file was modified since client last read it
    if let Some(expected) = modified {
        if path.exists() {
            let current = get_modified_time(&path)?;
            if current != expected {
                return Err(Error::other(format!(
                    "file was modified: expected {}, found {}",
                    expected, current
                )));
            }
        }
    }

    // Create parent directories if they don't exist
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }

    fs::write(&path, &contents)?;

    Ok(())
}

// === Dir namespace (dir.get) ===

pub fn dir<C: Context>() -> ParentHandler<C> {
    ParentHandler::new().subcommand(
        "get",
        from_fn(dir_get)
            .with_display_serializable()
            .with_call_remote::<CliContext>(),
    )
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum FileType {
    RegularFile,
    Directory,
    Symlink,
    BlockDevice,
    CharDevice,
    Fifo,
    Socket,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct DirEntry {
    pub name: String,
    pub size: u64,
    pub blocks: u64,
    pub io_block: u64,
    pub file_type: FileType,
    pub device: u64,
    pub inode: u64,
    pub links: u64,
    pub mode: u32,
    pub uid: u32,
    pub gid: u32,
    pub access: DateTime<Utc>,
    pub modify: DateTime<Utc>,
    pub change: DateTime<Utc>,
}

#[derive(Parser, Serialize, Deserialize)]
pub struct DirGetArgs {
    pub path: PathBuf,
}

fn file_type_from_mode(mode: u32) -> FileType {
    const S_IFMT: u32 = 0o170000;
    const S_IFREG: u32 = 0o100000;
    const S_IFDIR: u32 = 0o040000;
    const S_IFLNK: u32 = 0o120000;
    const S_IFBLK: u32 = 0o060000;
    const S_IFCHR: u32 = 0o020000;
    const S_IFIFO: u32 = 0o010000;
    const S_IFSOCK: u32 = 0o140000;

    match mode & S_IFMT {
        S_IFREG => FileType::RegularFile,
        S_IFDIR => FileType::Directory,
        S_IFLNK => FileType::Symlink,
        S_IFBLK => FileType::BlockDevice,
        S_IFCHR => FileType::CharDevice,
        S_IFIFO => FileType::Fifo,
        S_IFSOCK => FileType::Socket,
        _ => FileType::RegularFile,
    }
}

fn timestamp_to_datetime(secs: i64) -> DateTime<Utc> {
    DateTime::from_timestamp(secs, 0).unwrap_or_default()
}

pub fn dir_get(_ctx: ServerContext, DirGetArgs { path }: DirGetArgs) -> Result<Vec<DirEntry>, Error> {
    let entries = fs::read_dir(&path)?
        .filter_map(|entry| {
            let entry = entry.ok()?;
            let name = entry.file_name().to_string_lossy().into_owned();
            let metadata = entry.metadata().ok()?;
            let mode = metadata.mode();

            Some(DirEntry {
                name,
                size: metadata.size(),
                blocks: metadata.blocks(),
                io_block: metadata.blksize(),
                file_type: file_type_from_mode(mode),
                device: metadata.dev(),
                inode: metadata.ino(),
                links: metadata.nlink(),
                mode: mode & 0o7777, // permission bits only
                uid: metadata.uid(),
                gid: metadata.gid(),
                access: timestamp_to_datetime(metadata.atime()),
                modify: timestamp_to_datetime(metadata.mtime()),
                change: timestamp_to_datetime(metadata.ctime()),
            })
        })
        .collect();

    Ok(entries)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread;
    use std::time::Duration;
    use tempfile::{tempdir, NamedTempFile};

    // === file::get tests ===

    #[test]
    fn test_get_reads_file_contents() {
        let temp_file = NamedTempFile::new().unwrap();
        fs::write(temp_file.path(), "hello world").unwrap();

        let result = get(
            ServerContext,
            GetFileArgs {
                path: temp_file.path().to_path_buf(),
            },
        );

        assert!(result.is_ok());
        let file_contents = result.unwrap();
        assert_eq!(file_contents.contents, "hello world");
    }

    #[test]
    fn test_get_returns_error_for_missing_file() {
        let result = get(
            ServerContext,
            GetFileArgs {
                path: PathBuf::from("/nonexistent/file.txt"),
            },
        );

        assert!(result.is_err());
    }

    // === file::set tests ===

    #[test]
    fn test_set_creates_new_file() {
        let temp_dir = tempdir().unwrap();
        let file_path = temp_dir.path().join("new_file.txt");

        let result = set(
            ServerContext,
            SetFileArgs {
                path: file_path.clone(),
                contents: "new content".to_string(),
                modified: None,
            },
        );

        assert!(result.is_ok());
        assert_eq!(fs::read_to_string(&file_path).unwrap(), "new content");
    }

    #[test]
    fn test_set_overwrites_existing_file() {
        let temp_file = NamedTempFile::new().unwrap();
        let path = temp_file.path().to_path_buf();
        fs::write(&path, "original").unwrap();

        let result = set(
            ServerContext,
            SetFileArgs {
                path: path.clone(),
                contents: "updated".to_string(),
                modified: None,
            },
        );

        assert!(result.is_ok());
        assert_eq!(fs::read_to_string(&path).unwrap(), "updated");
    }

    #[test]
    fn test_set_succeeds_with_matching_modified_time() {
        let temp_file = NamedTempFile::new().unwrap();
        let path = temp_file.path().to_path_buf();
        fs::write(&path, "original").unwrap();

        // Get the current modified time
        let modified = get_modified_time(&path).unwrap();

        let result = set(
            ServerContext,
            SetFileArgs {
                path: path.clone(),
                contents: "updated".to_string(),
                modified: Some(modified),
            },
        );

        assert!(result.is_ok());
        assert_eq!(fs::read_to_string(&path).unwrap(), "updated");
    }

    #[test]
    fn test_set_fails_with_mismatched_modified_time() {
        let temp_file = NamedTempFile::new().unwrap();
        let path = temp_file.path().to_path_buf();
        fs::write(&path, "original").unwrap();

        // Get the current modified time
        let old_modified = get_modified_time(&path).unwrap();

        // Wait and modify the file to change its timestamp
        thread::sleep(Duration::from_millis(10));
        fs::write(&path, "changed by someone else").unwrap();

        // Try to set with the old modified time
        let result = set(
            ServerContext,
            SetFileArgs {
                path: path.clone(),
                contents: "my update".to_string(),
                modified: Some(old_modified),
            },
        );

        assert!(result.is_err());
        // File should still have the "changed by someone else" content
        assert_eq!(fs::read_to_string(&path).unwrap(), "changed by someone else");
    }

    #[test]
    fn test_set_creates_parent_directories() {
        let temp_dir = tempdir().unwrap();
        let nested_path = temp_dir.path().join("a/b/c/file.txt");

        let result = set(
            ServerContext,
            SetFileArgs {
                path: nested_path.clone(),
                contents: "nested content".to_string(),
                modified: None,
            },
        );

        assert!(result.is_ok());
        assert!(nested_path.exists());
        assert_eq!(fs::read_to_string(&nested_path).unwrap(), "nested content");
    }

    // === dir::get tests ===

    #[test]
    fn test_dir_get_lists_entries() {
        let temp_dir = tempdir().unwrap();

        // Create a file and a subdirectory
        fs::write(temp_dir.path().join("file.txt"), "content").unwrap();
        fs::create_dir(temp_dir.path().join("subdir")).unwrap();

        let result = dir_get(
            ServerContext,
            DirGetArgs {
                path: temp_dir.path().to_path_buf(),
            },
        );

        assert!(result.is_ok());
        let entries = result.unwrap();
        assert_eq!(entries.len(), 2);

        let file_entry = entries.iter().find(|e| e.name == "file.txt").unwrap();
        assert_eq!(file_entry.file_type, FileType::RegularFile);
        assert_eq!(file_entry.size, 7); // "content" is 7 bytes

        let dir_entry = entries.iter().find(|e| e.name == "subdir").unwrap();
        assert_eq!(dir_entry.file_type, FileType::Directory);
    }

    #[test]
    fn test_dir_get_empty_directory() {
        let temp_dir = tempdir().unwrap();

        let result = dir_get(
            ServerContext,
            DirGetArgs {
                path: temp_dir.path().to_path_buf(),
            },
        );

        assert!(result.is_ok());
        assert!(result.unwrap().is_empty());
    }

    #[test]
    fn test_dir_get_returns_error_for_missing_directory() {
        let result = dir_get(
            ServerContext,
            DirGetArgs {
                path: PathBuf::from("/nonexistent/directory"),
            },
        );

        assert!(result.is_err());
    }
}
