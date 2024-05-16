use std::collections::BTreeMap;
use std::io::Cursor;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use ed25519_dalek::SigningKey;

use crate::prelude::*;
use crate::s9pk::merkle_archive::directory_contents::DirectoryContents;
use crate::s9pk::merkle_archive::file_contents::FileContents;
use crate::s9pk::merkle_archive::source::FileSource;
use crate::s9pk::merkle_archive::{Entry, EntryContents, MerkleArchive};
use crate::util::io::TrackingIO;

/// Creates a MerkleArchive (a1) with the provided files at the provided paths. NOTE: later files can overwrite previous files/directories at the same path
/// Tests:
///     - a1.update_hashes(): returns Ok(_)
///     - a1.serialize(verify: true): returns Ok(s1)
///     - MerkleArchive::deserialize(s1): returns Ok(a2)
///     - a2: contains all expected files with expected content
///     - a2.serialize(verify: true): returns Ok(s2)
///     - s1 == s2
#[instrument]
fn test(files: Vec<(PathBuf, String)>) -> Result<(), Error> {
    let mut root = DirectoryContents::<Arc<[u8]>>::new();
    let mut check_set = BTreeMap::<PathBuf, String>::new();
    for (path, content) in files {
        if let Err(e) = root.insert_path(
            &path,
            Entry::new(EntryContents::File(FileContents::new(
                content.clone().into_bytes().into(),
            ))),
        ) {
            eprintln!("failed to insert file at {path:?}: {e}");
        } else {
            let path = path.strip_prefix("/").unwrap_or(&path);
            let mut remaining = check_set.split_off(path);
            while {
                if let Some((p, s)) = remaining.pop_first() {
                    if !p.starts_with(path) {
                        remaining.insert(p, s);
                        false
                    } else {
                        true
                    }
                } else {
                    false
                }
            } {}
            check_set.append(&mut remaining);
            check_set.insert(path.to_owned(), content);
        }
    }
    let key = SigningKey::generate(&mut rand::thread_rng());
    let mut a1 = MerkleArchive::new(root, key, "test");
    tokio::runtime::Builder::new_current_thread()
        .enable_io()
        .build()
        .unwrap()
        .block_on(async move {
            a1.update_hashes(true).await?;
            let mut s1 = Vec::new();
            a1.serialize(&mut TrackingIO::new(0, &mut s1), true).await?;
            let s1: Arc<[u8]> = s1.into();
            let a2 = MerkleArchive::deserialize(
                &s1,
                "test",
                &mut Cursor::new(s1.clone()),
                Some(&a1.commitment().await?),
            )
            .await?;

            for (path, content) in check_set {
                match a2
                    .contents
                    .get_path(&path)
                    .map(|e| (e.as_contents(), e.hash()))
                {
                    Some((EntryContents::File(f), hash)) => {
                        ensure_code!(
                            &f.to_vec(hash).await? == content.as_bytes(),
                            ErrorKind::ParseS9pk,
                            "File at {path:?} does not match input"
                        )
                    }
                    _ => {
                        return Err(Error::new(
                            eyre!("expected file at {path:?}"),
                            ErrorKind::ParseS9pk,
                        ))
                    }
                }
            }

            let mut s2 = Vec::new();
            a2.serialize(&mut TrackingIO::new(0, &mut s2), true).await?;
            let s2: Arc<[u8]> = s2.into();
            ensure_code!(s1 == s2, ErrorKind::Pack, "s1 does not match s2");

            Ok(())
        })
}

proptest::proptest! {
    #[test]
    fn property_test(files: Vec<(PathBuf, String)>) {
        let files: Vec<(PathBuf, String)> = files.into_iter().filter(|(p, _)| p.file_name().is_some() && p.iter().all(|s| s.to_str().is_some())).collect();
        if let Err(e) = test(files.clone()) {
            panic!("{e}\nInput: {files:#?}\n{e:?}");
        }
    }
}

#[test]
fn test_example_1() {
    if let Err(e) = test(vec![(Path::new("foo").into(), "bar".into())]) {
        panic!("{e}\n{e:?}");
    }
}

#[test]
fn test_example_2() {
    if let Err(e) = test(vec![
        (Path::new("a/a.txt").into(), "a.txt".into()),
        (Path::new("a/b/a.txt").into(), "a.txt".into()),
        (Path::new("a/b/b/a.txt").into(), "a.txt".into()),
        (Path::new("a/b/c.txt").into(), "c.txt".into()),
        (Path::new("a/c.txt").into(), "c.txt".into()),
    ]) {
        panic!("{e}\n{e:?}");
    }
}

#[test]
fn test_example_3() {
    if let Err(e) = test(vec![
        (Path::new("b/a").into(), "𑦪".into()),
        (Path::new("a/c/a").into(), "·".into()),
    ]) {
        panic!("{e}\n{e:?}");
    }
}
