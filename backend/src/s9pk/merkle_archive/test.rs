use std::collections::BTreeMap;
use std::io::Cursor;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use ed25519_dalek::SigningKey;

use crate::prelude::*;
use crate::s9pk::merkle_archive::directory_contents::DirectoryContents;
use crate::s9pk::merkle_archive::file_contents::FileContents;
use crate::s9pk::merkle_archive::sink::TrackingWriter;
use crate::s9pk::merkle_archive::{Entry, EntryContents, MerkleArchive};

#[instrument]
fn test(files: Vec<(PathBuf, String)>) -> Result<(), Error> {
    let mut root = DirectoryContents::<Arc<[u8]>>::new();
    let mut check_set = BTreeMap::new();
    for (path, content) in files {
        if let Err(e) = root.insert_path(
            &path,
            Entry::new(EntryContents::File(FileContents::new(
                content.clone().into_bytes().into(),
            ))),
        ) {
            eprintln!("failed to insert file at {path:?}: {e}");
        } else {
            check_set.insert(path.clone(), content);
        }
    }
    let key = SigningKey::generate(&mut rand::thread_rng());
    let mut archive = MerkleArchive::new(root, key);
    tokio::runtime::Builder::new_current_thread()
        .enable_io()
        .build()
        .unwrap()
        .block_on(async move {
            archive.update_hashes(true).await?;
            let mut s1 = Vec::new();
            archive
                .serialize(&mut TrackingWriter::new(0, &mut s1), true)
                .await?;
            let s1: Arc<[u8]> = s1.into();
            let archive = MerkleArchive::deserialize(&s1, &mut Cursor::new(s1.clone())).await?;

            for (path, content) in check_set {
                match archive.contents.get_path(&path).map(|e| e.as_contents()) {
                    Some(EntryContents::File(f)) => {
                        ensure_code!(
                            &f.to_vec().await? == content.as_bytes(),
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
            archive
                .serialize(&mut TrackingWriter::new(0, &mut s2), true)
                .await?;
            let s2: Arc<[u8]> = s2.into();
            ensure_code!(s1 == s2, ErrorKind::Pack, "s1 does not match s2");

            Ok(())
        })
}

proptest::proptest! {
    #[test]
    fn property_test(files: Vec<(PathBuf, String)>) {
        // TODO: make unique
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
        (Path::new("b/ⶨ").into(), "𑦪".into()),
        (Path::new("a/c/0").into(), "·".into()),
    ]) {
        panic!("{e}\n{e:?}");
    }
}
