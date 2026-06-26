use std::future::Future;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

use imbl_value::{json, Value};
use json_ptr::JsonPointer;
use patch_db::{PatchDb, Revision};
use proptest::prelude::*;
use tokio::fs;
use tokio::runtime::Builder;

use crate::{self as patch_db};

/// Atomic counter to generate unique file paths across concurrent tests.
static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn unique_db_path(prefix: &str) -> String {
    let id = TEST_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("test-{}-{}.db", prefix, id)
}

async fn init_db(db_name: String) -> PatchDb {
    cleanup_db(&db_name).await;
    let db = PatchDb::open(db_name).await.unwrap();
    db.put(
        &JsonPointer::<&'static str>::default(),
        &json!({
            "a": "test1",
            "b": {
                "a": "test2",
                "b": 1,
                "c": null,
            },
        }),
    )
    .await
    .unwrap();
    db
}

async fn cleanup_db(db_name: &str) {
    fs::remove_file(db_name).await.ok();
    fs::remove_file(format!("{}.bak", db_name)).await.ok();
    fs::remove_file(format!("{}.bak.tmp", db_name)).await.ok();
    fs::remove_file(format!("{}.failed", db_name)).await.ok();
}

async fn put_string_into_root(db: &PatchDb, s: String) -> Arc<Revision> {
    db.put(&JsonPointer::<&'static str>::default(), &s)
        .await
        .unwrap()
        .unwrap()
}

#[tokio::test]
async fn basic() {
    let path = unique_db_path("basic");
    let db = init_db(path.clone()).await;
    let ptr: JsonPointer = "/b/b".parse().unwrap();
    let mut get_res: Value = db.get(&ptr).await.unwrap();
    assert_eq!(get_res.as_u64(), Some(1));
    db.put(&ptr, "hello").await.unwrap();
    get_res = db.get(&ptr).await.unwrap();
    assert_eq!(get_res.as_str(), Some("hello"));
    db.close().await;
    cleanup_db(&path).await;
}

fn run_future<S: Into<String>, Fut: Future<Output = ()>>(name: S, fut: Fut) {
    Builder::new_multi_thread()
        .thread_name(name)
        .build()
        .unwrap()
        .block_on(fut)
}

proptest! {
    #[test]
    fn doesnt_crash(s in "\\PC*") {
        run_future("test-doesnt-crash", async {
            let path = unique_db_path("proptest");
            let db = init_db(path.clone()).await;
            put_string_into_root(&db, s).await;
            db.close().await;
            cleanup_db(&path).await;
        });
    }
}
