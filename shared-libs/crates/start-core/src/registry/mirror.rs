use std::collections::BTreeMap;
use std::sync::{Arc, Weak};

use patch_db::json_ptr::JsonPointer;
use patch_db::{ModelExt, PatchDb};

use crate::prelude::*;
use crate::registry::RegistryDatabase;
use crate::registry::package::index::Reader;
use crate::rpc_continuations::Guid;
use crate::util::future::NonDetachingJoinHandle;

/// Mirrors keyed by the signer they show private packages to. `None` keys the public mirror.
pub type IndexMirrors = tokio::sync::Mutex<BTreeMap<Option<Guid>, Weak<IndexMirror>>>;

/// An in-memory copy of `/index` without the packages hidden from one signer.
pub struct IndexMirror {
    pub db: PatchDb,
    _sync: NonDetachingJoinHandle<()>,
}

impl IndexMirror {
    async fn spawn(
        main: &TypedPatchDb<RegistryDatabase>,
        signer: Option<Guid>,
    ) -> Result<Arc<Self>, Error> {
        let (dump, mut sub) = main.dump_and_sub(JsonPointer::default()).await;
        let mut root = dump.value;
        let mut visible = visible_root(&root, signer.as_ref())?;
        let db = PatchDb::in_memory();
        db.put(&JsonPointer::<&str>::default(), &visible).await?;
        let mirror = db.clone();
        let sync = tokio::spawn(async move {
            while let Some(rev) = sub.recv().await {
                let res = async {
                    patch_db::json_patch::patch(&mut root, &rev.patch)
                        .map_err(|e| Error::new(eyre!("{e}"), ErrorKind::Database))?;
                    let next = visible_root(&root, signer.as_ref())?;
                    let diff = patch_db::diff(&visible, &next);
                    visible = next;
                    if !diff.is_empty() {
                        mirror.apply(diff).await?;
                    }
                    Ok::<_, Error>(())
                }
                .await;
                if let Err(e) = res {
                    tracing::error!("Error mirroring registry index: {e}");
                    tracing::debug!("{e:?}");
                    break;
                }
            }
        });
        Ok(Arc::new(Self {
            db,
            _sync: sync.into(),
        }))
    }
}

/// The signer's shared mirror, created when no subscription holds it.
pub async fn index_mirror(
    mirrors: &IndexMirrors,
    main: &TypedPatchDb<RegistryDatabase>,
    signer: Option<Guid>,
) -> Result<Arc<IndexMirror>, Error> {
    let mut mirrors = mirrors.lock().await;
    if let Some(mirror) = mirrors.get(&signer).and_then(Weak::upgrade) {
        return Ok(mirror);
    }
    let mirror = IndexMirror::spawn(main, signer.clone()).await?;
    mirrors.retain(|_, m| m.strong_count() > 0);
    mirrors.insert(signer, Arc::downgrade(&mirror));
    Ok(mirror)
}

/// A registry database root holding only `/index`, without the packages hidden from the signer.
fn visible_root(root: &Value, signer: Option<&Guid>) -> Result<Value, Error> {
    let db: Model<RegistryDatabase> = root.clone().into();
    let reader = match signer {
        Some(signer) => Reader::for_signer(&db, signer)?,
        None => None,
    };
    let mut index = db.into_index();
    index.as_package_mut().retain_visible(reader.as_ref())?;
    Ok(imbl_value::json!({ "index": index.into_value() }))
}

#[cfg(test)]
mod tests {
    use imbl_value::json;

    use super::*;

    fn package(private: bool, reader: &Guid) -> Value {
        json!({
            "authorized": {},
            "categories": [],
            "versions": { "1.0.0:0": { "releaseNotes": "notes" } },
            "private": private,
            "readers": [reader],
        })
    }

    fn root(private: bool, reader: &Guid, admins: &[&Guid]) -> Value {
        json!({
            "migrations": [],
            "admins": admins,
            "index": {
                "name": null,
                "icon": null,
                "description": null,
                "os": { "versions": {} },
                "signers": {
                    reader.as_ref(): { "name": "reader", "contact": [], "keys": [] },
                },
                "package": {
                    "categories": {},
                    "packages": {
                        "public": package(false, reader),
                        "secret": package(private, reader),
                    },
                },
            },
        })
    }

    fn packages(root: &Value) -> Vec<String> {
        root["index"]["package"]["packages"]
            .as_object()
            .unwrap()
            .keys()
            .map(|k| k.to_string())
            .collect()
    }

    #[test]
    fn visible_root_hides_private_packages_from_everyone_else() {
        let reader = Guid::new();
        let private = root(true, &reader, &[]);
        let visible = visible_root(&private, None).unwrap();
        assert_eq!(packages(&visible), ["public"]);
        assert!(visible.get("admins").is_none());
        assert_eq!(
            packages(&visible_root(&private, Some(&Guid::new())).unwrap()),
            ["public"]
        );
        assert_eq!(
            packages(&visible_root(&private, Some(&reader)).unwrap()),
            ["public", "secret"]
        );
        assert_eq!(
            packages(&visible_root(&root(false, &reader, &[]), None).unwrap()),
            ["public", "secret"]
        );
    }

    #[tokio::test]
    async fn mirror_follows_visibility_changes() {
        let reader = Guid::new();
        let main = TypedPatchDb::<RegistryDatabase>::load_unchecked(PatchDb::in_memory());
        main.put(&JsonPointer::<&str>::default(), &root(true, &reader, &[]))
            .await
            .unwrap();
        let mirrors = IndexMirrors::default();
        let public = index_mirror(&mirrors, &main, None).await.unwrap();
        let shared = index_mirror(&mirrors, &main, None).await.unwrap();
        assert!(Arc::ptr_eq(&public, &shared));
        let own = index_mirror(&mirrors, &main, Some(reader.clone()))
            .await
            .unwrap();
        let (dump, mut sub) = public.db.dump_and_sub("/index".parse().unwrap()).await;
        assert_eq!(packages(&json!({ "index": dump.value })), ["public"]);
        assert_eq!(
            packages(&own.db.dump(&JsonPointer::<&str>::default()).await.value),
            ["public", "secret"]
        );

        main.mutate(|db| {
            db.as_index_mut()
                .as_package_mut()
                .as_packages_mut()
                .as_idx_mut(&"secret".parse().unwrap())
                .unwrap()
                .as_private_mut()
                .ser(&false)
        })
        .await
        .result
        .unwrap();
        sub.recv().await.unwrap();
        assert_eq!(
            packages(&public.db.dump(&JsonPointer::<&str>::default()).await.value),
            ["public", "secret"]
        );

        drop((public, shared));
        let fresh = index_mirror(&mirrors, &main, None).await.unwrap();
        assert_eq!(mirrors.lock().await.len(), 2);
        drop(fresh);
    }
}
