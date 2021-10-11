use std::collections::BTreeMap;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::Arc;

use reqwest::{Client, Url};
use sequence_trie::SequenceTrie;
use serde::Serialize;
use tracing::Subscriber;
use tracing_subscriber::filter::LevelFilter;
use tracing_subscriber::Layer;

use crate::version::COMMIT_HASH;

#[derive(Clone, Debug)]
pub struct ModuleMap {
    trie: SequenceTrie<String, (LevelFilter, bool)>,
}
impl ModuleMap {
    fn new(vals: BTreeMap<String, LevelFilter>) -> Self {
        let mut module_trie = SequenceTrie::new();
        for (k, v) in vals {
            let mut include_submodules = false;
            let module_key = k
                .split("::")
                .take_while(|&s| {
                    if s == "*" {
                        include_submodules = true;
                        false
                    } else {
                        true
                    }
                })
                .collect::<Vec<&str>>();
            match module_trie.get_node(module_key.clone()) {
                None => match module_trie.get_ancestor_node(module_key.clone()) {
                    None => {
                        module_trie.insert(module_key, (v, include_submodules));
                    }
                    Some(ancestor) => match ancestor.value() {
                        None => {
                            module_trie.insert(module_key, (v, include_submodules));
                        }
                        Some((_, sub)) => {
                            if !sub {
                                module_trie.insert(module_key, (v, include_submodules));
                            }
                        }
                    },
                },
                Some(n) => match n.value() {
                    None => {
                        module_trie.insert(module_key.clone(), (v, include_submodules));
                        if include_submodules {
                            let new_node = module_trie.get_node_mut(module_key).unwrap(); // we just inserted it
                            let child_keys = new_node
                                .children_with_keys()
                                .into_iter()
                                .map(|x| x.0.clone())
                                .collect::<Vec<String>>();
                            for c in child_keys {
                                new_node.remove(std::iter::once(&c));
                            }
                        }
                    }
                    Some(_) => unreachable!("Trie build failed on 'impossible' duplicate"),
                },
            }
        }
        ModuleMap { trie: module_trie }
    }
    fn level_for<'a>(&'a self, k: &str) -> &'a LevelFilter {
        let module_key = k.split("::");
        match self.trie.get(module_key.clone()) {
            None => match self.trie.get_ancestor(module_key) {
                None => &LevelFilter::OFF,
                Some((level_filter, include_submodules)) => {
                    if *include_submodules {
                        level_filter
                    } else {
                        &LevelFilter::OFF
                    }
                }
            },
            Some((level_filter, _)) => level_filter,
        }
    }
}

pub struct SharingLayer {
    log_epoch: Arc<AtomicU64>,
    sharing: Arc<AtomicBool>,
    share_dest: String,
}
impl<S: Subscriber> Layer<S> for SharingLayer {
    fn on_event(
        &self,
        event: &tracing::Event<'_>,
        _ctx: tracing_subscriber::layer::Context<'_, S>,
    ) {
        if self.sharing.load(Ordering::SeqCst) {
            #[derive(Serialize)]
            #[serde(rename_all = "kebab-case")]
            struct LogRequest<'a> {
                log_epoch: u64,
                commit_hash: &'static str,
                file: Option<&'a str>,
                line: Option<u32>,
                target: &'a str,
                level: &'static str,
                message: Option<String>,
            }
            if event.metadata().level() <= &tracing::Level::WARN {
                let body = LogRequest {
                    log_epoch: self.log_epoch.load(Ordering::SeqCst),
                    commit_hash: COMMIT_HASH,
                    file: event.metadata().file(),
                    line: event.metadata().line(),
                    target: event.metadata().target(),
                    level: event.metadata().level().as_str(),
                    message: event
                        .fields()
                        .find(|f| f.name() == "message")
                        .map(|f| f.to_string()),
                };
                // we don't care about the result and need it to be fast
                tokio::spawn(Client::new().post(&self.share_dest).json(&body).send());
            }
        }
    }
}

#[derive(Clone)]
pub struct EmbassyLogger {
    log_epoch: Arc<AtomicU64>,
    sharing: Arc<AtomicBool>,
}
impl EmbassyLogger {
    fn base_subscriber() -> impl Subscriber {
        use tracing_error::ErrorLayer;
        use tracing_subscriber::prelude::*;
        use tracing_subscriber::{fmt, EnvFilter};

        let filter_layer = EnvFilter::from_default_env();
        let fmt_layer = fmt::layer().with_target(true);

        tracing_subscriber::registry()
            .with(filter_layer)
            .with(fmt_layer)
            .with(ErrorLayer::default())
    }
    pub fn no_sharing() {
        use tracing_subscriber::prelude::*;

        Self::base_subscriber().init();
        color_eyre::install().expect("Color Eyre Init");
    }
    pub fn init(log_epoch: Arc<AtomicU64>, share_dest: Option<Url>, share_errors: bool) -> Self {
        use tracing_subscriber::prelude::*;

        let share_dest = match share_dest {
            None => "https://beta-registry-0-3.start9labs.com/error-logs".to_owned(), // TODO
            Some(a) => a.to_string(),
        };
        let sharing = Arc::new(AtomicBool::new(share_errors));
        let sharing_layer = SharingLayer {
            log_epoch: log_epoch.clone(),
            share_dest,
            sharing: sharing.clone(),
        };

        Self::base_subscriber().with(sharing_layer).init();
        color_eyre::install().expect("Color Eyre Init");

        EmbassyLogger { log_epoch, sharing }
    }
    pub fn set_sharing(&self, sharing: bool) {
        self.sharing.store(sharing, Ordering::SeqCst)
    }
}

#[tokio::test]
pub async fn order_level() {
    assert!(tracing::Level::WARN > tracing::Level::ERROR)
}

#[test]
pub fn module() {
    println!("{}", module_path!())
}

proptest::proptest! {
    #[test]
    fn submodules_handled_by_parent(s0 in "[a-z][a-z0-9_]+", s1 in "[a-z][a-z0-9_]+", level in filter_strategy()) {
        proptest::prop_assume!(level > LevelFilter::OFF);
        let mut hm = BTreeMap::new();
        hm.insert(format!("{}::*", s0.clone()), level);
        let mod_map = ModuleMap::new(hm);
        proptest::prop_assert_eq!(mod_map.level_for(&format!("{}::{}", s0, s1)), &level)
    }
    #[test]
    fn submodules_ignored_by_parent(s0 in "[a-z][a-z0-9_]+", s1 in "[a-z][a-z0-9_]+", level in filter_strategy()) {
        proptest::prop_assume!(level > LevelFilter::OFF);
        let mut hm = BTreeMap::new();
        hm.insert(s0.clone(), level);
        let mod_map = ModuleMap::new(hm);
        proptest::prop_assert_eq!(mod_map.level_for(&format!("{}::{}", s0, s1)), &LevelFilter::OFF)
    }
    #[test]
    fn duplicate_insertion_ignored(s0 in "[a-z][a-z0-9_]+", s1 in "[a-z][a-z0-9_]+", level in filter_strategy()) {
        proptest::prop_assume!(level > LevelFilter::OFF);
        let mut hm = BTreeMap::new();
        hm.insert(format!("{}::*", s0.clone()), level);
        let sub = format!("{}::{}", s0, s1);
        hm.insert(sub.clone(), level);
        let mod_map = ModuleMap::new(hm);
        proptest::prop_assert_eq!(mod_map.trie.get(sub.split("::")), None)
    }
    #[test]
    fn parent_child_simul(s0 in "[a-z][a-z0-9_]+", s1 in "[a-z][a-z0-9_]+", level0 in filter_strategy(), level1 in filter_strategy()) {
        let mut hm = BTreeMap::new();
        hm.insert(s0.clone(), level0);
        let sub = format!("{}::{}", s0, s1);
        hm.insert(sub.clone(), level1);
        let mod_map = ModuleMap::new(hm);
        proptest::prop_assert_eq!(mod_map.level_for(&s0), &level0);
        proptest::prop_assert_eq!(mod_map.level_for(&sub), &level1);
    }
}
fn filter_strategy() -> impl proptest::strategy::Strategy<Value = LevelFilter> {
    use proptest::strategy::Just;
    proptest::prop_oneof![
        Just(LevelFilter::OFF),
        Just(LevelFilter::ERROR),
        Just(LevelFilter::WARN),
        Just(LevelFilter::INFO),
        Just(LevelFilter::DEBUG),
        Just(LevelFilter::TRACE),
    ]
}
