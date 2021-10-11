use std::collections::BTreeMap;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::Arc;

use reqwest::Url;
use sequence_trie::SequenceTrie;
use tracing_subscriber::filter::LevelFilter;

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

#[derive(Clone)]
pub struct EmbassyLogger {
    log_level: LevelFilter,
    log_epoch: Arc<AtomicU64>,
    sharing: Arc<AtomicBool>,
    share_dest: Url,
    module_map: ModuleMap,
}
impl EmbassyLogger {
    pub fn init(
        log_level: LevelFilter,
        log_epoch: Arc<AtomicU64>,
        share_dest: Option<Url>,
        share_errors: bool,
        module_map: BTreeMap<String, LevelFilter>,
    ) -> Self {
        let share_dest = match share_dest {
            None => Url::parse("https://beta-registry-0-3.start9labs.com/error-logs").unwrap(), // TODO
            Some(a) => a,
        };
        use tracing_error::ErrorLayer;
        use tracing_subscriber::prelude::*;
        use tracing_subscriber::{fmt, EnvFilter};

        let fmt_layer = fmt::layer().with_target(false);
        let filter_layer = EnvFilter::from_default_env().add_directive(log_level.into());

        tracing_subscriber::registry()
            .with(filter_layer)
            .with(fmt_layer)
            .with(ErrorLayer::default())
            .init();

        color_eyre::install().expect("Color Eyre Init");
        let embassy_logger = EmbassyLogger {
            log_level,
            log_epoch,
            sharing: Arc::new(AtomicBool::new(share_errors)),
            share_dest: share_dest,
            module_map: ModuleMap::new(module_map),
        };
        embassy_logger
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
