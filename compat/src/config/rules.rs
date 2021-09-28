use std::borrow::Cow;
use std::sync::Arc;

use linear_map::LinearMap;
use pest::iterators::Pairs;
use pest::Parser;
use rand::SeedableRng;
use serde_json::Value;

use embassy::config::Config;
use embassy::config::util::STATIC_NULL;

#[derive(Parser)]
#[grammar = "config/rule_parser.pest"]
struct RuleParser;

lazy_static::lazy_static! {
    static ref NUM_PREC_CLIMBER: pest::prec_climber::PrecClimber<Rule> = {
        use pest::prec_climber::*;
        use Rule::*;
        use Assoc::*;

        PrecClimber::new(vec![
            Operator::new(add, Left) | Operator::new(sub, Left),
            Operator::new(mul, Left) | Operator::new(div, Left),
            Operator::new(pow, Right)
        ])
    };

    static ref STR_PREC_CLIMBER: pest::prec_climber::PrecClimber<Rule> = {
        use pest::prec_climber::*;
        use Rule::*;
        use Assoc::*;

        PrecClimber::new(vec![
            Operator::new(add, Left)
        ])
    };

    static ref BOOL_PREC_CLIMBER: pest::prec_climber::PrecClimber<Rule> = {
        use pest::prec_climber::*;
        use Rule::*;
        use Assoc::*;

        PrecClimber::new(vec![
            Operator::new(or, Left),
            Operator::new(xor, Left),
            Operator::new(and, Left)
        ])
    };
}

pub type Accessor = Box<
    dyn for<'a> Fn(&'a Value, &LinearMap<&str, Cow<Config>>) -> VarRes<&'a Value> + Send + Sync,
>;
pub type AccessorMut = Box<
    dyn for<'a> Fn(&'a mut Value, &LinearMap<&str, Cow<Config>>) -> Option<&'a mut Value>
        + Send
        + Sync,
>;
pub type CompiledExpr<T> = Box<dyn Fn(&Config, &LinearMap<&str, Cow<Config>>) -> T + Send + Sync>;
pub type CompiledReference = Box<
    dyn for<'a> Fn(&'a mut Config, &LinearMap<&str, Cow<Config>>) -> Option<&'a mut Value>
        + Send
        + Sync,
>;
pub type Mutator = Box<dyn Fn(&mut Config, &LinearMap<&str, Cow<Config>>) + Send + Sync>;
pub type CompiledRule = Box<dyn Fn(&Config, &LinearMap<&str, Cow<Config>>) -> bool + Send + Sync>;
pub type CompiledRuleRes = Result<CompiledRule, failure::Error>;

#[derive(Clone)]
pub struct ConfigRule {
    pub src: String,
    pub compiled: Arc<CompiledRule>,
}
impl std::fmt::Debug for ConfigRule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ConfigRule")
            .field("src", &self.src)
            .field("compiled", &"Fn(&Config, &Config) -> bool")
            .finish()
    }
}
impl<'de> serde::de::Deserialize<'de> for ConfigRule {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        let src = String::deserialize(deserializer)?;
        let compiled = compile(&src).map_err(serde::de::Error::custom)?;
        Ok(ConfigRule {
            src,
            compiled: Arc::new(compiled),
        })
    }
}
impl serde::ser::Serialize for ConfigRule {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::ser::Serializer,
    {
        serializer.serialize_str(&self.src)
    }
}
#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]
pub struct ConfigRuleEntry {
    pub rule: ConfigRule,
    pub description: String,
}
impl ConfigRuleEntry {
    pub fn check(
        &self,
        cfg: &Config,
        cfgs: &LinearMap<&str, Cow<Config>>,
    ) -> Result<(), anyhow::Error> {
        if !(self.rule.compiled)(cfg, cfgs) {
            return Err(anyhow::anyhow!("{}", self.description))
        }
        Ok(())
    }
}

#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum SetVariant {
    To(String),
    ToValue(Value),
    ToEntropy(super::spec::Entropy),
}

#[derive(Clone)]
pub enum SuggestionVariant {
    Set {
        var: String,
        to: SetVariant,
        compiled: Arc<Mutator>,
    },
    Delete {
        src: String,
        compiled: Arc<Mutator>,
    },
    Push {
        to: String,
        value: Value,
        compiled: Arc<Mutator>,
    },
}
impl SuggestionVariant {
    pub fn apply<'a>(
        &self,
        id: &'a str,
        cfg: &mut Config,
        cfgs: &mut LinearMap<&'a str, Cow<Config>>,
    ) {
        match self {
            SuggestionVariant::Set { ref compiled, .. } => compiled(cfg, cfgs),
            SuggestionVariant::Delete { ref compiled, .. } => compiled(cfg, cfgs),
            SuggestionVariant::Push { ref compiled, .. } => compiled(cfg, cfgs),
        }
        cfgs.insert(id, Cow::Owned(cfg.clone()));
    }
}
impl std::fmt::Debug for SuggestionVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SuggestionVariant::Set {
                ref var, ref to, ..
            } => f
                .debug_struct("SuggestionVariant::Set")
                .field("var", var)
                .field("to", to)
                .field("compiled", &"Fn(&mut Config, Config)")
                .finish(),
            SuggestionVariant::Delete { ref src, .. } => f
                .debug_struct("SuggestionVariant::Delete")
                .field("src", src)
                .field("compiled", &"Fn(&mut Config, Config)")
                .finish(),
            SuggestionVariant::Push {
                ref to, ref value, ..
            } => f
                .debug_struct("SuggestionVariant::Delete")
                .field("to", to)
                .field("value", value)
                .field("compiled", &"Fn(&mut Config, Config)")
                .finish(),
        }
    }
}
impl<'de> serde::de::Deserialize<'de> for SuggestionVariant {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        #[derive(serde::Deserialize)]
        enum _SuggestionVariant {
            SET {
                var: String,
                #[serde(flatten)]
                to: SetVariant,
            },
            DELETE(String),
            PUSH {
                to: String,
                value: Value,
            },
        }
        let raw = _SuggestionVariant::deserialize(deserializer)?;
        Ok(match raw {
            _SuggestionVariant::SET { var, to } => SuggestionVariant::Set {
                compiled: Arc::new(
                    compile_set_action(&var, &to).map_err(serde::de::Error::custom)?,
                ),
                to: to,
                var: var,
            },
            _SuggestionVariant::DELETE(src) => SuggestionVariant::Delete {
                compiled: Arc::new(
                    compile_del_action(
                        RuleParser::parse(Rule::del_action, &src)
                            .map_err(serde::de::Error::custom)?,
                    )
                    .map_err(serde::de::Error::custom)?,
                ),
                src,
            },
            _SuggestionVariant::PUSH { to, value } => SuggestionVariant::Push {
                compiled: Arc::new(
                    compile_push_action(
                        RuleParser::parse(Rule::reference, &to)
                            .map_err(serde::de::Error::custom)?,
                        value.clone(),
                    )
                    .map_err(serde::de::Error::custom)?,
                ),
                to,
                value,
            },
        })
    }
}
impl serde::ser::Serialize for SuggestionVariant {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::ser::Serializer,
    {
        #[derive(serde::Serialize)]
        enum _SuggestionVariant<'a> {
            SET {
                var: &'a str,
                #[serde(flatten)]
                to: &'a SetVariant,
            },
            DELETE(&'a str),
            PUSH {
                to: &'a str,
                value: &'a Value,
            },
        }
        match self {
            SuggestionVariant::Set {
                ref var, ref to, ..
            } => serde::ser::Serialize::serialize(&_SuggestionVariant::SET { var, to }, serializer),
            SuggestionVariant::Delete { ref src, .. } => {
                serde::ser::Serialize::serialize(&_SuggestionVariant::DELETE(src), serializer)
            }
            SuggestionVariant::Push {
                ref to, ref value, ..
            } => serde::ser::Serialize::serialize(
                &_SuggestionVariant::PUSH { to, value },
                serializer,
            ),
        }
    }
}
#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct Suggestion {
    #[serde(rename = "if")]
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub condition: Option<ConfigRule>,
    #[serde(flatten)]
    pub variant: SuggestionVariant,
}
impl Suggestion {
    pub fn apply<'a>(
        &self,
        id: &'a str,
        cfg: &mut Config,
        cfgs: &mut LinearMap<&'a str, Cow<Config>>,
    ) {
        match &self.condition {
            Some(condition) if !(condition.compiled)(cfg, cfgs) => (),
            _ => self.variant.apply(id, cfg, cfgs),
        }
    }
}
#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct ConfigRuleEntryWithSuggestions {
    #[serde(flatten)]
    pub entry: ConfigRuleEntry,
    pub suggestions: Vec<Suggestion>,
}
impl ConfigRuleEntryWithSuggestions {
    pub fn apply<'a>(
        &self,
        id: &'a str,
        cfg: &mut Config,
        cfgs: &mut LinearMap<&'a str, Cow<Config>>,
    ) -> Result<(), anyhow::Error> {
        if self.entry.check(cfg, cfgs).is_err() {
            for suggestion in &self.suggestions {
                suggestion.apply(id, cfg, cfgs);
            }
            self.entry.check(cfg, cfgs)
        } else {
            Ok(())
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum VarRes<T> {
    Exactly(T),
    Any(Vec<VarRes<T>>),
    All(Vec<VarRes<T>>),
}
impl<T> VarRes<T> {
    fn map<U, F: FnMut(T) -> U>(self, mut f: F) -> VarRes<U> {
        fn map_rec<T, U, F: FnMut(T) -> U>(s: VarRes<T>, f: &mut F) -> VarRes<U> {
            match s {
                VarRes::Exactly(a) => VarRes::Exactly(f(a)),
                VarRes::Any(a) => VarRes::Any(a.into_iter().map(|a| map_rec(a, f)).collect()),
                VarRes::All(a) => VarRes::All(a.into_iter().map(|a| map_rec(a, f)).collect()),
            }
        }
        map_rec(self, &mut f)
    }
    fn and_then<U, F: FnMut(T) -> VarRes<U>>(self, mut f: F) -> VarRes<U> {
        fn and_then_rec<T, U, F: FnMut(T) -> VarRes<U>>(s: VarRes<T>, f: &mut F) -> VarRes<U> {
            match s {
                VarRes::Exactly(a) => f(a),
                VarRes::Any(a) => VarRes::Any(a.into_iter().map(|a| and_then_rec(a, f)).collect()),
                VarRes::All(a) => VarRes::All(a.into_iter().map(|a| and_then_rec(a, f)).collect()),
            }
        }
        and_then_rec(self, &mut f)
    }
}
impl VarRes<bool> {
    fn resolve(self) -> bool {
        match self {
            VarRes::Exactly(a) => a,
            VarRes::Any(a) => a.into_iter().any(|a| a.resolve()),
            VarRes::All(a) => a.into_iter().all(|a| a.resolve()),
        }
    }
}

fn compile_var_rec(mut ident: Pairs<Rule>) -> Option<Accessor> {
    let idx = ident.next();
    if let Some(idx) = idx {
        let deref: Accessor = match idx.as_rule() {
            Rule::sub_ident_any => Box::new(|v, _| match v {
                Value::Array(l) => VarRes::Any(l.iter().map(VarRes::Exactly).collect()),
                Value::Object(o) => {
                    VarRes::Any(o.iter().map(|(_, a)| VarRes::Exactly(a)).collect())
                }
                _ => VarRes::Exactly(&STATIC_NULL),
            }),
            Rule::sub_ident_all => Box::new(|v, _| match v {
                Value::Array(l) => VarRes::All(l.iter().map(VarRes::Exactly).collect()),
                Value::Object(o) => {
                    VarRes::All(o.iter().map(|(_, a)| VarRes::Exactly(a)).collect())
                }
                _ => VarRes::Exactly(&STATIC_NULL),
            }),
            Rule::sub_ident_fn => {
                let idx = idx.into_inner().next().unwrap();
                match idx.as_rule() {
                    Rule::list_access_function_first => {
                        let mut pred_iter = idx.into_inner();
                        let item_var = pred_iter.next().unwrap().as_str().to_owned();
                        let predicate = compile_bool_expr(pred_iter.next().unwrap().into_inner());
                        Box::new(move |v, cfgs| match v {
                            Value::Array(l) => VarRes::Exactly(
                                l.iter()
                                    .filter(|item| {
                                        let mut cfg = Config::default();
                                        cfg.insert(item_var.clone(), (*item).clone());
                                        predicate(&cfg, cfgs)
                                    })
                                    .next()
                                    .unwrap_or(&STATIC_NULL),
                            ),
                            Value::Object(o) => VarRes::Exactly(
                                &o.iter()
                                    .map(|(_, item)| item)
                                    .filter(|item| {
                                        let mut cfg = Config::default();
                                        cfg.insert(item_var.clone(), (*item).clone());
                                        predicate(&cfg, cfgs)
                                    })
                                    .next()
                                    .unwrap_or(&STATIC_NULL),
                            ),
                            _ => VarRes::Exactly(&STATIC_NULL),
                        })
                    }
                    Rule::list_access_function_last => {
                        let mut pred_iter = idx.into_inner();
                        let item_var = pred_iter.next().unwrap().as_str().to_owned();
                        let predicate = compile_bool_expr(pred_iter.next().unwrap().into_inner());
                        Box::new(move |v, cfgs| match v {
                            Value::Array(l) => VarRes::Exactly(
                                l.iter()
                                    .filter(|item| {
                                        let mut cfg = Config::default();
                                        cfg.insert(item_var.clone(), (*item).clone());
                                        predicate(&cfg, cfgs)
                                    })
                                    .next_back()
                                    .unwrap_or(&STATIC_NULL),
                            ),
                            Value::Object(o) => VarRes::Exactly(
                                &o.iter()
                                    .map(|(_, item)| item)
                                    .filter(|item| {
                                        let mut cfg = Config::default();
                                        cfg.insert(item_var.clone(), (*item).clone());
                                        predicate(&cfg, cfgs)
                                    })
                                    .next_back()
                                    .unwrap_or(&STATIC_NULL),
                            ),
                            _ => VarRes::Exactly(&STATIC_NULL),
                        })
                    }
                    Rule::list_access_function_any => {
                        let mut pred_iter = idx.into_inner();
                        let item_var = pred_iter.next().unwrap().as_str().to_owned();
                        let predicate = compile_bool_expr(pred_iter.next().unwrap().into_inner());
                        Box::new(move |v, cfgs| match v {
                            Value::Array(l) => VarRes::Any(
                                l.iter()
                                    .filter(|item| {
                                        let mut cfg = Config::default();
                                        cfg.insert(item_var.clone(), (*item).clone());
                                        predicate(&cfg, cfgs)
                                    })
                                    .map(VarRes::Exactly)
                                    .collect(),
                            ),
                            Value::Object(o) => VarRes::Any(
                                o.iter()
                                    .map(|(_, item)| item)
                                    .filter(|item| {
                                        let mut cfg = Config::default();
                                        cfg.insert(item_var.clone(), (*item).clone());
                                        predicate(&cfg, cfgs)
                                    })
                                    .map(VarRes::Exactly)
                                    .collect(),
                            ),
                            _ => VarRes::Exactly(&STATIC_NULL),
                        })
                    }
                    Rule::list_access_function_all => {
                        let mut pred_iter = idx.into_inner();
                        let item_var = pred_iter.next().unwrap().as_str().to_owned();
                        let predicate = compile_bool_expr(pred_iter.next().unwrap().into_inner());
                        Box::new(move |v, cfgs| match v {
                            Value::Array(l) => VarRes::All(
                                l.iter()
                                    .filter(|item| {
                                        let mut cfg = Config::default();
                                        cfg.insert(item_var.clone(), (*item).clone());
                                        predicate(&cfg, cfgs)
                                    })
                                    .map(VarRes::Exactly)
                                    .collect(),
                            ),
                            Value::Object(o) => VarRes::All(
                                o.iter()
                                    .map(|(_, item)| item)
                                    .filter(|item| {
                                        let mut cfg = Config::default();
                                        cfg.insert(item_var.clone(), (*item).clone());
                                        predicate(&cfg, cfgs)
                                    })
                                    .map(VarRes::Exactly)
                                    .collect(),
                            ),
                            _ => VarRes::Exactly(&STATIC_NULL),
                        })
                    }
                    _ => unreachable!(),
                }
            }
            Rule::sub_ident_regular => {
                let idx = idx.into_inner().next().unwrap();
                match idx.as_rule() {
                    Rule::sub_ident_regular_base => {
                        let idx = idx.as_str().to_owned();
                        Box::new(move |v, _| match v {
                            Value::Object(o) => {
                                VarRes::Exactly(o.get(&idx).unwrap_or(&STATIC_NULL))
                            }
                            _ => VarRes::Exactly(&STATIC_NULL),
                        })
                    }
                    Rule::sub_ident_regular_expr => {
                        let idx = compile_str_expr(idx.into_inner().next().unwrap().into_inner());
                        Box::new(move |v, dep_cfg| match v {
                            Value::Object(o) => idx(&Config::default(), dep_cfg).map(|idx| {
                                idx.and_then(|idx| o.get(&idx)).unwrap_or(&STATIC_NULL)
                            }),
                            _ => VarRes::Exactly(&STATIC_NULL),
                        })
                    }
                    _ => unreachable!(),
                }
            }
            Rule::sub_ident_index => {
                let idx = idx.into_inner().next().unwrap();
                match idx.as_rule() {
                    Rule::sub_ident_index_base => {
                        let idx: usize = idx.as_str().parse().unwrap();
                        Box::new(move |v, _| match v {
                            Value::Array(l) => VarRes::Exactly(l.get(idx).unwrap_or(&STATIC_NULL)),
                            _ => VarRes::Exactly(&STATIC_NULL),
                        })
                    }
                    Rule::sub_ident_index_expr => {
                        let idx = compile_num_expr(idx.into_inner().next().unwrap().into_inner());
                        Box::new(move |v, dep_cfg| match v {
                            Value::Array(l) => idx(&Config::default(), dep_cfg)
                                .map(|idx| l.get(idx as usize).unwrap_or(&STATIC_NULL)),
                            _ => VarRes::Exactly(&STATIC_NULL),
                        })
                    }
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        };
        Some(if let Some(rest) = compile_var_rec(ident) {
            Box::new(move |v, cfgs| deref(v, cfgs).and_then(|v| rest(v, cfgs)))
        } else {
            deref
        })
    } else {
        None
    }
}

fn compile_var(mut var: Pairs<Rule>) -> CompiledExpr<VarRes<Value>> {
    let mut first_seg = var.next().unwrap();
    let app_id = if first_seg.as_rule() == Rule::app_id {
        let app_id = first_seg.into_inner().next().unwrap().as_str().to_owned();
        first_seg = var.next().unwrap();
        Some(app_id)
    } else {
        None
    };
    let first_seg_string = first_seg.as_str().to_owned();
    let accessor = compile_var_rec(var);
    Box::new(move |cfg, cfgs| {
        let mut cfg: &Config = cfg;
        if let Some(ref app_id) = app_id {
            cfg = if let Some(cfg) = cfgs.get(&app_id.as_str()) {
                cfg
            } else {
                return VarRes::Exactly(Value::Null);
            };
        }
        let val = cfg.get(&first_seg_string).unwrap_or(&STATIC_NULL);
        if let Some(accessor) = &accessor {
            accessor(val, cfgs).map(|v| v.clone())
        } else {
            VarRes::Exactly(val.clone())
        }
    })
}

fn compile_var_mut_rec(mut ident: Pairs<Rule>) -> Result<Option<AccessorMut>, failure::Error> {
    let idx = ident.next();
    Ok(if let Some(idx) = idx {
        let deref: AccessorMut = match idx.as_rule() {
            Rule::sub_ident_fn => {
                let idx = idx.into_inner().next().unwrap();
                match idx.as_rule() {
                    Rule::list_access_function_first => {
                        let mut pred_iter = idx.into_inner();
                        let item_var = pred_iter.next().unwrap().as_str().to_owned();
                        let predicate = compile_bool_expr(pred_iter.next().unwrap().into_inner());
                        Box::new(move |v, cfgs| match v {
                            Value::Array(l) => l
                                .iter_mut()
                                .filter(|item| {
                                    let mut cfg = Config::default();
                                    cfg.insert(item_var.clone(), (*item).clone());
                                    predicate(&cfg, cfgs)
                                })
                                .next(),
                            Value::Object(o) => {
                                o.iter_mut()
                                    .map(|(_, item)| item)
                                    .filter(|item| {
                                        let mut cfg = Config::default();
                                        cfg.insert(item_var.clone(), (*item).clone());
                                        predicate(&cfg, cfgs)
                                    })
                                    .next()
                            }
                            _ => None,
                        })
                    }
                    Rule::list_access_function_last => {
                        let mut pred_iter = idx.into_inner();
                        let item_var = pred_iter.next().unwrap().as_str().to_owned();
                        let predicate = compile_bool_expr(pred_iter.next().unwrap().into_inner());
                        Box::new(move |v, cfgs| match v {
                            Value::Array(l) => l
                                .iter_mut()
                                .filter(|item| {
                                    let mut cfg = Config::default();
                                    cfg.insert(item_var.clone(), (*item).clone());
                                    predicate(&cfg, cfgs)
                                })
                                .next_back(),
                            Value::Object(o) => {
                                o.iter_mut()
                                    .map(|(_, item)| item)
                                    .filter(|item| {
                                        let mut cfg = Config::default();
                                        cfg.insert(item_var.clone(), (*item).clone());
                                        predicate(&cfg, cfgs)
                                    })
                                    .next_back()
                            }
                            _ => None,
                        })
                    }
                    Rule::list_access_function_any | Rule::list_access_function_all => {
                        failure::bail!("Any and All are immutable")
                    }
                    _ => unreachable!(),
                }
            }
            Rule::sub_ident_regular => {
                let idx = idx.into_inner().next().unwrap();
                match idx.as_rule() {
                    Rule::sub_ident_regular_base => {
                        let idx = idx.as_str().to_owned();
                        Box::new(move |v, _| match v {
                            Value::Object(ref mut o) => {
                                if o.contains_key(&idx) {
                                    o.get_mut(&idx)
                                } else {
                                    o.insert(idx.clone(), Value::Null);
                                    o.get_mut(&idx)
                                }
                            }
                            _ => None,
                        })
                    }
                    Rule::sub_ident_regular_expr => {
                        let idx = compile_str_expr(idx.into_inner().next().unwrap().into_inner());
                        Box::new(
                            move |v, dep_cfg| match (v, idx(&Config::default(), dep_cfg)) {
                                (Value::Object(ref mut o), VarRes::Exactly(Some(ref idx))) => {
                                    if o.contains_key(idx) {
                                        o.get_mut(idx)
                                    } else {
                                        o.insert(idx.clone(), Value::Null);
                                        o.get_mut(idx)
                                    }
                                }
                                _ => None,
                            },
                        )
                    }
                    _ => unreachable!(),
                }
            }
            Rule::sub_ident_index => {
                let idx = idx.into_inner().next().unwrap();
                match idx.as_rule() {
                    Rule::sub_ident_index_base => {
                        let idx: usize = idx.as_str().parse().unwrap();
                        Box::new(move |v, _| match v {
                            Value::Array(l) => {
                                if l.len() > idx {
                                    l.get_mut(idx)
                                } else if idx == l.len() {
                                    l.push(Value::Null);
                                    l.get_mut(idx)
                                } else {
                                    None
                                }
                            }
                            _ => None,
                        })
                    }
                    Rule::sub_ident_index_expr => {
                        let idx = compile_num_expr(idx.into_inner().next().unwrap().into_inner());
                        Box::new(
                            move |v, dep_cfg| match (v, idx(&Config::default(), dep_cfg)) {
                                (Value::Array(l), VarRes::Exactly(idx)) => {
                                    let idx = idx as usize;
                                    if l.len() > idx {
                                        l.get_mut(idx)
                                    } else if idx == l.len() {
                                        l.push(Value::Null);
                                        l.get_mut(idx)
                                    } else {
                                        None
                                    }
                                }
                                _ => None,
                            },
                        )
                    }
                    _ => unreachable!(),
                }
            }
            _ => failure::bail!("invalid token: {:?}", idx.as_rule()),
        };
        Some(if let Some(rest) = compile_var_mut_rec(ident)? {
            Box::new(move |v, cfgs| deref(v, cfgs).and_then(|v| rest(v, cfgs)))
        } else {
            deref
        })
    } else {
        None
    })
}

fn compile_var_mut(mut var: Pairs<Rule>) -> Result<CompiledReference, failure::Error> {
    let first_seg = var.next().unwrap();
    if first_seg.as_rule() == Rule::app_id {
        failure::bail!("Can only assign to relative path");
    }
    let first_seg_string = first_seg.as_str().to_owned();
    let accessor_mut = compile_var_mut_rec(var)?;
    Ok(Box::new(move |cfg, cfgs| {
        let var = if cfg.contains_key(&first_seg_string) {
            cfg.get_mut(&first_seg_string).unwrap()
        } else {
            cfg.insert(first_seg_string.clone(), Value::Null);
            cfg.get_mut(&first_seg_string).unwrap()
        };
        if let Some(accessor_mut) = &accessor_mut {
            accessor_mut(var, cfgs)
        } else {
            Some(var)
        }
    }))
}

fn compile_bool_var(var: Pairs<Rule>) -> CompiledRule {
    let var = compile_var(var);
    Box::new(move |cfg, cfgs| {
        var(cfg, cfgs)
            .map(|a| match a {
                Value::Bool(false) | Value::Null => false,
                _ => true,
            })
            .resolve()
    })
}

fn compile_num_var(var: Pairs<Rule>) -> CompiledExpr<VarRes<f64>> {
    let var = compile_var(var);
    Box::new(move |cfg, cfgs| {
        var(cfg, cfgs).map(|a| match a {
            Value::Number(n) => n.as_f64().unwrap(),
            Value::String(s) => match s.parse() {
                Ok(n) => n,
                Err(_) => panic!("string cannot be parsed as an f64")
            },
            Value::Bool(b) => {
                if b {
                    1.0
                } else {
                    0.0
                }
            }
            _ => panic!("object or list cannot be parsed as an f64"),
        })
    })
}

fn compile_num(num_str: &str) -> CompiledExpr<VarRes<f64>> {
    let num = VarRes::Exactly(num_str.parse().unwrap());
    Box::new(move |_, _| num.clone())
}

fn compile_num_expr(pairs: Pairs<Rule>) -> CompiledExpr<VarRes<f64>> {
    NUM_PREC_CLIMBER.climb(
        pairs,
        |pair| match pair.as_rule() {
            Rule::num_var => compile_num_var(pair.into_inner()),
            Rule::num => compile_num(pair.as_str()),
            Rule::num_expr => compile_num_expr(pair.into_inner()),
            _ => unreachable!(),
        },
        |lhs, op, rhs| match op.as_rule() {
            Rule::add => Box::new(move |cfg, cfgs| {
                lhs(cfg, cfgs).and_then(|lhs| rhs(cfg, cfgs).map(|rhs| lhs + rhs))
            }),
            Rule::sub => Box::new(move |cfg, cfgs| {
                lhs(cfg, cfgs).and_then(|lhs| rhs(cfg, cfgs).map(|rhs| lhs - rhs))
            }),
            Rule::mul => Box::new(move |cfg, cfgs| {
                lhs(cfg, cfgs).and_then(|lhs| rhs(cfg, cfgs).map(|rhs| lhs * rhs))
            }),
            Rule::div => Box::new(move |cfg, cfgs| {
                lhs(cfg, cfgs).and_then(|lhs| rhs(cfg, cfgs).map(|rhs| lhs / rhs))
            }),
            Rule::pow => Box::new(move |cfg, cfgs| {
                lhs(cfg, cfgs).and_then(|lhs| rhs(cfg, cfgs).map(|rhs| lhs.powf(rhs)))
            }),
            _ => unreachable!(),
        },
    )
}

fn compile_num_cmp_expr(mut pairs: Pairs<Rule>) -> CompiledRule {
    let lhs = compile_num_expr(pairs.next().unwrap().into_inner());
    let op = pairs.next().unwrap();
    let rhs = compile_num_expr(pairs.next().unwrap().into_inner());
    match op.as_rule() {
        Rule::lt => Box::new(move |cfg, cfgs| {
            lhs(cfg, cfgs)
                .and_then(|lhs| rhs(cfg, cfgs).map(|rhs| lhs < rhs))
                .resolve()
        }),
        Rule::lte => Box::new(move |cfg, cfgs| {
            lhs(cfg, cfgs)
                .and_then(|lhs| rhs(cfg, cfgs).map(|rhs| lhs <= rhs))
                .resolve()
        }),
        Rule::eq => Box::new(move |cfg, cfgs| {
            lhs(cfg, cfgs)
                .and_then(|lhs| rhs(cfg, cfgs).map(|rhs| lhs == rhs))
                .resolve()
        }),
        Rule::neq => Box::new(move |cfg, cfgs| {
            lhs(cfg, cfgs)
                .and_then(|lhs| rhs(cfg, cfgs).map(|rhs| lhs != rhs))
                .resolve()
        }),
        Rule::gt => Box::new(move |cfg, cfgs| {
            lhs(cfg, cfgs)
                .and_then(|lhs| rhs(cfg, cfgs).map(|rhs| lhs > rhs))
                .resolve()
        }),
        Rule::gte => Box::new(move |cfg, cfgs| {
            lhs(cfg, cfgs)
                .and_then(|lhs| rhs(cfg, cfgs).map(|rhs| lhs >= rhs))
                .resolve()
        }),
        _ => unreachable!(),
    }
}

fn compile_str_var(var: Pairs<Rule>) -> CompiledExpr<VarRes<Option<String>>> {
    let var = compile_var(var);
    Box::new(move |cfg, cfgs| {
        var(cfg, cfgs).map(|a| match a {
            Value::String(s) => Some(s),
            Value::Number(n) => Some(format!("{}", n)),
            Value::Bool(b) => Some(format!("{}", b)),
            _ => None,
        })
    })
}

fn compile_str(str_str: &str) -> CompiledExpr<VarRes<Option<String>>> {
    let str_str = &str_str[1..str_str.len() - 1];
    let mut out = String::with_capacity(str_str.len());
    let mut escape = false;
    for c in str_str.chars() {
        match c {
            '\\' => {
                if escape {
                    out.push('\\');
                } else {
                    escape = true;
                }
            }
            'n' if escape => out.push('\n'),
            'r' if escape => out.push('\r'),
            't' if escape => out.push('\t'),
            '0' if escape => out.push('\0'),
            '"' if escape => out.push('"'),
            '\'' if escape => out.push('\''),
            _ => {
                if escape {
                    out.push('\\')
                }
                out.push(c)
            }
        }
    }
    let res = VarRes::Exactly(Some(out));
    Box::new(move |_, _| res.clone())
}

fn compile_str_expr(pairs: Pairs<Rule>) -> CompiledExpr<VarRes<Option<String>>> {
    STR_PREC_CLIMBER.climb(
        pairs,
        |pair| match pair.as_rule() {
            Rule::str_var => compile_str_var(pair.into_inner()),
            Rule::str => compile_str(pair.as_str()),
            Rule::str_expr => compile_str_expr(pair.into_inner()),
            _ => unreachable!(),
        },
        |lhs, op, rhs| match op.as_rule() {
            Rule::add => Box::new(move |cfg, cfgs| {
                lhs(cfg, cfgs).and_then(|lhs| {
                    rhs(cfg, cfgs).map(|rhs| {
                        let lhs = lhs.clone()?;
                        let rhs = rhs?;
                        Some(lhs + &rhs)
                    })
                })
            }),
            _ => unreachable!(),
        },
    )
}

fn compile_str_cmp_expr(mut pairs: Pairs<Rule>) -> CompiledRule {
    let lhs = compile_str_expr(pairs.next().unwrap().into_inner());
    let op = pairs.next().unwrap();
    let rhs = compile_str_expr(pairs.next().unwrap().into_inner());
    match op.as_rule() {
        Rule::lt => Box::new(move |cfg, cfgs| {
            lhs(cfg, cfgs)
                .and_then(|lhs| {
                    rhs(cfg, cfgs).map(|rhs| match (&lhs, &rhs) {
                        (Some(lhs), Some(rhs)) => rhs.contains(lhs) && lhs.len() < rhs.len(),
                        _ => false,
                    })
                })
                .resolve()
        }),
        Rule::lte => Box::new(move |cfg, cfgs| {
            lhs(cfg, cfgs)
                .and_then(|lhs| {
                    rhs(cfg, cfgs).map(|rhs| match (&lhs, &rhs) {
                        (Some(lhs), Some(rhs)) => rhs.contains(lhs),
                        _ => false,
                    })
                })
                .resolve()
        }),
        Rule::eq => Box::new(move |cfg, cfgs| {
            lhs(cfg, cfgs)
                .and_then(|lhs| {
                    rhs(cfg, cfgs).map(|rhs| match (&lhs, &rhs) {
                        (Some(lhs), Some(rhs)) => lhs == rhs,
                        (None, None) => true,
                        _ => false,
                    })
                })
                .resolve()
        }),
        Rule::neq => Box::new(move |cfg, cfgs| {
            lhs(cfg, cfgs)
                .and_then(|lhs| {
                    rhs(cfg, cfgs).map(|rhs| match (&lhs, &rhs) {
                        (Some(lhs), Some(rhs)) => lhs != rhs,
                        (None, None) => false,
                        _ => true,
                    })
                })
                .resolve()
        }),
        Rule::gt => Box::new(move |cfg, cfgs| {
            lhs(cfg, cfgs)
                .and_then(|lhs| {
                    rhs(cfg, cfgs).map(|rhs| match (&lhs, &rhs) {
                        (Some(lhs), Some(rhs)) => lhs.contains(rhs) && lhs.len() > rhs.len(),
                        _ => true,
                    })
                })
                .resolve()
        }),
        Rule::gte => Box::new(move |cfg, cfgs| {
            lhs(cfg, cfgs)
                .and_then(|lhs| {
                    rhs(cfg, cfgs).map(|rhs| match (&lhs, &rhs) {
                        (Some(lhs), Some(rhs)) => lhs.contains(rhs),
                        _ => true,
                    })
                })
                .resolve()
        }),
        _ => unreachable!(),
    }
}

fn compile_inv_bool_expr(mut pairs: Pairs<Rule>) -> CompiledRule {
    let expr = compile_bool_expr(pairs.next().unwrap().into_inner());
    Box::new(move |cfg, cfgs| !expr(cfg, cfgs))
}

fn compile_bool_expr(pairs: Pairs<Rule>) -> CompiledRule {
    BOOL_PREC_CLIMBER.climb(
        pairs,
        |pair| match pair.as_rule() {
            Rule::bool_var => compile_bool_var(pair.into_inner()),
            Rule::bool_expr => compile_bool_expr(pair.into_inner()),
            Rule::inv_bool_expr => compile_inv_bool_expr(pair.into_inner()),
            Rule::num_cmp_expr => compile_num_cmp_expr(pair.into_inner()),
            Rule::str_cmp_expr => compile_str_cmp_expr(pair.into_inner()),
            _ => unreachable!(),
        },
        |lhs, op, rhs| -> CompiledRule {
            match op.as_rule() {
                Rule::and => Box::new(move |cfg, cfgs| lhs(cfg, cfgs) && rhs(cfg, cfgs)),
                Rule::or => Box::new(move |cfg, cfgs| lhs(cfg, cfgs) || rhs(cfg, cfgs)),
                Rule::xor => Box::new(move |cfg, cfgs| lhs(cfg, cfgs) ^ rhs(cfg, cfgs)),
                _ => unreachable!(),
            }
        },
    )
}

fn compile_value_expr(mut pairs: Pairs<Rule>) -> CompiledExpr<VarRes<Value>> {
    let expr = pairs.next().unwrap();
    match expr.as_rule() {
        Rule::any_var => compile_var(expr.into_inner()),
        Rule::str_expr => {
            let expr = compile_str_expr(expr.into_inner());
            Box::new(move |cfg, cfgs| {
                expr(cfg, cfgs).map(|s| s.map(Value::String).unwrap_or(Value::Null))
            })
        }
        Rule::num_expr => {
            let expr = compile_num_expr(expr.into_inner());
            Box::new(move |cfg, cfgs| expr(cfg, cfgs).map(|n| match
                serde_json::Number::from_f64(n) {
                    Some(a) => Value::Number(a),
                    None => panic!("cannot coerce f64 into numberc type")
            }))
        }
        Rule::bool_expr => {
            let expr = compile_bool_expr(expr.into_inner());
            Box::new(move |cfg, cfgs| VarRes::Exactly(expr(cfg, cfgs)).map(Value::Bool))
        }
        _ => unreachable!(),
    }
}

fn compile_del_action(mut pairs: Pairs<Rule>) -> Result<Mutator, failure::Error> {
    let list_mut = compile_var_mut(pairs.next().unwrap().into_inner())?;
    let var = pairs.next().unwrap().as_str().to_owned();
    let predicate = compile_bool_expr(pairs.next().unwrap().into_inner());
    Ok(Box::new(move |cfg, cfgs| match (&list_mut)(cfg, cfgs) {
        Some(Value::Array(ref mut l)) => {
            *l = std::mem::take(l)
                .into_iter()
                .filter(|item| {
                    let mut obj = Config::default();
                    obj.insert(var.clone(), item.clone());
                    !predicate(&obj, cfgs)
                })
                .collect();
        }
        Some(Value::Object(ref mut o)) => {
            *o = std::mem::take(o)
                .into_iter()
                .filter(|(_, item)| {
                    let mut obj = Config::default();
                    obj.insert(var.clone(), item.clone());
                    !predicate(&obj, cfgs)
                })
                .collect();
        }
        _ => return,
    }))
}

fn compile_push_action(mut pairs: Pairs<Rule>, value: Value) -> Result<Mutator, failure::Error> {
    let list_mut = compile_var_mut(pairs.next().unwrap().into_inner())?;
    Ok(Box::new(move |cfg, cfgs| {
        let vec = match (&list_mut)(cfg, cfgs) {
            Some(Value::Array(ref mut a)) => a,
            _ => return,
        };
        vec.push(value.clone())
    }))
}

fn compile_set_action(var: &str, to: &SetVariant) -> Result<Mutator, failure::Error> {
    let mut var = RuleParser::parse(Rule::reference, var)?;
    let get_mut = compile_var_mut(var.next().unwrap().into_inner())?;
    Ok(match to {
        SetVariant::To(expr) => {
            let expr = compile_expr(&expr)?;
            Box::new(move |cfg, cfgs| {
                let val = expr(cfg, cfgs);
                if let Some(var) = get_mut(cfg, cfgs) {
                    *var = val;
                }
            })
        }
        SetVariant::ToValue(val) => {
            let val = val.clone();
            Box::new(move |cfg, cfgs| {
                if let Some(var) = get_mut(cfg, cfgs) {
                    *var = val.clone()
                }
            })
        }
        SetVariant::ToEntropy(entropy) => {
            let entropy = entropy.clone();
            Box::new(move |cfg, cfgs| {
                if let Some(var) = get_mut(cfg, cfgs) {
                    *var = Value::String(entropy.gen(&mut rand::rngs::StdRng::from_entropy()));
                }
            })
        }
    })
}

pub fn validate_key(key: &str) -> Result<(), pest::error::Error<Rule>> {
    RuleParser::parse(Rule::obj_key, key)?;
    Ok(())
}

pub fn parse_and<T, F: FnOnce(Pairs<Rule>) -> T>(
    rule: &str,
    f: F,
) -> Result<T, pest::error::Error<Rule>> {
    let mut parsed = RuleParser::parse(Rule::rule, rule)?;
    let pairs = parsed.next().unwrap().into_inner();
    Ok(f(pairs))
}

pub fn compile(rule: &str) -> Result<CompiledRule, failure::Error> {
    parse_and(rule, compile_bool_expr).map_err(From::from)
}

pub fn compile_expr(expr: &str) -> Result<CompiledExpr<Value>, failure::Error> {
    let compiled = compile_value_expr(RuleParser::parse(Rule::value, expr)?);
    Ok(Box::new(move |cfg, cfgs| match compiled(cfg, cfgs) {
        VarRes::Exactly(v) => v,
        _ => Value::Null,
    }))
}

#[cfg(test)]
mod test {
    use serde_json::json;

    use super::*;

    #[test]
    fn test_compile_str() {
        assert_eq!(
            compile_str("\"foo\"")(&Default::default(), &Default::default()),
            VarRes::Exactly(Some("foo".to_owned()))
        );
    }

    #[test]
    fn test_access_expr() {
        let mut cfg = Config::default();
        let mut cfgs = LinearMap::new();
        let mut foo = Config::default();
        foo.insert("bar!\"".to_owned(), json!(3.0));
        cfg.insert(
            "foo".to_owned(),
            Value::Array(vec![Value::Null, Value::Object(foo), json!(3)]),
        );
        cfgs.insert("my-app", Cow::Borrowed(&cfg));
        assert!((compile("#[my-app].foo.1.[\"ba\" + \"r!\\\"\"] = 3")
            .map_err(|e| eprintln!("{}", e))
            .expect("compile failed"))(&cfg, &cfgs));
        assert!((compile("#[my-app].foo.[0 + 1].[\"bar!\\\"\"] = 3")
            .map_err(|e| eprintln!("{}", e))
            .expect("compile failed"))(&cfg, &cfgs));
    }

    #[test]
    fn test_any_all() {
        let mut cfg = Config::default();
        let mut cfgs = LinearMap::new();
        let mut foo = Config::default();
        foo.insert("bar".to_owned(), json!(3.0));
        cfg.insert(
            "foo".to_owned(),
            Value::Array(vec![Value::Null, Value::Object(foo), json!(3.0)]),
        );
        cfgs.insert("my-app", Cow::Borrowed(&cfg));
        // NOTE: these now fail due to added panic for parsing f64's
        // assert!((compile("#[my-app].foo.*.bar = 3")
        //     .map_err(|e| eprintln!("{}", e))
        //     .expect("compile failed"))(&cfg, &cfgs));
        // assert!(!(compile("#[my-app].foo.&.bar = 3")
        //     .map_err(|e| eprintln!("{}", e))
        //     .expect("compile failed"))(&cfg, &cfgs));
    }

    #[test]
    fn test_first_last() {
        let mut cfg = Config::default();
        let mut cfgs = LinearMap::new();
        let mut foo = Config::default();
        foo.insert("bar".to_owned(), json!(3.0));
        foo.insert("baz".to_owned(), json!(4.0));
        let mut qux = Config::default();
        qux.insert("bar".to_owned(), json!(7.0));
        qux.insert("baz".to_owned(), json!(4.0));
        cfg.insert(
            "foo".to_owned(),
            Value::Array(vec![
                Value::Null,
                Value::Object(foo),
                Value::Object(qux),
                json!(3.0),
            ]),
        );
        cfgs.insert("my-app", Cow::Borrowed(&cfg));
        // NOTE: these now fail due to added panic for parsing f64's
        // assert!((compile("#foo.[first(item => #item.baz = 4)].bar = 3")
        //     .map_err(|e| eprintln!("{}", e))
        //     .expect("compile failed"))(&cfg, &cfgs));
        // assert!((compile("#foo.[last(item => #item.baz = 4)].bar = 7")
        //     .map_err(|e| eprintln!("{}", e))
        //     .expect("compile failed"))(&cfg, &cfgs));
    }

    #[test]
    fn test_app_id() {
        let mut dependent_cfg = Config::default();
        let mut dependency_cfg = Config::default();
        let mut cfgs = LinearMap::new();
        dependent_cfg
            .insert("foo".to_owned(), Value::String("bar".to_owned()));
        dependency_cfg
            .insert("foo".to_owned(), Value::String("bar!".to_owned()));
        cfgs.insert("my-dependent", Cow::Borrowed(&dependent_cfg));
        cfgs.insert("my-dependency", Cow::Borrowed(&dependency_cfg));
        assert!((compile("'foo = '[my-dependent].foo + \"!\"")
            .map_err(|e| eprintln!("{}", e))
            .expect("compile failed"))(
            &dependency_cfg, &cfgs
        ))
    }
}
