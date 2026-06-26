use proptest::prelude::*;
use proptest_derive::Arbitrary;
use yasi::InternedString;

use crate::Value;

impl Arbitrary for Value {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;
    fn arbitrary() -> Self::Strategy {
        value_strategy()
    }
    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        value_strategy()
    }
}

pub fn value_strategy() -> BoxedStrategy<Value> {
    let leaf = proptest::prop_oneof![
        Just(Value::Null),
        any::<bool>().prop_map(Value::Bool),
        number_strategy(),
        ".*".prop_map(|s| Value::from(s)),
    ];
    leaf.prop_recursive(8, 256, 10, |inner| {
        prop_oneof![array_strategy(inner.clone()), object_strategy(inner)]
    })
    .boxed()
}

pub fn number_strategy() -> BoxedStrategy<Value> {
    #[derive(Debug, Arbitrary)]
    enum Number {
        PosInt(u64),
        NegInt(i64),
        Float(f64),
    }
    Number::arbitrary()
        .prop_map::<serde_json::Number, _>(|n| match n {
            Number::PosInt(a) => a.into(),
            Number::NegInt(a) => a.into(),
            Number::Float(a) => {
                serde_json::Number::from_f64(a).unwrap_or(serde_json::Number::from(0))
            }
        })
        .prop_map(Value::Number)
        .boxed()
}

pub fn array_strategy(inner: impl Strategy<Value = Value> + 'static) -> BoxedStrategy<Value> {
    imbl::proptest::vector(inner, 0..10)
        .prop_map(Value::Array)
        .boxed()
}

pub fn object_strategy(inner: impl Strategy<Value = Value> + 'static) -> BoxedStrategy<Value> {
    prop::collection::hash_map(".*", inner, 0..10)
        .prop_map(|map| {
            map.into_iter()
                .map(|(k, v)| (InternedString::intern(k), v))
                .collect()
        })
        .prop_map(Value::Object)
        .boxed()
}
