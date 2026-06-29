use ts_rs::TS;

use crate::InternedString;

impl TS for InternedString {
    type WithoutGenerics = Self;
    fn name() -> String {
        "string".to_owned()
    }
    fn inline() -> String {
        Self::name()
    }
    fn decl() -> String {
        panic!("InternedString cannot be declared")
    }
    fn decl_concrete() -> String {
        Self::decl()
    }
    fn inline_flattened() -> String {
        panic!("InternedString cannot be flattened")
    }
}

#[cfg(test)]
mod test {
    use crate::InternedString;
    use ts_rs::TS;

    #[derive(TS)]
    struct HasString {
        s: InternedString,
    }

    #[test]
    fn test_export() {
        HasString::export_to_string().unwrap();
    }
}
