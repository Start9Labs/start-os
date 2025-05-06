#[cfg(test)]
mod tests {
    use super::super::*; // Access items from lib.rs
    use uciedit_macros::UciSection; // Import the derive macro

    // Helper to normalize whitespace for comparisons
    fn normalize(s: String) -> String {
        s.replace("\t", "    ")
    }

    #[derive(UciSection, PartialEq, Eq, Debug, Default)]
    struct Bar {
        always: i32,
        yes: Option<i32>,
        no: Option<i32>,
        many: Vec<i32>,
        #[uci(default)] // Test explicit default for non-Option/Vec
        num_default: i32,
        #[uci(default_value = "Some(\"default_val\".to_string())")]
        opt_str_default_val: Option<String>,
        #[uci(default_value = "vec![\"a\".to_string(), \"b\".to_string()]")]
        vec_str_default_val: Vec<String>,
    }

    #[test]
    fn test_read_section() {
        let original = r"
config bar
    option always 0
    option yes 1
    list many 2
    list many 3
    list many 4
";

        let expected = Bar {
            always: 0,
            yes: Some(1),
            no: None,
            many: vec![2, 3, 4],
            num_default: 0, // Default for i32
            opt_str_default_val: Some("default_val".to_string()),
            vec_str_default_val: vec!["a".to_string(), "b".to_string()],
        };

        let parsed: Bar = parse_config_string(original, |mut ctx| {
            assert!(ctx.step());
            ctx.get()
        })
        .unwrap();

        println!(
            "===Original==={original}===Parsed===\n{parsed:#?}\n===Expected===\n{expected:#?}\n====="
        );
        assert_eq!(parsed, expected);
    }

    #[test]
    fn test_append_section() {
        let original = r"
config foo
    option hello world
    # a comment here
";

        let expected = r"
config foo
    option hello world
    # a comment here

config bar appended
    option always 0
    option yes 1
    list many 2
    list many 3
    list many 4
    option num_default 0
    option opt_str_default_val default_val
    list vec_str_default_val a
    list vec_str_default_val b
";

        let edited = rewrite_config_string(original.to_string(), |mut ctx| {
            ctx.push(
                Bar {
                    always: 0,
                    yes: Some(1),
                    no: None,
                    many: vec![2, 3, 4],
                    num_default: 0,
                    opt_str_default_val: Some("default_val".to_string()),
                    vec_str_default_val: vec!["a".to_string(), "b".to_string()],
                },
                Some("appended"),
            )
        })
        .unwrap();

        println!("===Original==={original}===Edited===\n{edited}\n===Expected===\n{expected}\n=====");
        assert_eq!(normalize(edited), normalize(expected.to_string()));
    }

    #[derive(UciSection, PartialEq, Eq, Debug)]
    struct EditBar {
        always: i32,
        yes: Option<i32>,
        no: Option<i32>,
        many: Vec<i32>,
        few: Vec<i32>,
    }

    #[test]
    fn test_edit_section() {
        let original = r"
# top comment
config bar named
    # always comment
    option always 0

    # no comment
    option no 1

    # many comment
    list many 2

    # few comment
    list few 3
    list few 4
    list few 5

    # ignored comment
    option ignored 6

# bottom comment
config other
    option something here
";

        let expected = r"
# top comment
config bar named
    # always comment
    option always 0

    # no comment

    # many comment
    list many 2

    # few comment
    list few 5

    # ignored comment
    option ignored 6
    option yes 1
    list many 3
    list many 4

# bottom comment
config other
    option something here
";

        let edited = rewrite_config_string(original.to_string(), |mut ctx| {
            while ctx.step() {
                if ctx.ty() == "bar" { // Only edit sections of type "bar"
                    let _ = ctx.set(EditBar {
                        always: 0,
                        yes: Some(1),
                        no: None,
                        many: vec![2, 3, 4],
                        few: vec![5],
                    });
                }
            }
            Ok::<_, Error>(())
        })
        .unwrap();

        println!("===Original==={original}===Edited===\n{edited}\n===Expected===\n{expected}\n=====");
        assert_eq!(normalize(edited), normalize(expected.to_string()));
    }

    #[test]
    fn test_remove_sections() {
        let original = r"
# section 1
config retain
    option foo bar
    # comment 1

# section 2
config remove
    option foo bar
    # comment 2

# section 3
config remove
    option foo bar
    # comment 3

# section 4
config retain
    option foo bar
    # comment 4

# section 5
config remove
config retain
";

        let expected = r"
# section 1
config retain
    option foo bar
    # comment 1



# section 4
config retain
    option foo bar
    # comment 4

config retain
";

        let edited = rewrite_config_string(original.to_string(), |mut ctx| {
            while ctx.step() {
                ctx.set_retain(ctx.ty() == "retain");
            }
            Ok::<_, Error>(())
        })
        .unwrap();

        println!("===Original==={original}===Edited===\n{edited}\n===Expected===\n{expected}\n=====");
        assert_eq!(normalize(edited), normalize(expected.to_string()));
    }

    #[test]
    fn test_line_parse_errors() {
        assert!(matches!(
            Line::parse("config foo 'invalid name", 0),
            Err(Error::BadSectionName { .. })
        ));
        assert!(matches!(
            Line::parse("config ", 0),
            Err(Error::BadSectionType { .. })
        ));
        assert!(matches!(
            Line::parse("\toption name_only", 0),
            Err(Error::BadOption { .. })
        ));
        assert!(matches!(
            Line::parse("\tlist name_only", 0),
            Err(Error::BadList { .. })
        ));
        assert!(matches!(
            Line::parse("unknown_keyword foo bar", 0),
            Err(Error::UnknownKeyword { .. })
        ));
    }

    #[test]
    fn test_token_functionality() {
        let arena = Arena::new();
        let token_w = Token::from_str("word", &arena);
        assert_eq!(token_w.as_str(), "word");
        assert_eq!(format!("{}", token_w), "word");

        let token_q_needed = Token::from_string("hello world".to_string(), &arena);
        assert_eq!(token_q_needed.as_str(), "hello world"); // as_str unescapes if from_string quoted
        assert_eq!(format!("{}", token_q_needed), "\"hello world\"");


        let token_sq_manual = Token::Sq(SingleQuoted { inner: "sq word" });
        assert_eq!(token_sq_manual.as_str(), "sq word");
        assert_eq!(format!("{}", token_sq_manual), "'sq word'");

        assert_eq!(Token::from_bool(true).as_str(), "1");
        assert_eq!(Token::from_bool(false).as_str(), "0");

        assert!(Token::from_str("true", &arena).parse_bool(0).unwrap());
        assert!(!Token::from_str("0", &arena).parse_bool(0).unwrap());
        assert!(matches!(
            Token::from_str("maybe", &arena).parse_bool(0),
            Err(Error::ValueBoolean { .. })
        ));

        assert_eq!(
            Token::from_str("123", &arena)
                .parse_fromstr::<i32>(0)
                .unwrap(),
            123
        );
        assert!(matches!(
            Token::from_str("abc", &arena).parse_fromstr::<i32>(0),
            Err(Error::ValueDyn { .. })
        ));
    }

    #[derive(UciSection, PartialEq, Eq, Debug)]
    #[uci(ty = "custom_type")]
    struct RenamedTypeSection {
        #[uci(rename = "old_name")]
        new_name: String,
        val: bool,
    }

    #[test]
    fn test_uci_section_rename_and_type_override() {
        assert_eq!(RenamedTypeSection::TY, "custom_type");
        let original = r"
config custom_type 'my_section'
    option old_name 'test_value'
    option val true
";
        let expected_obj = RenamedTypeSection {
            new_name: "test_value".to_string(),
            val: true,
        };

        let parsed: RenamedTypeSection = parse_config_string(original, |mut ctx| {
            assert!(ctx.step());
            assert_eq!(ctx.ty(), "custom_type");
            assert_eq!(ctx.name().unwrap(), "my_section");
            ctx.get()
        })
        .unwrap();
        assert_eq!(parsed, expected_obj);

        let written = rewrite_config_string("".to_string(), |mut ctx| {
            ctx.push(expected_obj, Some("my_section"))
        })
        .unwrap();
        let expected_written = r"
config custom_type my_section
    option old_name test_value
    option val 1
";
        assert_eq!(normalize(written), normalize(expected_written.to_string()));
    }

    #[derive(UciSection, PartialEq, Eq, Debug)]
    struct DefaultDemo {
        #[uci(default)]
        my_int: i32, // uses i32::default()
        my_opt_int: Option<i32>, // uses None
        my_vec_int: Vec<i32>,    // uses vec![]
        #[uci(default_value = r#"Some("hello".to_string())"#)]
        my_opt_str: Option<String>,
        #[uci(default_value = r#"vec![10, 20]"#)]
        my_default_vec: Vec<i32>,
    }

    #[test]
    fn test_uci_section_defaults() {
        let original_empty_section = "config defaultdemo 'test'";
        let parsed: DefaultDemo = parse_config_string(original_empty_section, |mut ctx| {
            assert!(ctx.step());
            ctx.get()
        })
        .unwrap();

        assert_eq!(parsed.my_int, 0);
        assert_eq!(parsed.my_opt_int, None);
        assert_eq!(parsed.my_vec_int, Vec::<i32>::new());
        assert_eq!(parsed.my_opt_str, Some("hello".to_string()));
        assert_eq!(parsed.my_default_vec, vec![10, 20]);

        let original_partial = r"
config defaultdemo 'test'
    option my_int 5
    list my_vec_int 1
";
        let parsed_partial: DefaultDemo = parse_config_string(original_partial, |mut ctx| {
            assert!(ctx.step());
            ctx.get()
        })
        .unwrap();
        assert_eq!(parsed_partial.my_int, 5);
        assert_eq!(parsed_partial.my_opt_int, None);
        assert_eq!(parsed_partial.my_vec_int, vec![1]);
        assert_eq!(parsed_partial.my_opt_str, Some("hello".to_string()));
        assert_eq!(parsed_partial.my_default_vec, vec![10, 20]);
    }

    #[derive(UciSection, PartialEq, Eq, Debug)]
    struct BoolFieldTest {
        is_enabled: bool,
        is_active: bool,
        is_present: Option<bool>,
    }

    #[test]
    fn test_uci_section_bool_field() {
        let original = r"
config boolfieldtest
    option is_enabled yes
    option is_active 0
    option is_present true
";
        let parsed: BoolFieldTest = parse_config_string(original, |mut ctx| {
            assert!(ctx.step());
            ctx.get()
        })
        .unwrap();
        assert_eq!(parsed.is_enabled, true);
        assert_eq!(parsed.is_active, false);
        assert_eq!(parsed.is_present, Some(true));

        let written = rewrite_config_string("".to_string(), |mut ctx| {
            ctx.push(parsed, None)
        })
        .unwrap();
        let expected_written = r"
config boolfieldtest
    option is_enabled 1
    option is_active 0
    option is_present 1
";
        assert_eq!(normalize(written), normalize(expected_written.to_string()));
    }

    #[derive(UciSection, Debug)]
    struct RequiredField {
        must_exist: String,
    }

    #[test]
    fn test_uci_section_missing_required_field() {
        let original = "config requiredfield 'test'"; // must_exist is missing
        let result: Result<RequiredField, Error> = parse_config_string(original, |mut ctx| {
            assert!(ctx.step());
            ctx.get()
        });
        assert!(matches!(
            result,
            Err(Error::MissingOption { missing, .. }) if missing == "must_exist"
        ));
    }

    #[test]
    fn test_sections_mut_push_to_empty_config() {
        let expected = r"
config bar
    option always 1
    option num_default 0
";
        let edited = rewrite_config_string("".to_string(), |mut ctx| {
            ctx.push(
                Bar {
                    always: 1,
                    yes: None,
                    no: None,
                    many: vec![],
                    num_default: 0,
                    opt_str_default_val: None, // Test with None for default_value field
                    vec_str_default_val: vec![], // Test with empty for default_value field
                },
                None,
            )
        })
        .unwrap();
        assert_eq!(normalize(edited), normalize(expected.to_string()));
    }
    
    #[derive(UciSection, Debug, PartialEq)]
    struct AnotherType {
        value: String,
    }

    #[test]
    fn test_sections_mut_set_non_matching_type() {
        let original = r"
config bar 'mybar'
    option always 123
";
        let result = rewrite_config_string(original.to_string(), |mut ctx| {
            assert!(ctx.step()); // Step to the 'bar' section
            assert_eq!(ctx.ty(), "bar");
            // Try to set it with a struct of a different UciSection type
            ctx.set(AnotherType { value: "test".to_string() })
        });

        assert!(matches!(
            result,
            Err(Error::ExpectedSectionType { expected, found, .. })
            if expected == AnotherType::TY && found == Bar::TY
        ));
    }

    #[test]
    fn test_parse_empty_config() {
        let original = "";
        let result: Result<(), Error> = parse_config_string(original, |mut ctx| {
            assert!(!ctx.step()); // No sections
            Ok(())
        });
        assert!(result.is_ok());

        let edited = rewrite_config_string(original.to_string(), |_ctx| Ok(())).unwrap();
        assert_eq!(edited, "");
    }

    #[test]
    fn test_parse_config_with_only_comments_and_empty_lines() {
        let original = r"
# This is a comment

# Another comment
";
        let result: Result<(), Error> = parse_config_string(original, |mut ctx| {
            assert!(!ctx.step()); // No sections
            Ok(())
        });
        assert!(result.is_ok());

        let edited = rewrite_config_string(original.to_string(), |_ctx| Ok(())).unwrap();
        assert_eq!(normalize(edited), normalize(original.to_string()));
    }

    #[derive(UciSection, PartialEq, Eq, Debug, Default)]
    struct ComplexSection {
        name: String,
        value: Option<i32>,
        items: Vec<String>,
        enabled: bool,
        #[uci(rename = "old_flag")]
        new_flag: Option<bool>,
        #[uci(default_value = r#""default string""#.to_string())]
        default_str: String,
    }

    #[test]
    fn test_round_trip_complex_config() {
        let section1 = ComplexSection {
            name: "Section One".to_string(),
            value: Some(42),
            items: vec!["item1".to_string(), "item2 with spaces".to_string()],
            enabled: true,
            new_flag: Some(false),
            default_str: "custom".to_string(),
        };
        let section2 = ComplexSection {
            name: "Section Two".to_string(),
            value: None,
            items: vec![],
            enabled: false,
            new_flag: None,
            default_str: "default string".to_string(), // Will use default_value
        };

        let original_config_str = rewrite_config_string("".to_string(), |mut ctx| {
            ctx.push(section1, Some("first"))?;
            ctx.push(section2, Some("second"))
        })
        .unwrap();
        
        println!("===Generated Config===\n{}", original_config_str);

        let (parsed_s1, parsed_s2): (ComplexSection, ComplexSection) =
            parse_config_string(&original_config_str, |mut ctx| {
                assert!(ctx.step());
                let s1 = ctx.get::<ComplexSection>()?;
                assert!(ctx.step());
                let s2 = ctx.get::<ComplexSection>()?;
                assert!(!ctx.step());
                Ok((s1, s2))
            })
            .unwrap();
        
        // Re-create original objects to compare, as default_str might differ if not set
         let expected_section1 = ComplexSection {
            name: "Section One".to_string(),
            value: Some(42),
            items: vec!["item1".to_string(), "item2 with spaces".to_string()],
            enabled: true,
            new_flag: Some(false),
            default_str: "custom".to_string(),
        };
        let expected_section2 = ComplexSection {
            name: "Section Two".to_string(),
            value: None,
            items: vec![],
            enabled: false,
            new_flag: None,
            default_str: "default string".to_string(),
        };

        assert_eq!(parsed_s1, expected_section1);
        assert_eq!(parsed_s2, expected_section2);

        // Also check the string output for consistency (order might vary for new fields)
        // This is a bit more fragile but good for a smoke test.
        // The order of options within a section is preserved for existing options,
        // but new options (from fields not in original config) are appended.
        let expected_written_config = r"
config complexsection first
    option default_str custom
    option old_flag 0
    option enabled 1
    list items item1
    list items "item2 with spaces"
    option value 42
    option name "Section One"

config complexsection second
    option default_str "default string"
    option enabled 0
    option name "Section Two"
";
        assert_eq!(normalize(original_config_str), normalize(expected_written_config.to_string()));
    }
    
    #[test]
    fn test_sections_mut_reset_clears_removal_flags() {
        let original = r"
config item one
config item two
config item three
";
        let expected_after_reset_no_op = original.to_string();

        let edited = rewrite_config_string(original.to_string(), |mut ctx| {
            assert!(ctx.step()); // on 'one'
            ctx.remove(); // Mark 'one' for removal

            assert!(ctx.step()); // on 'two'
            // Don't remove 'two'

            ctx.reset(); // This should clear the removal flag for 'one' and process pending removals (none here as step wasn't called after remove)

            // After reset, iterate again to ensure 'one' is still there
            let mut count = 0;
            while ctx.step() {
                count += 1;
                if ctx.name().unwrap() == "one" {
                    // If 'one' was removed, this branch wouldn't be hit, or ty/name would panic.
                }
            }
            assert_eq!(count, 3, "All sections should be present after reset");
            Ok::<_, Error>(())
        })
        .unwrap();
        assert_eq!(normalize(edited), normalize(expected_after_reset_no_op));

        // Test where removal actually happens before reset
        let original_for_removal = r"
config item one
config item two
config item three
";
        let expected_after_partial_removal_then_reset = r"
config item two
config item three
";
         let edited_for_removal = rewrite_config_string(original_for_removal.to_string(), |mut ctx| {
            assert!(ctx.step()); // on 'one'
            ctx.remove(); // Mark 'one' for removal
            
            // Crucially, step again to process the removal of 'one'
            assert!(ctx.step()); // on 'two', 'one' is now gone from lines

            // Now reset. 'one' is already removed. 'two' and 'three' should remain.
            ctx.reset(); 

            let mut names = Vec::new();
            while ctx.step() {
                names.push(ctx.name().unwrap().to_string());
            }
            assert_eq!(names, vec!["two", "three"], "Only 'two' and 'three' should remain");
            Ok::<_, Error>(())
        })
        .unwrap();
        assert_eq!(normalize(edited_for_removal), normalize(expected_after_partial_removal_then_reset.to_string()));
    }
}
