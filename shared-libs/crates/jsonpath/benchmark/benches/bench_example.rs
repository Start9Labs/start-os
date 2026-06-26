#![feature(test)]
extern crate bencher;
extern crate jsonpath_lib as jsonpath;
extern crate serde;
extern crate serde_json;
extern crate test;

use std::io::Read;

use serde_json::Value;

use self::test::Bencher;

fn read_json(path: &str) -> String {
    let mut f = std::fs::File::open(path).unwrap();
    let mut contents = String::new();
    f.read_to_string(&mut contents).unwrap();
    contents
}

fn get_string() -> String {
    read_json("./example.json")
}

fn get_json() -> Value {
    let string = get_string();
    serde_json::from_str(string.as_str()).unwrap()
}

fn get_path(i: usize) -> &'static str {
    let paths = vec![
        "$.store.book[*].author",   //0
        "$..author",    //1
        "$.store.*",    //2
        "$.store..price",   //3
        "$..book[2]",   //4
        "$..book[-2]",  //5
        "$..book[0,1]", //6
        "$..book[:2]",  //7
        "$..book[1:2]", //8
        "$..book[-2:]", //9
        "$..book[2:]",  //10
        "$..book[?(@.isbn)]",   //11
        "$.store.book[?(@.price == 10)]",    //12
        "$..*", //13
        "$..book[ ?( (@.price < 13 || $.store.bicycle.price < @.price) && @.price <=10 ) ]", //14
        "$.store.book[?( (@.price < 10 || @.price > 10) && @.price > 10 )]"
    ];
    paths[i]
}

fn _selector(b: &mut Bencher, index: usize) {
    let json = get_json();
    b.iter(move || {
        for _ in 1..100 {
            let parser = jsonpath::PathParser::compile(get_path(index)).unwrap();
            let mut selector = jsonpath::JsonSelector::new(parser);
            selector.value(&json);
            let r = selector.select();
            if r.is_err() {
                panic!()
            }
        }
    });
}

macro_rules! selector {
    ($name:ident, $i:expr) => {
        #[bench]
        fn $name(b: &mut Bencher) { _selector(b, $i); }
    };
}

selector!(example0_1, 0);
selector!(example1_1, 1);
selector!(example2_1, 2);
selector!(example3_1, 3);
selector!(example4_1, 4);
selector!(example5_1, 5);
selector!(example6_1, 6);
selector!(example7_1, 7);
selector!(example8_1, 8);
selector!(example9_1, 9);
selector!(example_10_1, 10);
selector!(example_11_1, 11);
selector!(example_12_1, 12);
selector!(example_13_1, 13);
selector!(example_14_1, 14);
selector!(example_15_1, 15);