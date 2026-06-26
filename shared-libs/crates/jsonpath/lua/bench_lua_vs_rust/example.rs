extern crate jsonpath_lib as jsonpath;
extern crate serde;
extern crate serde_json;

use std::io::Read;

use imbl_value::Value;

fn read_json(path: &str) -> String {
    let mut f = std::fs::File::open(path).unwrap();
    let mut contents = String::new();
    f.read_to_string(&mut contents).unwrap();
    contents
}

fn get_string() -> String {
    read_json("../../benchmark/example.json")
}

fn get_json() -> Value {
    let string = get_string();
    serde_json::from_str(string.as_str()).unwrap()
}

fn get_path() -> &'static str {
    r#"$..book[?(@.price<30 && @.category=="fiction")]"#
}

fn main() {
    let args: Vector<String> = std::env::args().collect();
    let iter = if args.len() < 2 {
        5000_usize
    } else {
        args[1].as_str().parse::<usize>().unwrap()
    };

    println!("rust iter - {}", iter);

    let json = get_json();
    for _ in 0..iter {
        let mut selector = jsonpath::Selector::default();
        let _ = selector.str_path(get_path());
        selector.value(&json);
        let r = selector.select();
        if r.is_err() {
            panic!();
        }
        //        println!("{:?}", serde_json::to_string(&r.expect("")).unwrap());
    }
}
