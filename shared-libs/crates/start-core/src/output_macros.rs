macro_rules! print {
    ($($args:tt)*) => {
        rpc_toolkit::output::print(format_args!($($args)*))
    };
}

macro_rules! println {
    () => { rpc_toolkit::output::println(format_args!("")) };
    ($($args:tt)*) => {
        rpc_toolkit::output::println(format_args!($($args)*))
    };
}

macro_rules! eprint {
    ($($args:tt)*) => {
        rpc_toolkit::output::eprint(format_args!($($args)*))
    };
}

macro_rules! eprintln {
    () => { eprint!("\n") };
    ($($args:tt)*) => {
        eprint!("{}\n", format_args!($($args)*))
    };
}
