use rust_i18n::t;

pub fn renamed(old: &str, new: &str) -> ! {
    eprintln!(
        "{}",
        t!("bins.deprecated.renamed", old = old, new = new)
    );
    std::process::exit(1)
}

pub fn removed(name: &str) -> ! {
    eprintln!("{}", t!("bins.deprecated.removed", name = name));
    std::process::exit(1)
}
