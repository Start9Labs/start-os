pub fn renamed(old: &str, new: &str) -> ! {
    eprintln!("{old} has been renamed to {new}");
    std::process::exit(1)
}

pub fn removed(name: &str) -> ! {
    eprintln!("{name} has been removed");
    std::process::exit(1)
}
