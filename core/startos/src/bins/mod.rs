use std::collections::VecDeque;
use std::ffi::OsString;
use std::path::Path;

#[cfg(feature = "avahi-alias")]
pub mod avahi_alias;
pub mod deprecated;
#[cfg(feature = "cli")]
pub mod start_cli;
#[cfg(feature = "daemon")]
pub mod start_init;
#[cfg(feature = "daemon")]
pub mod startd;

fn select_executable(name: &str) -> Option<fn(VecDeque<OsString>)> {
    match name {
        #[cfg(feature = "avahi-alias")]
        "avahi-alias" => Some(avahi_alias::main),
        #[cfg(feature = "cli")]
        "start-cli" => Some(start_cli::main),
        #[cfg(feature = "daemon")]
        "startd" => Some(startd::main),
        "embassy-cli" => Some(|_| deprecated::renamed("embassy-cli", "start-cli")),
        "embassy-sdk" => Some(|_| deprecated::renamed("embassy-sdk", "start-sdk")),
        "embassyd" => Some(|_| deprecated::renamed("embassyd", "startd")),
        "embassy-init" => Some(|_| deprecated::removed("embassy-init")),
        _ => None,
    }
}

pub fn startbox() {
    let args = std::env::args_os().collect::<VecDeque<_>>();
    for _ in 0..2 {
        if let Some(s) = args.pop_front() {
            if let Some(x) = Path::new(&*s)
                .file_name()
                .and_then(|s| s.to_str())
                .and_then(|s| select_executable(&s))
            {
                args.push_front(s);
                return x(args);
            }
        }
    }
    let args = std::env::args().collect::<VecDeque<_>>();
    eprintln!(
        "unknown executable: {}",
        args.get(1)
            .or_else(|| args.get(0))
            .map(|s| s.as_str())
            .unwrap_or("N/A")
    );
    std::process::exit(1);
}
