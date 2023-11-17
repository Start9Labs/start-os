use std::path::Path;

#[cfg(feature = "avahi-alias")]
pub mod avahi_alias;
pub mod deprecated;
#[cfg(feature = "cli")]
pub mod start_cli;
#[cfg(feature = "daemon")]
pub mod start_init;
#[cfg(feature = "sdk")]
pub mod start_sdk;
#[cfg(feature = "daemon")]
pub mod startd;

fn select_executable(name: &str) -> Option<fn()> {
    match name {
        #[cfg(feature = "avahi-alias")]
        "avahi-alias" => Some(avahi_alias::main),
        #[cfg(feature = "cli")]
        "start-cli" => Some(start_cli::main),
        #[cfg(feature = "sdk")]
        "start-sdk" => Some(start_sdk::main),
        #[cfg(feature = "daemon")]
        "startd" => Some(startd::main),
        "embassy-cli" => Some(|| deprecated::renamed("embassy-cli", "start-cli")),
        "embassy-sdk" => Some(|| deprecated::renamed("embassy-sdk", "start-sdk")),
        "embassyd" => Some(|| deprecated::renamed("embassyd", "startd")),
        "embassy-init" => Some(|| deprecated::removed("embassy-init")),
        _ => None,
    }
}

pub fn startbox() {
    let args = std::env::args().take(2).collect::<Vec<_>>();
    let executable = args
        .get(0)
        .and_then(|s| Path::new(&*s).file_name())
        .and_then(|s| s.to_str());
    if let Some(x) = executable.and_then(|s| select_executable(&s)) {
        x()
    } else {
        eprintln!("unknown executable: {}", executable.unwrap_or("N/A"));
        std::process::exit(1);
    }
}
