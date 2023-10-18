use std::path::Path;

#[cfg(feature = "avahi-alias")]
pub mod avahi_alias;
pub mod deprecated;
#[cfg(feature = "cli")]
pub mod start_cli;
#[cfg(feature = "js_engine")]
pub mod start_deno;
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
        #[cfg(feature = "js_engine")]
        "start-deno" => Some(start_deno::main),
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
    if let Some(x) = args
        .get(0)
        .and_then(|s| Path::new(&*s).file_name())
        .and_then(|s| s.to_str())
        .and_then(|s| select_executable(&s))
    {
        x()
    } else if let Some(x) = args.get(1).and_then(|s| select_executable(&s)) {
        x()
    } else {
        eprintln!(
            "unknown executable: {}",
            args.get(0)
                .filter(|x| &**x != "startbox")
                .or_else(|| args.get(1))
                .map(|s| s.as_str())
                .unwrap_or("N/A")
        );
        std::process::exit(1);
    }
}
