use std::collections::VecDeque;
use std::ffi::OsString;
use std::path::Path;

#[cfg(feature = "cli-container")]
pub mod container_cli;
pub mod deprecated;
#[cfg(any(feature = "registry", feature = "cli-registry"))]
pub mod registry;
#[cfg(feature = "cli")]
pub mod start_cli;
#[cfg(feature = "startd")]
pub mod start_init;
#[cfg(feature = "startd")]
pub mod startd;
#[cfg(any(feature = "tunnel", feature = "cli-tunnel"))]
pub mod tunnel;

fn select_executable(name: &str) -> Option<fn(VecDeque<OsString>)> {
    match name {
        #[cfg(feature = "startd")]
        "startd" => Some(startd::main),
        #[cfg(feature = "startd")]
        "embassyd" => Some(|_| deprecated::renamed("embassyd", "startd")),
        #[cfg(feature = "startd")]
        "embassy-init" => Some(|_| deprecated::removed("embassy-init")),

        #[cfg(feature = "cli-startd")]
        "start-cli" => Some(start_cli::main),
        #[cfg(feature = "cli-startd")]
        "embassy-cli" => Some(|_| deprecated::renamed("embassy-cli", "start-cli")),
        #[cfg(feature = "cli-startd")]
        "embassy-sdk" => Some(|_| deprecated::removed("embassy-sdk")),

        #[cfg(feature = "cli-container")]
        "start-container" => Some(container_cli::main),

        #[cfg(feature = "registry")]
        "start-registryd" => Some(registry::main),
        #[cfg(feature = "cli-registry")]
        "start-registry" => Some(registry::cli),

        #[cfg(feature = "tunnel")]
        "start-tunneld" => Some(tunnel::main),
        #[cfg(feature = "cli-tunnel")]
        "start-tunnel" => Some(tunnel::cli),

        "contents" => Some(|_| {
            #[cfg(feature = "startd")]
            println!("startd");
            #[cfg(feature = "cli-startd")]
            println!("start-cli");
            #[cfg(feature = "cli-container")]
            println!("start-container");
            #[cfg(feature = "registry")]
            println!("start-registryd");
            #[cfg(feature = "cli-registry")]
            println!("start-registry");
            #[cfg(feature = "tunnel")]
            println!("start-tunneld");
            #[cfg(feature = "cli-tunnel")]
            println!("start-tunnel");
        }),
        _ => None,
    }
}

pub fn startbox() {
    let mut args = std::env::args_os().collect::<VecDeque<_>>();
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
