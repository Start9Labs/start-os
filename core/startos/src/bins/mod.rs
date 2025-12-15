use std::collections::{BTreeMap, VecDeque};
use std::ffi::OsString;
use std::path::Path;

pub mod container_cli;
pub mod deprecated;
pub mod registry;
pub mod start_cli;
pub mod start_init;
pub mod startd;
pub mod tunnel;

#[derive(Default)]
pub struct MultiExecutable(BTreeMap<&'static str, fn(VecDeque<OsString>)>);
impl MultiExecutable {
    pub fn enable_startd(&mut self) -> &mut Self {
        self.0.insert("startd", startd::main);
        self.0
            .insert("embassyd", |_| deprecated::renamed("embassyd", "startd"));
        self.0
            .insert("embassy-init", |_| deprecated::removed("embassy-init"));
        self
    }
    pub fn enable_start_cli(&mut self) -> &mut Self {
        self.0.insert("start-cli", start_cli::main);
        self.0.insert("embassy-cli", |_| {
            deprecated::renamed("embassy-cli", "start-cli")
        });
        self.0
            .insert("embassy-sdk", |_| deprecated::removed("embassy-sdk"));
        self
    }
    pub fn enable_start_container(&mut self) -> &mut Self {
        self.0.insert("start-container", container_cli::main);
        self
    }
    pub fn enable_start_registryd(&mut self) -> &mut Self {
        self.0.insert("start-registryd", registry::main);
        self
    }
    pub fn enable_start_registry(&mut self) -> &mut Self {
        self.0.insert("start-registry", registry::cli);
        self
    }
    pub fn enable_start_tunneld(&mut self) -> &mut Self {
        self.0.insert("start-tunneld", tunnel::main);
        self
    }
    pub fn enable_start_tunnel(&mut self) -> &mut Self {
        self.0.insert("start-tunnel", tunnel::cli);
        self
    }

    fn select_executable(&self, name: &str) -> Option<fn(VecDeque<OsString>)> {
        self.0.get(&name).copied()
    }

    pub fn execute(&self) {
        let mut args = std::env::args_os().collect::<VecDeque<_>>();
        for _ in 0..2 {
            if let Some(s) = args.pop_front() {
                if let Some(name) = Path::new(&*s).file_name().and_then(|s| s.to_str()) {
                    if name == "--contents" {
                        for name in self.0.keys() {
                            println!("{name}");
                        }
                    }
                    if let Some(x) = self.select_executable(&name) {
                        args.push_front(s);
                        return x(args);
                    }
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
}
