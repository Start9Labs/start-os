use std::{fs::File, io::stdout, path::Path};

#[macro_use] extern crate failure;
extern crate pest;
#[macro_use]
extern crate pest_derive;

mod backup;
mod config;
use anyhow::anyhow;
use backup::{create_backup, restore_backup};
use clap::{App, Arg, SubCommand};
use config::set_configuration;
use embassy::config::action::{ConfigRes, SetResult};
use embassy::Error;

fn main() {
    match inner_main() {
        Ok(a) => a,
        Err(e) => {
            eprintln!("{}", e.source);
            log::debug!("{:?}", e.source);
            drop(e.source);
            std::process::exit(e.kind as i32)
        }
    }
}

fn inner_main() {
    let app = App::new("compat")
        .subcommand(
            SubCommand::with_name("config").subcommand(
                SubCommand::with_name("get")
                    .arg(
                        Arg::with_name("mountpoint")
                            .help("Path to the config file")
                            .required(true),
                    )
                    .arg(
                        Arg::with_name("spec")
                            .help("The path to the config spec in the container")
                            .required(true),
                    ),
            )
            .subcommand(
                SubCommand::with_name("set")
                    .arg(
                        Arg::with_name("mountpoint")
                            .help("Path to the config file")
                            .required(true),
                    )
                    .arg(
                        Arg::with_name("app_id")
                            .help("service identifier")
                            .required(true),
                    )
                    .arg(
                        Arg::with_name("assets")
                            .help("Path to the rules file")
                            .required(true),
                    )
            )
        )
        .subcommand(
            SubCommand::with_name("dependency").subcommand(
                SubCommand::with_name("check")
                    .arg(
                        Arg::with_name("mountpoint")
                            .help("Path to the config rules file")
                            .required(true),
                    )
            ),
        )
        .subcommand(
            SubCommand::with_name("duplicity").subcommand(
                SubCommand::with_name("create")
                    .arg(
                        Arg::with_name("package-id")
                            .help("The `id` field from the manifest file")
                            .required(true),
                    )
                    .arg(
                        Arg::with_name("mountpoint")
                            .help(
                                "The backups mount point"
                            )
                            .required(true),
                    )
                    .arg(
                        Arg::with_name("datapath")
                            .help("The path to the data to be backed up in the container")
                            .required(true),
                    ),
            ),
        );
    let matches = app.get_matches();
    match matches.subcommand() {
        ("config", Some(sub_m)) => match sub_m.subcommand() {
            ("get", Some(sub_m)) => {
                let cfg_path =
                    Path::new(sub_m.value_of("mountpoint").unwrap()).join("start9/config.yaml");
                let cfg = if cfg_path.exists() {
                    Some(serde_yaml::from_reader(File::open(cfg_path).unwrap()).unwrap())
                } else {
                    None
                };
                let spec_path = Path::new(sub_m.value_of("spec").unwrap());
                let spec = serde_yaml::from_reader(File::open(spec_path).unwrap()).unwrap();
                serde_yaml::to_writer(stdout(), &ConfigRes { config: cfg, spec }).unwrap();
            },
            ("set", Some(sub_m)) => {
                // valiate against rules
                // save file
                let cfg_path =
                    Path::new(sub_m.value_of("mountpoint").unwrap());
                let config = serde_yaml::from_reader(File::open(cfg_path).unwrap()).unwrap();
                let rules_path =
                    Path::new(sub_m.value_of("assets").unwrap());
                let name = sub_m.value_of("app_id").unwrap();
                set_configuration(&name, config, rules_path, cfg_path);
            },
            (subcmd, _) => {
                panic!("unknown subcommand: {}", subcmd);
            }
        },
        ("dependency", Some(sub_m)) => match sub_m.subcommand() {
            ("check", Some(sub_m)) => {
                let cfg_path =
                    Path::new(sub_m.value_of("mountpoint").unwrap()).join("start9/config.yaml");
                let cfg = if cfg_path.exists() {
                    Some(serde_yaml::from_reader(File::open(cfg_path).unwrap()).unwrap())
                } else {
                    None
                };
                let spec_path = Path::new(sub_m.value_of("spec").unwrap());
                let spec = serde_yaml::from_reader(File::open(spec_path).unwrap()).unwrap();
                serde_yaml::to_writer(stdout(), &ConfigRes { config: cfg, spec }).unwrap();
            },
            (subcmd, _) => {
                panic!("unknown subcommand: {}", subcmd);
            }
        },
        ("duplicity", Some(sub_m)) => match sub_m.subcommand() {
            ("create", Some(sub_m)) => {
                let res = create_backup(
                        sub_m.value_of("mountpoint").unwrap(),
                        sub_m.value_of("datapath").unwrap(),
                        sub_m.value_of("package-id").unwrap(),
                    ).await;
                match res {
                    Ok(r) => {
                        serde_yaml::to_writer(stdout(), &r).unwrap()
                    }
                    Err(e) => {
                        log::error!("could not create backup: {}", e.source);
                    }
                };
            },
            ("restore", Some(sub_m)) => {
                let res = restore_backup(
                        sub_m.value_of("package-id").unwrap(),
                        sub_m.value_of("datapath").unwrap(),
                        sub_m.value_of("mountpoint").unwrap(),
                    ).await;
                match res {
                    Ok(r) => {
                        serde_yaml::to_writer(stdout(), &r).unwrap()
                    }
                    Err(e) => {
                        log::error!("could not restore backup: {}", e.source);
                    }
                };
            },
            (subcmd, _) => {
                panic!("unknown subcommand: {}", subcmd);
            },
        },
        (subcmd, _) => {
            panic!("unknown subcommand: {}", subcmd);
        }
    }
}