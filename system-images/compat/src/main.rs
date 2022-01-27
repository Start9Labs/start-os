use std::{
    env,
    fs::File,
    io::{stdin, stdout},
    path::Path,
};

#[macro_use]
extern crate failure;
extern crate pest;
#[macro_use]
extern crate pest_derive;

mod backup;
mod config;
use anyhow::anyhow;
use backup::{create_backup, restore_backup};
use clap::{App, Arg, SubCommand};
use config::{
    apply_dependency_configuration, validate_configuration, validate_dependency_configuration,
};
use embassy::config::action::ConfigRes;
use serde_json::json;

const PROPERTIES_FALLBACK_MESSAGE: &str =
    "Could not find properties. The service might still be starting";
pub enum CompatRes {
    SetResult,
    ConfigRes,
}

fn main() {
    match inner_main() {
        Ok(a) => a,
        Err(e) => {
            eprintln!("{}", e);
            log::debug!("{:?}", e.backtrace());
            drop(e);
            std::process::exit(1)
        }
    }
}

fn inner_main() -> Result<(), anyhow::Error> {
    let app = App::new("compat")
        .subcommand(
            SubCommand::with_name("config")
                .subcommand(
                    SubCommand::with_name("get")
                        .arg(
                            Arg::with_name("mountpoint")
                                .help("Path to the data mountpoint")
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
                            Arg::with_name("package_id")
                                .help("The `id` field from the manifest file")
                                .required(true),
                        )
                        .arg(
                            Arg::with_name("mountpoint")
                                .help("Path to the data mountpoint")
                                .required(true),
                        )
                        .arg(
                            Arg::with_name("assets")
                                .help("Path to the rules file")
                                .required(true),
                        )
                        .arg(
                            Arg::with_name("dependencies")
                                .help("Path to rules for optional dependency config")
                        ),
                ),
        )
        .subcommand(
            SubCommand::with_name("dependency")
                .subcommand(
                    SubCommand::with_name("check")
                        .arg(
                            Arg::with_name("dependent_package_id")
                                .help("Package identifier of this package (the child/depdendent)")
                                .required(true),
                        )
                        .arg(
                            Arg::with_name("dependency_package_id")
                                .help("Identifier of the dependency")
                                .required(true),
                        )
                        .arg(
                            Arg::with_name("mountpoint")
                                .help(" ountpoint for the dependent's config file")
                                .required(true),
                        )
                        .arg(
                            Arg::with_name("assets")
                                .help("Path to the dependency's config rules file")
                                .required(true),
                        ),
                )
                .subcommand(
                    SubCommand::with_name("auto-configure")
                        .arg(
                            Arg::with_name("dependent_package_id")
                                .help("Package identifier of this package (the child/depdendent)")
                                .required(true),
                        )
                        .arg(
                            Arg::with_name("dependency_package_id")
                                .help("Package identifier of the parent/dependency")
                                .required(true),
                        )
                        .arg(
                            Arg::with_name("mountpoint")
                                .help("Mountpoint for the dependent's config file")
                                .required(true),
                        )
                        .arg(
                            Arg::with_name("assets")
                                .help("Path to the dependency's config rules file")
                                .required(true),
                        ),
                ),
        )
        .subcommand(
            SubCommand::with_name("duplicity")
                .subcommand(
                    SubCommand::with_name("create")
                        .arg(
                            Arg::with_name("mountpoint")
                                .help("The backups mount point")
                                .required(true),
                        )
                        .arg(
                            Arg::with_name("datapath")
                                .help("The path to the data to be backed up in the container")
                                .required(true),
                        ),
                )
                .subcommand(
                    SubCommand::with_name("restore")
                        .arg(
                            Arg::with_name("mountpoint")
                                .help("The backups mount point")
                                .required(true),
                        )
                        .arg(
                            Arg::with_name("datapath")
                                .help("The path to the data to be restored to the container")
                                .required(true),
                        ),
                ),
        )
        .subcommand(
            SubCommand::with_name("properties").arg(
                Arg::with_name("mountpoint")
                    .help("The data directory of the service to mount to.")
                    .required(true),
            ).arg(
                Arg::with_name("fallbackMessage")
                    .help("The message to indicate that the startup is still working, or stats.yaml couldn't be found")
                    .required(false),
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
                serde_yaml::to_writer(stdout(), &ConfigRes { config: cfg, spec })?;
                Ok(())
            }
            ("set", Some(sub_m)) => {
                let config = serde_yaml::from_reader(stdin())?;
                let cfg_path = Path::new(sub_m.value_of("mountpoint").unwrap()).join("start9");
                if !cfg_path.exists() {
                    std::fs::create_dir_all(&cfg_path).unwrap();
                };
                let rules_path = Path::new(sub_m.value_of("assets").unwrap());
                let name = sub_m.value_of("package_id").unwrap();
                let deps_path = sub_m.value_of("dependencies");
                match validate_configuration(
                    &name,
                    config,
                    rules_path,
                    &cfg_path.join("config.yaml"),
                    deps_path,
                ) {
                    Ok(a) => {
                        serde_yaml::to_writer(stdout(), &a)?;
                        Ok(())
                    }
                    Err(e) => Err(e),
                }
            }
            (subcmd, _) => {
                panic!("Unknown subcommand: {}", subcmd);
            }
        },
        ("dependency", Some(sub_m)) => match sub_m.subcommand() {
            ("check", Some(sub_m)) => {
                let parent_config = serde_yaml::from_reader(stdin())?;
                let cfg_path =
                    Path::new(sub_m.value_of("mountpoint").unwrap()).join("start9/config.yaml");
                let config = if cfg_path.exists() {
                    Some(serde_yaml::from_reader(File::open(cfg_path).unwrap()).unwrap())
                } else {
                    None
                };
                let rules_path = Path::new(sub_m.value_of("assets").unwrap());
                let name = sub_m.value_of("dependent_package_id").unwrap();
                let parent_name = sub_m.value_of("dependency_package_id").unwrap();
                match validate_dependency_configuration(
                    name,
                    &config,
                    parent_name,
                    parent_config,
                    rules_path,
                ) {
                    Ok(a) => {
                        serde_yaml::to_writer(stdout(), &a)?;
                        Ok(())
                    }
                    Err(e) => {
                        // error string is configs rules failure description
                        Err(e)
                    }
                }
            }
            ("auto-configure", Some(sub_m)) => {
                let dep_config = serde_yaml::from_reader(stdin())?;
                let cfg_path =
                    Path::new(sub_m.value_of("mountpoint").unwrap()).join("start9/config.yaml");
                let config = if cfg_path.exists() {
                    Some(serde_yaml::from_reader(File::open(cfg_path).unwrap()).unwrap())
                } else {
                    None
                };
                let rules_path = Path::new(sub_m.value_of("assets").unwrap());
                let package_id = sub_m.value_of("dependent_package_id").unwrap();
                let dependency_id = sub_m.value_of("dependency_package_id").unwrap();
                match apply_dependency_configuration(
                    package_id,
                    config,
                    dependency_id,
                    dep_config,
                    rules_path,
                ) {
                    Ok(a) => {
                        serde_yaml::to_writer(stdout(), &a)?;
                        Ok(())
                    }
                    Err(e) => Err(e),
                }
            }
            (subcmd, _) => {
                panic!("Unknown subcommand: {}", subcmd);
            }
        },
        ("duplicity", Some(sub_m)) => match sub_m.subcommand() {
            ("create", Some(sub_m)) => {
                let res = create_backup(
                    sub_m.value_of("mountpoint").unwrap(),
                    sub_m.value_of("datapath").unwrap(),
                );
                match res {
                    Ok(r) => {
                        serde_yaml::to_writer(stdout(), &r)?;
                        Ok(())
                    }
                    Err(e) => Err(anyhow!("Could not create backup: {}", e)),
                }
            }
            ("restore", Some(sub_m)) => {
                let res = restore_backup(
                    sub_m.value_of("mountpoint").unwrap(),
                    sub_m.value_of("datapath").unwrap(),
                );
                match res {
                    Ok(r) => {
                        serde_yaml::to_writer(stdout(), &r)?;
                        Ok(())
                    }
                    Err(e) => Err(anyhow!("Could not restore backup: {}", e)),
                }
            }
            (subcmd, _) => {
                panic!("Unknown subcommand: {}", subcmd);
            }
        },
        ("properties", Some(sub_m)) => {
            let stats_path =
                Path::new(sub_m.value_of("mountpoint").unwrap()).join("start9/stats.yaml");
            let stats: serde_json::Value = if stats_path.exists() {
                serde_yaml::from_reader(File::open(stats_path).unwrap()).unwrap()
            } else {
                let fallback_message: &str = sub_m
                    .value_of("fallbackMessage")
                    .unwrap_or_else(|| PROPERTIES_FALLBACK_MESSAGE);
                json!({
                    "version": 2i64,
                    "data": {
                        "Not Ready": {
                            "type": "string",
                            "value": fallback_message,
                            "qr": false,
                            "copyable": false,
                            "masked": false,
                            "description":"Fallback Message When Properties could not be found"
                        }
                    }
                })
            };
            serde_json::to_writer(stdout(), &stats)?;
            Ok(())
        }
        (subcmd, _) => {
            panic!("Unknown subcommand: {}", subcmd);
        }
    }
}
