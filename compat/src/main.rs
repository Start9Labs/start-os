use std::{fs::File, io::stdout, path::Path};

mod backup;
use anyhow::anyhow;
use backup::{create_backup, restore_backup};
use clap::{App, Arg, SubCommand};
use embassy::config::action::{ConfigRes, SetResult};
use embassy::Error;
use futures::future::FutureExt;

async fn main() {
    let app = App::new("compat")
        .subcommand(
            SubCommand::with_name("config").subcommand(
                SubCommand::with_name("get")
                    .arg(
                        Arg::with_name("mountpoint")
                            .help("The `mount` field from manifest.yaml")
                            .required(true),
                    )
                    .arg(
                        Arg::with_name("spec")
                            .help("The path to the config spec in the container")
                            .required(true),
                    ),
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
                                "The backups mount point in the format: file:///<mount-point-path>",
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
            }
            ("set", Some(sub_m)) => {
                // valiate against rules
                // save file
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
            }
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
            }
            (subcmd, _) => {
                panic!("unknown subcommand: {}", subcmd);
            }
        },
        (subcmd, _) => {
            panic!("unknown subcommand: {}", subcmd);
        }
    }
}
