#![type_length_limit = "10000000"]

use std::borrow::Cow;
use std::path::Path;

use appmgrlib::version::VersionT;
use appmgrlib::*;

use clap::{App, Arg, SubCommand};

#[tokio::main]
async fn main() {
    match inner_main().await {
        Ok(()) => (),
        Err(e) => {
            eprintln!("{}", e.failure);
            log::warn!("{:?}", e.failure);
            std::process::exit(e.code.unwrap_or(1));
        }
    }
}

async fn inner_main() -> Result<(), Error> {
    simple_logging::log_to_stderr(log::LevelFilter::Info);
    #[cfg(not(feature = "portable"))]
    {
        if !Path::new(crate::PERSISTENCE_DIR).join(".lock").exists() {
            tokio::fs::create_dir_all(crate::PERSISTENCE_DIR).await?;
            tokio::fs::File::create(Path::new(crate::PERSISTENCE_DIR).join(".lock")).await?;
        }
    }
    let q = *QUIET.read().await;
    *QUIET.write().await = true;
    #[cfg(not(feature = "portable"))]
    init().await?;
    *QUIET.write().await = q;
    let version = format!("{}", crate::version::Current::new().semver());
    let git_version =
        git_version::git_version!(args = ["--always", "--abbrev=40", "--dirty=-modified"]);
    #[cfg(not(feature = "production"))]
    let git_version = format!("{}-dev", git_version);
    #[allow(unused_mut)]
    let mut app = App::new("Start9 Application Manager")
        .version(version.as_str())
        .author("Dr. BoneZ <drbonez@start9labs.com>")
        .about("Manage applications installed on the Start9 Embassy")
        .arg(
            Arg::with_name("verbosity")
                .short("v")
                .help("Sets verbosity level")
                .multiple(true),
        )
        .subcommand(SubCommand::with_name("semver").about("Prints semantic version and exits"))
        .subcommand(SubCommand::with_name("git-info").about("Prints git version info and exits"))
        .subcommand(
            SubCommand::with_name("pack")
                .about("Creates a new application package")
                .arg(
                    Arg::with_name("output")
                        .short("o")
                        .long("output")
                        .takes_value(true)
                        .default_value("app.s9pk"),
                )
                .arg(
                    Arg::with_name("PATH")
                        .help("Path to the folder containing the application data")
                        .required(true),
                ),
        )
        .subcommand(
            SubCommand::with_name("verify")
                .about("Verifies an application package")
                .arg(
                    Arg::with_name("PATH")
                        .help("Path to the s9pk file to verify")
                        .required(true),
                ),
        )
        .subcommand(
            SubCommand::with_name("inspect")
                .about("Inspects an application package")
                .subcommand(
                    SubCommand::with_name("info")
                        .about("Prints information about an app")
                        .arg(
                            Arg::with_name("PATH")
                                .help("Path to the s9pk file to inspect")
                                .required(true),
                        )
                        .arg(
                            Arg::with_name("json")
                                .conflicts_with("yaml")
                                .required_unless("yaml")
                                .long("json")
                                .short("j")
                                .help("Output as json"),
                        )
                        .arg(
                            Arg::with_name("pretty")
                                .requires("json")
                                .long("pretty")
                                .short("p")
                                .help("Pretty print output"),
                        )
                        .arg(
                            Arg::with_name("yaml")
                                .conflicts_with("json")
                                .required_unless("json")
                                .long("yaml")
                                .short("y")
                                .help("Output as yaml"),
                        )
                        .arg(
                            Arg::with_name("include-manifest")
                                .long("include-manifest")
                                .short("m"),
                        )
                        .arg(
                            Arg::with_name("include-config")
                                .long("include-config")
                                .short("c"),
                        )
                        .arg(
                            Arg::with_name("only-manifest")
                                .long("only-manifest")
                                .short("M")
                                .conflicts_with_all(&[
                                    "include-manifest",
                                    "include-config",
                                    "only-config",
                                ]),
                        )
                        .arg(
                            Arg::with_name("only-config")
                                .long("only-config")
                                .short("C")
                                .conflicts_with_all(&[
                                    "include-manifest",
                                    "include-config",
                                    "only-manifest",
                                ]),
                        ),
                )
                .subcommand(
                    SubCommand::with_name("instructions")
                        .about("Prints instructions for an app")
                        .arg(
                            Arg::with_name("PATH")
                                .help("Path to the s9pk file to inspect")
                                .required(true),
                        ),
                ),
        )
        .subcommand(
            SubCommand::with_name("index")
                .about("Indexes all s9pk files in a directory")
                .arg(
                    Arg::with_name("DIR")
                        .help("Path to the directory to index")
                        .required(true),
                ),
        );

    #[cfg(not(feature = "portable"))]
    let mut app = app
        .subcommand(
            SubCommand::with_name("install")
                .about("Installs a new app")
                .arg(
                    Arg::with_name("no-cache")
                        .long("no-cache")
                        .help("Replace cached download of application"),
                )
                .arg(
                    Arg::with_name("ID|PATH|URL")
                        .help("The app to install")
                        .long_help(concat!(
                            "The app to install\n",
                            "ID: The id of the app in the Start9 registry\n",
                            "PATH: The path to the s9pk file on your local file system\n",
                            "URL: The url of the s9pk file"
                        ))
                        .required(true),
                ),
        )
        .subcommand(
            SubCommand::with_name("update")
                .about("Updates an app")
                .arg(
                    Arg::with_name("ID")
                        .help("The id of the app in the Start9 registry")
                        .required(true),
                )
                .arg(
                    Arg::with_name("dry-run")
                        .long("dry-run")
                        .help("Do not commit result"),
                )
                .arg(
                    Arg::with_name("json")
                        .conflicts_with("yaml")
                        .long("json")
                        .short("j")
                        .help("Output as json"),
                )
                .arg(
                    Arg::with_name("pretty")
                        .requires("json")
                        .long("pretty")
                        .short("p")
                        .help("Pretty print output"),
                )
                .arg(
                    Arg::with_name("yaml")
                        .conflicts_with("json")
                        .long("yaml")
                        .short("y")
                        .help("Output as yaml"),
                ),
        )
        .subcommand(
            SubCommand::with_name("start")
                .about("Starts an app")
                .arg(Arg::with_name("ID").help("The app to start").required(true)),
        )
        .subcommand(
            SubCommand::with_name("stop")
                .about("Stops an app")
                .arg(Arg::with_name("ID").help("The app to stop").required(true))
                .arg(
                    Arg::with_name("dry-run")
                        .long("dry-run")
                        .help("Do not commit result"),
                )
                .arg(
                    Arg::with_name("json")
                        .conflicts_with("yaml")
                        .long("json")
                        .short("j")
                        .help("Output as json"),
                )
                .arg(
                    Arg::with_name("pretty")
                        .requires("json")
                        .long("pretty")
                        .short("p")
                        .help("Pretty print output"),
                )
                .arg(
                    Arg::with_name("yaml")
                        .conflicts_with("json")
                        .long("yaml")
                        .short("y")
                        .help("Output as yaml"),
                ),
        )
        .subcommand(
            SubCommand::with_name("restart")
                .about("Restarts an app")
                .arg(
                    Arg::with_name("ID")
                        .help("The app to restart")
                        .required(true),
                ),
        )
        .subcommand(
            SubCommand::with_name("configure")
                .about("Configures an app")
                .arg(
                    Arg::with_name("ID")
                        .help("The app to configure")
                        .required(true),
                )
                .arg(Arg::with_name("FILE").help("The configuration file to use"))
                .arg(
                    Arg::with_name("stdin")
                        .long("stdin")
                        .help("Use stdin for the config file")
                        .conflicts_with("FILE"),
                )
                .arg(
                    Arg::with_name("timeout")
                        .short("t")
                        .long("timeout")
                        .help("Max seconds to attempt generating entropy per field")
                        .default_value("3")
                        .conflicts_with("no-timeout"),
                )
                .arg(
                    Arg::with_name("no-timeout")
                        .long("no-timeout")
                        .help("Disable timeout on entropy generation")
                        .conflicts_with("timeout"),
                )
                .arg(
                    Arg::with_name("dry-run")
                        .long("dry-run")
                        .help("Do not commit result"),
                )
                .arg(
                    Arg::with_name("json")
                        .conflicts_with("yaml")
                        .long("json")
                        .short("j")
                        .help("Output as json"),
                )
                .arg(
                    Arg::with_name("pretty")
                        .requires("json")
                        .long("pretty")
                        .short("p")
                        .help("Pretty print output"),
                )
                .arg(
                    Arg::with_name("yaml")
                        .conflicts_with("json")
                        .long("yaml")
                        .short("y")
                        .help("Output as yaml"),
                ),
        )
        .subcommand(
            SubCommand::with_name("check-dependencies")
                .about("Check dependencies for an app")
                .arg(
                    Arg::with_name("ID")
                        .help("The app to check dependencies for.")
                        .required(true),
                )
                .arg(Arg::with_name("local-only").long("local-only").help(
                    "Disable reaching out to the Start9 registry if the app isn't installed.",
                ))
                .arg(
                    Arg::with_name("json")
                        .conflicts_with("yaml")
                        .long("json")
                        .short("j")
                        .help("Output as json"),
                )
                .arg(
                    Arg::with_name("pretty")
                        .requires("json")
                        .long("pretty")
                        .short("p")
                        .help("Pretty print output"),
                )
                .arg(
                    Arg::with_name("yaml")
                        .conflicts_with("json")
                        .long("yaml")
                        .short("y")
                        .help("Output as yaml"),
                ),
        )
        .subcommand(
            SubCommand::with_name("autoconfigure-dependency")
                .about("Automatically configure a dependency")
                .arg(
                    Arg::with_name("ID")
                        .help("The app to autoconfigure a dependency for.")
                        .required(true),
                )
                .arg(
                    Arg::with_name("DEPENDENCY")
                        .help("The dependency to autoconfigure.")
                        .required(true),
                )
                .arg(
                    Arg::with_name("dry-run")
                        .long("dry-run")
                        .help("Do not commit result"),
                )
                .arg(
                    Arg::with_name("json")
                        .conflicts_with("yaml")
                        .long("json")
                        .short("j")
                        .help("Output as json"),
                )
                .arg(
                    Arg::with_name("pretty")
                        .requires("json")
                        .long("pretty")
                        .short("p")
                        .help("Pretty print output"),
                )
                .arg(
                    Arg::with_name("yaml")
                        .conflicts_with("json")
                        .long("yaml")
                        .short("y")
                        .help("Output as yaml"),
                ),
        )
        .subcommand(
            SubCommand::with_name("remove")
                .alias("rm")
                .about("Removes an installed app")
                .arg(
                    Arg::with_name("purge")
                        .short("p")
                        .long("purge")
                        .help("Deletes all application data"),
                )
                .arg(
                    Arg::with_name("ID")
                        .help("ID of the application to be removed")
                        .required(true),
                )
                .arg(
                    Arg::with_name("dry-run")
                        .long("dry-run")
                        .help("Do not commit result"),
                )
                .arg(
                    Arg::with_name("json")
                        .conflicts_with("yaml")
                        .long("json")
                        .short("j")
                        .help("Output as json"),
                )
                .arg(
                    Arg::with_name("pretty")
                        .requires("json")
                        .long("pretty")
                        .short("p")
                        .help("Pretty print output"),
                )
                .arg(
                    Arg::with_name("yaml")
                        .conflicts_with("json")
                        .long("yaml")
                        .short("y")
                        .help("Output as yaml"),
                ),
        )
        .subcommand(
            SubCommand::with_name("tor")
                .about("Configures tor hidden services")
                .subcommand(
                    SubCommand::with_name("show")
                        .about("Shows the onion address for the hidden service")
                        .arg(
                            Arg::with_name("ID")
                                .help("ID of the application to get the onion address for")
                                .required(true),
                        ),
                )
                .subcommand(SubCommand::with_name("reload").about("Reloads the tor configuration")),
        )
        .subcommand(
            SubCommand::with_name("info")
                .about("Prints information about an installed app")
                .arg(
                    Arg::with_name("ID")
                        .help("ID of the application to print information about")
                        .required(true),
                )
                .arg(
                    Arg::with_name("json")
                        .conflicts_with("yaml")
                        .required_unless("yaml")
                        .long("json")
                        .short("j")
                        .help("Output as json"),
                )
                .arg(
                    Arg::with_name("pretty")
                        .requires("json")
                        .long("pretty")
                        .short("p")
                        .help("Pretty print output"),
                )
                .arg(
                    Arg::with_name("yaml")
                        .conflicts_with("json")
                        .required_unless("json")
                        .long("yaml")
                        .short("y")
                        .help("Output as yaml"),
                )
                .arg(
                    Arg::with_name("include-status")
                        .long("include-status")
                        .short("s"),
                )
                .arg(
                    Arg::with_name("include-manifest")
                        .long("include-manifest")
                        .short("m"),
                )
                .arg(
                    Arg::with_name("include-config")
                        .long("include-config")
                        .short("c"),
                )
                .arg(
                    Arg::with_name("include-dependencies")
                        .long("include-dependencies")
                        .short("d"),
                )
                .arg(
                    Arg::with_name("only-status")
                        .long("only-status")
                        .short("S")
                        .conflicts_with_all(&[
                            "include-status",
                            "include-manifest",
                            "include-config",
                            "include-dependencies",
                            "only-manifest",
                            "only-config",
                            "only-dependencies",
                        ]),
                )
                .arg(
                    Arg::with_name("only-manifest")
                        .long("only-manifest")
                        .short("M")
                        .conflicts_with_all(&[
                            "include-status",
                            "include-manifest",
                            "include-config",
                            "include-dependencies",
                            "only-status",
                            "only-config",
                            "only-dependencies",
                        ]),
                )
                .arg(
                    Arg::with_name("only-config")
                        .long("only-config")
                        .short("C")
                        .conflicts_with_all(&[
                            "include-status",
                            "include-manifest",
                            "include-config",
                            "include-dependencies",
                            "only-status",
                            "only-manifest",
                            "only-dependencies",
                        ]),
                )
                .arg(
                    Arg::with_name("only-dependencies")
                        .long("only-dependencies")
                        .short("D")
                        .conflicts_with_all(&[
                            "include-status",
                            "include-manifest",
                            "include-config",
                            "include-dependencies",
                            "only-status",
                            "only-manifest",
                            "only-config",
                        ]),
                ),
        )
        .subcommand(
            SubCommand::with_name("instructions")
                .about("Prints instructions for an installed app")
                .arg(
                    Arg::with_name("ID")
                        .help("ID of the application to print instructions for")
                        .required(true),
                ),
        )
        .subcommand(
            SubCommand::with_name("list")
                .alias("ls")
                .about("Lists apps successfully installed on the system")
                .arg(
                    Arg::with_name("json")
                        .conflicts_with("yaml")
                        .long("json")
                        .short("j")
                        .help("Output as json"),
                )
                .arg(
                    Arg::with_name("pretty")
                        .requires("json")
                        .long("pretty")
                        .short("p")
                        .help("Pretty print output"),
                )
                .arg(
                    Arg::with_name("yaml")
                        .conflicts_with("json")
                        .long("yaml")
                        .short("y")
                        .help("Output as yaml"),
                )
                .arg(
                    Arg::with_name("include-status")
                        .long("include-status")
                        .short("s"),
                )
                .arg(
                    Arg::with_name("include-manifest")
                        .long("include-manifest")
                        .short("m"),
                )
                .arg(
                    Arg::with_name("include-config")
                        .long("include-config")
                        .short("c"),
                )
                .arg(
                    Arg::with_name("include-dependencies")
                        .long("include-dependencies")
                        .short("d"),
                ),
        )
        .subcommand(
            SubCommand::with_name("self-update")
                .about("Updates appmgr")
                .arg(
                    Arg::with_name("VERSION_REQUIREMENT")
                        .help("Version requirement to update to (i.e. ^0.1.0)"),
                ),
        )
        .subcommand(
            SubCommand::with_name("logs")
                .about("Fetch the logs of an app")
                .arg(
                    Arg::with_name("ID")
                        .help("ID of the application to fetch logs for")
                        .required(true),
                )
                .arg(
                    Arg::with_name("details")
                        .help("Show extra details provided to logs")
                        .long("details"),
                )
                .arg(
                    Arg::with_name("follow")
                        .help("Follow log output")
                        .long("follow")
                        .short("f"),
                )
                .arg(
                    Arg::with_name("since")
                        .help(concat!(
                            "Show logs since timestamp (e.g. 2013-01-02T13:23:37)",
                            " or relative (e.g. 42m for 42 minutes)"
                        ))
                        .long("since")
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("tail")
                        .help("Number of lines to show from the end of the logs")
                        .long("tail")
                        .takes_value(true)
                        .default_value("all"),
                )
                .arg(
                    Arg::with_name("timestamps")
                        .help("Show timestamps")
                        .short("t")
                        .long("timestamps"),
                )
                .arg(
                    Arg::with_name("until")
                        .help(concat!(
                            "Show logs before a timestamp (e.g. 2013-01-02T13:23:37)",
                            " or relative (e.g. 42m for 42 minutes)"
                        ))
                        .long("until")
                        .takes_value(true),
                ),
        )
        .subcommand(
            SubCommand::with_name("notifications")
                .about("Get notifications broadcast by an app")
                .arg(
                    Arg::with_name("ID")
                        .help("ID of the application to get notifications for")
                        .required(true),
                )
                .arg(
                    Arg::with_name("json")
                        .conflicts_with("yaml")
                        .long("json")
                        .short("j")
                        .help("Output as json"),
                )
                .arg(
                    Arg::with_name("pretty")
                        .requires("json")
                        .long("pretty")
                        .short("p")
                        .help("Pretty print output"),
                )
                .arg(
                    Arg::with_name("yaml")
                        .conflicts_with("json")
                        .long("yaml")
                        .short("y")
                        .help("Output as yaml"),
                ),
        )
        .subcommand(
            SubCommand::with_name("stats")
                .about("Get stats broadcast by an app")
                .arg(
                    Arg::with_name("ID")
                        .help("ID of the application to get stats for")
                        .required(true),
                )
                .arg(
                    Arg::with_name("json")
                        .conflicts_with("yaml")
                        .required_unless("yaml")
                        .long("json")
                        .short("j")
                        .help("Output as json"),
                )
                .arg(
                    Arg::with_name("pretty")
                        .requires("json")
                        .long("pretty")
                        .short("p")
                        .help("Pretty print output"),
                )
                .arg(
                    Arg::with_name("yaml")
                        .conflicts_with("json")
                        .required_unless("json")
                        .long("yaml")
                        .short("y")
                        .help("Output as yaml"),
                ),
        )
        .subcommand(
            SubCommand::with_name("disks")
                .about("Manage external disks")
                .subcommand(
                    SubCommand::with_name("show")
                        .alias("list")
                        .alias("ls")
                        .about("List external drive information")
                        .arg(
                            Arg::with_name("json")
                                .conflicts_with("yaml")
                                .long("json")
                                .short("j")
                                .help("Output as json"),
                        )
                        .arg(
                            Arg::with_name("pretty")
                                .requires("json")
                                .long("pretty")
                                .short("p")
                                .help("Pretty print output"),
                        )
                        .arg(
                            Arg::with_name("yaml")
                                .conflicts_with("json")
                                .long("yaml")
                                .short("y")
                                .help("Output as yaml"),
                        ),
                )
                .subcommand(SubCommand::with_name("use")),
        )
        .subcommand(
            SubCommand::with_name("backup")
                .about("Manage app data backups")
                .subcommand(
                    SubCommand::with_name("create")
                        .about("Backup current app state")
                        .arg(
                            Arg::with_name("ID")
                                .help("ID of the application to backup data for")
                                .required(true),
                        )
                        .arg(
                            Arg::with_name("PARTITION")
                                .help("Logical name of the partition you would like to backup to")
                                .required(true),
                        )
                        .arg(
                            Arg::with_name("password")
                                .long("password")
                                .short("p")
                                .takes_value(true)
                                .help("Password to use for encryption of backup file"),
                        ),
                )
                .subcommand(
                    SubCommand::with_name("restore")
                        .about("Restore app state from backup")
                        .arg(
                            Arg::with_name("ID")
                                .help("ID of the application to restore data for")
                                .required(true),
                        )
                        .arg(
                            Arg::with_name("PARTITION")
                                .help("Logical name of the partition you would like to backup to")
                                .required(true),
                        )
                        .arg(
                            Arg::with_name("timestamp")
                                .long("timestamp")
                                .short("t")
                                .takes_value(true)
                                .help("Timestamp of the backup to restore"),
                        )
                        .arg(
                            Arg::with_name("password")
                                .long("password")
                                .short("p")
                                .takes_value(true)
                                .help("Password to use for encryption of backup file"),
                        ),
                ),
        )
        .subcommand(
            SubCommand::with_name("repair-app-status").about("Restarts crashed apps"), // TODO: remove
        )
        .subcommand(
            SubCommand::with_name("actions")
                .about("Perform an action for a service")
                .arg(
                    Arg::with_name("SERVICE")
                        .help("ID of the service to perform an action on")
                        .required(true),
                )
                .arg(Arg::with_name("ACTION").help("ID of the action to perform")),
        );

    let matches = app.clone().get_matches();

    log::set_max_level(match matches.occurrences_of("verbosity") {
        0 => log::LevelFilter::Error,
        1 => log::LevelFilter::Warn,
        2 => log::LevelFilter::Info,
        3 => log::LevelFilter::Debug,
        _ => log::LevelFilter::Trace,
    });

    match matches.subcommand() {
        ("semver", _) => {
            println!("{}", version);
        }
        ("git-info", _) => {
            println!("{}", git_version);
        }
        #[cfg(not(feature = "portable"))]
        ("install", Some(sub_m)) => {
            let target = sub_m.value_of("ID|PATH|URL").unwrap();
            if target.starts_with("https://") || target.starts_with("http://") {
                install_url(target, None).await?;
            } else if target.ends_with(".s9pk") {
                install_path(target, None).await?;
            } else {
                install_name(target, !sub_m.is_present("no-cache")).await?;
            }
        }
        #[cfg(not(feature = "portable"))]
        ("update", Some(sub_m)) => {
            let res = update(sub_m.value_of("ID").unwrap(), sub_m.is_present("dry-run")).await?;
            if sub_m.is_present("json") {
                if sub_m.is_present("pretty") {
                    println!(
                        "{}",
                        serde_json::to_string_pretty(&res).with_code(crate::error::SERDE_ERROR)?
                    );
                } else {
                    println!(
                        "{}",
                        serde_json::to_string(&res).with_code(crate::error::SERDE_ERROR)?
                    );
                }
            } else if sub_m.is_present("yaml") {
                println!(
                    "{}",
                    serde_yaml::to_string(&res).with_code(crate::error::SERDE_ERROR)?
                );
            } else if !res.is_empty() {
                use prettytable::{Cell, Row, Table};
                let mut table = Table::new();
                let heading = vec![
                    Cell::new("APPLICATION ID"),
                    Cell::new("STATUS"),
                    Cell::new("REASON"),
                ];
                table.add_row(Row::new(heading));
                for (name, reason) in res {
                    table.add_row(Row::new(vec![
                        Cell::new(&name),
                        Cell::new("Stopped"),
                        Cell::new(&format!("{}", reason)),
                    ]));
                }
                table.print(&mut std::io::stdout())?;
            }
        }
        #[cfg(not(feature = "portable"))]
        ("start", Some(sub_m)) => {
            start_app(sub_m.value_of("ID").unwrap(), true).await?;
        }
        #[cfg(not(feature = "portable"))]
        ("stop", Some(sub_m)) => {
            let res = stop_app(
                sub_m.value_of("ID").unwrap(),
                true,
                sub_m.is_present("dry-run"),
            )
            .await?;
            if sub_m.is_present("json") {
                if sub_m.is_present("pretty") {
                    println!(
                        "{}",
                        serde_json::to_string_pretty(&res).with_code(crate::error::SERDE_ERROR)?
                    );
                } else {
                    println!(
                        "{}",
                        serde_json::to_string(&res).with_code(crate::error::SERDE_ERROR)?
                    );
                }
            } else if sub_m.is_present("yaml") {
                println!(
                    "{}",
                    serde_yaml::to_string(&res).with_code(crate::error::SERDE_ERROR)?
                );
            } else if !res.is_empty() {
                use prettytable::{Cell, Row, Table};
                let mut table = Table::new();
                let heading = vec![
                    Cell::new("APPLICATION ID"),
                    Cell::new("STATUS"),
                    Cell::new("REASON"),
                ];
                table.add_row(Row::new(heading));
                for (name, reason) in res {
                    table.add_row(Row::new(vec![
                        Cell::new(&name),
                        Cell::new("Stopped"),
                        Cell::new(&format!("{}", reason)),
                    ]));
                }
                table.print(&mut std::io::stdout())?;
            }
        }
        #[cfg(not(feature = "portable"))]
        ("restart", Some(sub_m)) => {
            restart_app(sub_m.value_of("ID").unwrap()).await?;
        }
        #[cfg(not(feature = "portable"))]
        ("configure", Some(sub_m)) => {
            let config: Option<Config> = if let Some(path) = sub_m.value_of("FILE") {
                let p = Path::new(path);
                if p.extension() == Some(std::ffi::OsStr::new("json"))
                    || (sub_m.is_present("json")
                        && p.extension() != Some(std::ffi::OsStr::new("yaml")))
                {
                    Some(util::from_json_async_reader(tokio::fs::File::open(p).await?).await?)
                } else {
                    Some(util::from_yaml_async_reader(tokio::fs::File::open(p).await?).await?)
                }
            } else if sub_m.is_present("stdin") {
                if sub_m.is_present("json") {
                    Some(util::from_yaml_async_reader(tokio::io::stdin()).await?)
                } else {
                    Some(util::from_yaml_async_reader(tokio::io::stdin()).await?)
                }
            } else {
                None
            };
            let timeout = if sub_m.is_present("no-timeout") {
                None
            } else if let Some(t) = sub_m.value_of("timeout") {
                Some(std::time::Duration::from_secs(t.parse().no_code()?))
            } else {
                Some(std::time::Duration::from_secs(3))
            };
            let res = configure(
                sub_m.value_of("ID").unwrap(),
                config,
                timeout,
                sub_m.is_present("dry-run"),
            )
            .await?;
            if sub_m.is_present("json") {
                if sub_m.is_present("pretty") {
                    println!(
                        "{}",
                        serde_json::to_string_pretty(&res).with_code(crate::error::SERDE_ERROR)?
                    );
                } else {
                    println!(
                        "{}",
                        serde_json::to_string(&res).with_code(crate::error::SERDE_ERROR)?
                    );
                }
            } else if sub_m.is_present("yaml") {
                println!(
                    "{}",
                    serde_yaml::to_string(&res).with_code(crate::error::SERDE_ERROR)?
                );
            } else if !res.needs_restart.is_empty() || !res.stopped.is_empty() {
                use prettytable::{Cell, Row, Table};
                let mut table = Table::new();
                let heading = vec![
                    Cell::new("APPLICATION ID"),
                    Cell::new("STATUS"),
                    Cell::new("REASON"),
                ];
                table.add_row(Row::new(heading));
                for name in res.needs_restart {
                    table.add_row(Row::new(vec![
                        Cell::new(&name),
                        Cell::new("Needs Restart"),
                        Cell::new("Configuration Changed"),
                    ]));
                }
                for (name, reason) in res.stopped {
                    table.add_row(Row::new(vec![
                        Cell::new(&name),
                        Cell::new("Stopped"),
                        Cell::new(&format!("{}", reason)),
                    ]));
                }
                table.print(&mut std::io::stdout())?;
            }
        }
        #[cfg(not(feature = "portable"))]
        ("check-dependencies", Some(sub_m)) => {
            let res = apps::dependencies(
                sub_m.value_of("ID").unwrap(),
                sub_m.is_present("local-only"),
            )
            .await?;
            if sub_m.is_present("json") {
                if sub_m.is_present("pretty") {
                    println!(
                        "{}",
                        serde_json::to_string_pretty(&res).with_code(crate::error::SERDE_ERROR)?
                    );
                } else {
                    println!(
                        "{}",
                        serde_json::to_string(&res).with_code(crate::error::SERDE_ERROR)?
                    );
                }
            } else if sub_m.is_present("yaml") {
                println!(
                    "{}",
                    serde_yaml::to_string(&res).with_code(crate::error::SERDE_ERROR)?
                );
            } else if !res.0.is_empty() {
                use prettytable::{Cell, Row, Table};
                let mut table = Table::new();
                let heading = vec![
                    Cell::new("APPLICATION ID"),
                    Cell::new("REQUIRED"),
                    Cell::new("VIOLATION"),
                ];
                table.add_row(Row::new(heading));
                for (name, info) in res.0 {
                    table.add_row(Row::new(vec![
                        Cell::new(&name),
                        Cell::new(&format!("{}", info.required)),
                        Cell::new(&if let Some(error) = info.error {
                            format!("{}", error)
                        } else {
                            "N/A".to_owned()
                        }),
                    ]));
                }
                table.print(&mut std::io::stdout())?;
            } else {
                println!("No dependencies for {}", sub_m.value_of("ID").unwrap());
            }
        }
        ("autoconfigure-dependency", Some(sub_m)) => {
            let res = dependencies::auto_configure(
                sub_m.value_of("ID").unwrap(),
                sub_m.value_of("DEPENDENCY").unwrap(),
                sub_m.is_present("dry-run"),
            )
            .await?;
            if sub_m.is_present("json") {
                if sub_m.is_present("pretty") {
                    println!(
                        "{}",
                        serde_json::to_string_pretty(&res).with_code(crate::error::SERDE_ERROR)?
                    );
                } else {
                    println!(
                        "{}",
                        serde_json::to_string(&res).with_code(crate::error::SERDE_ERROR)?
                    );
                }
            } else if sub_m.is_present("yaml") {
                println!(
                    "{}",
                    serde_yaml::to_string(&res).with_code(crate::error::SERDE_ERROR)?
                );
            } else if !res.needs_restart.is_empty() || !res.stopped.is_empty() {
                use prettytable::{Cell, Row, Table};
                let mut table = Table::new();
                let heading = vec![
                    Cell::new("APPLICATION ID"),
                    Cell::new("STATUS"),
                    Cell::new("REASON"),
                ];
                table.add_row(Row::new(heading));
                for name in res.needs_restart {
                    table.add_row(Row::new(vec![
                        Cell::new(&name),
                        Cell::new("Needs Restart"),
                        Cell::new("Configuration Changed"),
                    ]));
                }
                for (name, reason) in res.stopped {
                    table.add_row(Row::new(vec![
                        Cell::new(&name),
                        Cell::new("Stopped"),
                        Cell::new(&format!("{}", reason)),
                    ]));
                }
                table.print(&mut std::io::stdout())?;
            }
        }
        #[cfg(not(feature = "portable"))]
        ("remove", Some(sub_m)) | ("rm", Some(sub_m)) => {
            let res = remove(
                sub_m.value_of("ID").unwrap(),
                sub_m.is_present("purge"),
                sub_m.is_present("dry-run"),
            )
            .await?;
            if sub_m.is_present("json") {
                if sub_m.is_present("pretty") {
                    println!(
                        "{}",
                        serde_json::to_string_pretty(&res).with_code(crate::error::SERDE_ERROR)?
                    );
                } else {
                    println!(
                        "{}",
                        serde_json::to_string(&res).with_code(crate::error::SERDE_ERROR)?
                    );
                }
            } else if sub_m.is_present("yaml") {
                println!(
                    "{}",
                    serde_yaml::to_string(&res).with_code(crate::error::SERDE_ERROR)?
                );
            } else if !res.is_empty() {
                use prettytable::{Cell, Row, Table};
                let mut table = Table::new();
                let heading = vec![
                    Cell::new("APPLICATION ID"),
                    Cell::new("STATUS"),
                    Cell::new("REASON"),
                ];
                table.add_row(Row::new(heading));
                for (name, reason) in res {
                    table.add_row(Row::new(vec![
                        Cell::new(&name),
                        Cell::new("Stopped"),
                        Cell::new(&format!("{}", reason)),
                    ]));
                }
                table.print(&mut std::io::stdout())?;
            }
        }
        #[cfg(not(feature = "portable"))]
        ("tor", Some(sub_m)) => match sub_m.subcommand() {
            ("show", Some(sub_sub_m)) => {
                println!(
                    "{}",
                    crate::tor::read_tor_address(sub_sub_m.value_of("ID").unwrap(), None).await?
                );
            }
            ("reload", Some(_)) => {
                crate::tor::reload().await?;
            }
            _ => {
                println!("{}", sub_m.usage());
                std::process::exit(1);
            }
        },
        #[cfg(not(feature = "portable"))]
        ("info", Some(sub_m)) => {
            let name = sub_m.value_of("ID").unwrap();
            let info = crate::apps::info_full(
                &name,
                sub_m.is_present("include-status") || sub_m.is_present("only-status"),
                sub_m.is_present("include-manifest") || sub_m.is_present("only-manifest"),
                sub_m.is_present("include-config") || sub_m.is_present("only-config"),
                sub_m.is_present("include-dependencies") || sub_m.is_present("only-dependencies"),
            )
            .await?;
            if sub_m.is_present("json") {
                if sub_m.is_present("pretty") {
                    if sub_m.is_present("only-status") {
                        println!(
                            "{}",
                            serde_json::to_string_pretty(&info.status)
                                .with_code(crate::error::SERDE_ERROR)?
                        );
                    } else if sub_m.is_present("only-manifest") {
                        println!(
                            "{}",
                            serde_json::to_string_pretty(&info.manifest)
                                .with_code(crate::error::SERDE_ERROR)?
                        );
                    } else if sub_m.is_present("only-config") {
                        println!(
                            "{}",
                            serde_json::to_string_pretty(&info.config)
                                .with_code(crate::error::SERDE_ERROR)?
                        );
                    } else if sub_m.is_present("only-dependencies") {
                        println!(
                            "{}",
                            serde_json::to_string_pretty(&info.dependencies)
                                .with_code(crate::error::SERDE_ERROR)?
                        );
                    } else {
                        println!(
                            "{}",
                            serde_json::to_string_pretty(&info)
                                .with_code(crate::error::SERDE_ERROR)?
                        );
                    }
                } else {
                    if sub_m.is_present("only-status") {
                        println!(
                            "{}",
                            serde_json::to_string(&info.status)
                                .with_code(crate::error::SERDE_ERROR)?
                        );
                    } else if sub_m.is_present("only-manifest") {
                        println!(
                            "{}",
                            serde_json::to_string(&info.manifest)
                                .with_code(crate::error::SERDE_ERROR)?
                        );
                    } else if sub_m.is_present("only-config") {
                        println!(
                            "{}",
                            serde_json::to_string(&info.config)
                                .with_code(crate::error::SERDE_ERROR)?
                        );
                    } else if sub_m.is_present("only-dependencies") {
                        println!(
                            "{}",
                            serde_json::to_string(&info.dependencies)
                                .with_code(crate::error::SERDE_ERROR)?
                        );
                    } else {
                        println!(
                            "{}",
                            serde_json::to_string(&info).with_code(crate::error::SERDE_ERROR)?
                        );
                    }
                }
            } else if sub_m.is_present("yaml") {
                if sub_m.is_present("only-status") {
                    println!(
                        "{}",
                        serde_yaml::to_string(&info.status).with_code(crate::error::SERDE_ERROR)?
                    );
                } else if sub_m.is_present("only-manifest") {
                    println!(
                        "{}",
                        serde_yaml::to_string(&info.manifest)
                            .with_code(crate::error::SERDE_ERROR)?
                    );
                } else if sub_m.is_present("only-config") {
                    println!(
                        "{}",
                        serde_yaml::to_string(&info.config).with_code(crate::error::SERDE_ERROR)?
                    );
                } else if sub_m.is_present("only-dependencies") {
                    println!(
                        "{}",
                        serde_yaml::to_string(&info.dependencies)
                            .with_code(crate::error::SERDE_ERROR)?
                    );
                } else {
                    println!(
                        "{}",
                        serde_yaml::to_string(&info).with_code(crate::error::SERDE_ERROR)?
                    );
                }
            }
        }
        #[cfg(not(feature = "portable"))]
        ("instructions", Some(sub_m)) => {
            crate::apps::print_instructions(sub_m.value_of("ID").unwrap()).await?;
        }
        #[cfg(not(feature = "portable"))]
        ("list", Some(sub_m)) | ("ls", Some(sub_m)) => {
            let info = crate::apps::list(
                sub_m.is_present("include-status"),
                sub_m.is_present("include-manifest"),
                sub_m.is_present("include-config"),
                sub_m.is_present("include-dependencies"),
            )
            .await?;
            if sub_m.is_present("json") {
                if sub_m.is_present("pretty") {
                    println!(
                        "{}",
                        serde_json::to_string_pretty(&info).with_code(crate::error::SERDE_ERROR)?
                    );
                } else {
                    println!(
                        "{}",
                        serde_json::to_string(&info).with_code(crate::error::SERDE_ERROR)?
                    );
                }
            } else if sub_m.is_present("yaml") {
                println!(
                    "{}",
                    serde_yaml::to_string(&info).with_code(crate::error::SERDE_ERROR)?
                );
            } else if !info.is_empty() {
                use prettytable::{Cell, Row, Table};
                let mut table = Table::new();
                let mut heading = vec![
                    Cell::new("APPLICATION ID"),
                    Cell::new("TITLE"),
                    Cell::new("VERSION"),
                    Cell::new("TOR ADDRESS"),
                    Cell::new("CONFIGURED"),
                ];
                if sub_m.is_present("include-status") {
                    heading.push(Cell::new("STATUS"));
                }
                if sub_m.is_present("include-dependencies") {
                    heading.push(Cell::new("DEPENDENCIES MET"))
                }
                table.add_row(Row::new(heading));
                for (name, info) in info {
                    table.add_row(Row::new(
                        vec![
                            Cell::new(&name),
                            Cell::new(&format!("{}", info.info.title)),
                            Cell::new(&format!("{}", info.info.version)),
                            Cell::new(&format!(
                                "{}",
                                info.info.tor_address.unwrap_or_else(|| "N/A".to_owned())
                            )),
                            Cell::new(&format!("{}", info.info.configured)),
                        ]
                        .into_iter()
                        .chain(
                            info.status
                                .into_iter()
                                .map(|s| Cell::new(&format!("{:?}", s.status))),
                        )
                        .chain(info.dependencies.into_iter().map(|s| {
                            Cell::new(&format!(
                                "{}",
                                s.0.into_iter()
                                    .all(|(_, dep)| dep.error.is_none() || !dep.required)
                            ))
                        }))
                        .collect(),
                    ));
                }
                table.print(&mut std::io::stdout())?;
            } else {
                println!("No apps installed");
            }
        }
        #[cfg(not(feature = "portable"))]
        ("self-update", Some(sub_m)) => {
            self_update(
                sub_m
                    .value_of("VERSION_REQUIREMENT")
                    .map(|a| a.parse())
                    .transpose()
                    .no_code()?
                    .unwrap_or_else(|| emver::VersionRange::any()),
            )
            .await?;
        }
        #[cfg(not(feature = "portable"))]
        ("logs", Some(sub_m)) => {
            logs(
                sub_m.value_of("ID").unwrap(),
                LogOptions {
                    details: sub_m.is_present("details"),
                    follow: sub_m.is_present("follow"),
                    since: sub_m.value_of("since"),
                    until: sub_m.value_of("until"),
                    tail: sub_m
                        .value_of("tail")
                        .filter(|t| t != &"all")
                        .map(|a| a.parse())
                        .transpose()
                        .no_code()?,
                    timestamps: sub_m.is_present("timestamps"),
                },
            )
            .await?;
        }
        #[cfg(not(feature = "portable"))]
        ("notifications", Some(sub_m)) => {
            let info = notifications(sub_m.value_of("ID").unwrap()).await?;
            if sub_m.is_present("json") {
                if sub_m.is_present("pretty") {
                    println!(
                        "{}",
                        serde_json::to_string_pretty(&info).with_code(crate::error::SERDE_ERROR)?
                    );
                } else {
                    println!(
                        "{}",
                        serde_json::to_string(&info).with_code(crate::error::SERDE_ERROR)?
                    );
                }
            } else if sub_m.is_present("yaml") {
                println!(
                    "{}",
                    serde_yaml::to_string(&info).with_code(crate::error::SERDE_ERROR)?
                );
            } else if !info.is_empty() {
                use prettytable::{Cell, Row, Table};
                let mut table = Table::new();
                let heading = vec![
                    Cell::new("LEVEL"),
                    Cell::new("CODE"),
                    Cell::new("TITLE"),
                    Cell::new("MESSAGE"),
                ];
                table.add_row(Row::new(heading));
                for note in info {
                    table.add_row(Row::new(vec![
                        Cell::new(&format!("{}", note.level)),
                        Cell::new(&format!("{}", note.code)),
                        Cell::new(&format!("{}", note.title)),
                        Cell::new(&format!("{}", note.message)),
                    ]));
                }
                table.print(&mut std::io::stdout())?;
            } else {
                println!("No notifications for {}", sub_m.value_of("ID").unwrap());
            }
        }
        #[cfg(not(feature = "portable"))]
        ("stats", Some(sub_m)) => {
            let info = stats(sub_m.value_of("ID").unwrap()).await?;
            if sub_m.is_present("json") {
                if sub_m.is_present("pretty") {
                    println!(
                        "{}",
                        serde_json::to_string_pretty(&info).with_code(crate::error::SERDE_ERROR)?
                    );
                } else {
                    println!(
                        "{}",
                        serde_json::to_string(&info).with_code(crate::error::SERDE_ERROR)?
                    );
                }
            } else if sub_m.is_present("yaml") {
                println!(
                    "{}",
                    serde_yaml::to_string(&info).with_code(crate::error::SERDE_ERROR)?
                );
            } else if let serde_yaml::Value::Mapping(map) = info {
                use prettytable::{Cell, Row, Table};
                let mut table = Table::new();
                for (k, v) in map {
                    let ks = match k {
                        serde_yaml::Value::Bool(k) => format!("{}", k),
                        serde_yaml::Value::Null => "null".to_owned(),
                        serde_yaml::Value::Number(k) => format!("{}", k),
                        serde_yaml::Value::String(k) => k,
                        k => serde_yaml::to_string(&k).with_code(crate::error::SERDE_ERROR)?,
                    };
                    let vs = match v {
                        serde_yaml::Value::Bool(v) => format!("{}", v),
                        serde_yaml::Value::Null => "null".to_owned(),
                        serde_yaml::Value::Number(v) => format!("{}", v),
                        serde_yaml::Value::String(v) => v,
                        v => serde_yaml::to_string(&v).with_code(crate::error::SERDE_ERROR)?,
                    };
                    table.add_row(Row::new(vec![Cell::new(&ks), Cell::new(&vs)]));
                }
                table.print(&mut std::io::stdout())?;
            }
        }
        #[cfg(not(feature = "portable"))]
        ("disks", Some(sub_m)) => match sub_m.subcommand() {
            ("show", Some(sub_sub_m)) | ("list", Some(sub_sub_m)) | ("ls", Some(sub_sub_m)) => {
                let info = disks::list().await?;
                if sub_sub_m.is_present("json") {
                    if sub_sub_m.is_present("pretty") {
                        println!(
                            "{}",
                            serde_json::to_string_pretty(&info)
                                .with_code(crate::error::SERDE_ERROR)?
                        );
                    } else {
                        println!(
                            "{}",
                            serde_json::to_string(&info).with_code(crate::error::SERDE_ERROR)?
                        );
                    }
                } else if sub_sub_m.is_present("yaml") {
                    println!(
                        "{}",
                        serde_yaml::to_string(&info).with_code(crate::error::SERDE_ERROR)?
                    );
                } else {
                    todo!()
                }
            }
            _ => {
                println!("{}", sub_m.usage());
                std::process::exit(1);
            }
        },
        #[cfg(not(feature = "portable"))]
        ("backup", Some(sub_m)) => match sub_m.subcommand() {
            ("create", Some(sub_sub_m)) => {
                crate::backup::backup_to_partition(
                    sub_sub_m.value_of("PARTITION").unwrap(),
                    sub_sub_m.value_of("ID").unwrap(),
                    &match sub_sub_m.value_of("password") {
                        Some(a) => Cow::Borrowed(a),
                        None => Cow::Owned(rpassword::read_password_from_tty(Some("Password: "))?),
                    },
                )
                .await?
            }
            ("restore", Some(sub_sub_m)) => {
                crate::backup::restore_from_partition(
                    sub_sub_m.value_of("PARTITION").unwrap(),
                    sub_sub_m.value_of("ID").unwrap(),
                    &match sub_sub_m.value_of("password") {
                        Some(a) => Cow::Borrowed(a),
                        None => Cow::Owned(rpassword::read_password_from_tty(Some("Password: "))?),
                    },
                )
                .await?
            }
            _ => {
                println!("{}", sub_m.usage());
                std::process::exit(1);
            }
        },
        #[cfg(not(feature = "portable"))]
        ("repair-app-status", _) => {
            control::repair_app_status().await?;
        }
        #[cfg(not(feature = "portable"))]
        ("actions", Some(sub_m)) => {
            use yajrc::{GenericRpcMethod, RpcResponse};

            let man = apps::manifest(sub_m.value_of("SERVICE").unwrap()).await?;
            let action_id = sub_m.value_of("ACTION").unwrap();
            println!(
                "{}",
                serde_json::to_string(&RpcResponse::<GenericRpcMethod>::from_result(
                    man.actions
                        .iter()
                        .filter(|a| &a.id == &action_id)
                        .next()
                        .ok_or_else(|| {
                            failure::format_err!(
                                "action {} does not exist for {}",
                                action_id,
                                man.id
                            )
                        })
                        .with_code(error::NOT_FOUND)?
                        .perform(&man.id)
                        .await
                        .map(serde_json::Value::String)
                ))
                .with_code(error::SERDE_ERROR)?
            )
        }
        ("pack", Some(sub_m)) => {
            pack(
                sub_m.value_of("PATH").unwrap(),
                sub_m.value_of("output").unwrap(),
            )
            .await?
        }
        ("verify", Some(sub_m)) => verify(sub_m.value_of("PATH").unwrap()).await?,
        ("inspect", Some(sub_m)) => match sub_m.subcommand() {
            ("info", Some(sub_sub_m)) => {
                let path = sub_sub_m.value_of("PATH").unwrap();
                let info = crate::inspect::info_full(
                    path,
                    sub_sub_m.is_present("include-manifest")
                        || sub_sub_m.is_present("only-manifest"),
                    sub_sub_m.is_present("include-config") || sub_sub_m.is_present("only-config"),
                )
                .await?;
                if sub_sub_m.is_present("json") {
                    if sub_sub_m.is_present("pretty") {
                        if sub_sub_m.is_present("only-manifest") {
                            println!(
                                "{}",
                                serde_json::to_string_pretty(&info.manifest)
                                    .with_code(crate::error::SERDE_ERROR)?
                            );
                        } else if sub_sub_m.is_present("only-config") {
                            println!(
                                "{}",
                                serde_json::to_string_pretty(&info.config)
                                    .with_code(crate::error::SERDE_ERROR)?
                            );
                        } else {
                            println!(
                                "{}",
                                serde_json::to_string_pretty(&info)
                                    .with_code(crate::error::SERDE_ERROR)?
                            );
                        }
                    } else {
                        if sub_sub_m.is_present("only-manifest") {
                            println!(
                                "{}",
                                serde_json::to_string(&info.manifest)
                                    .with_code(crate::error::SERDE_ERROR)?
                            );
                        } else if sub_sub_m.is_present("only-config") {
                            println!(
                                "{}",
                                serde_json::to_string(&info.config)
                                    .with_code(crate::error::SERDE_ERROR)?
                            );
                        } else {
                            println!(
                                "{}",
                                serde_json::to_string(&info)
                                    .with_code(crate::error::SERDE_ERROR)?
                            );
                        }
                    }
                } else if sub_sub_m.is_present("yaml") {
                    if sub_sub_m.is_present("only-manifest") {
                        println!(
                            "{}",
                            serde_yaml::to_string(&info.manifest)
                                .with_code(crate::error::SERDE_ERROR)?
                        );
                    } else if sub_sub_m.is_present("only-config") {
                        println!(
                            "{}",
                            serde_yaml::to_string(&info.config)
                                .with_code(crate::error::SERDE_ERROR)?
                        );
                    } else {
                        println!(
                            "{}",
                            serde_yaml::to_string(&info).with_code(crate::error::SERDE_ERROR)?
                        );
                    }
                }
            }
            ("instructions", Some(sub_sub_m)) => {
                crate::inspect::print_instructions(Path::new(sub_sub_m.value_of("PATH").unwrap()))
                    .await?;
            }
            _ => {
                println!("{}", sub_m.usage());
                std::process::exit(1);
            }
        },
        ("index", Some(sub_m)) => {
            let idx = crate::index::index(Path::new(sub_m.value_of("DIR").unwrap())).await?;
            println!(
                "{}",
                serde_yaml::to_string(&idx).with_code(crate::error::SERDE_ERROR)?
            );
        }
        _ => {
            app.print_long_help().unwrap();
            std::process::exit(1);
        }
    }

    Ok(())
}
