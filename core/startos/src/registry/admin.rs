use std::path::PathBuf;
use std::time::Duration;

use color_eyre::eyre::eyre;
use console::style;
use futures::StreamExt;
use indicatif::{ProgressBar, ProgressStyle};
use reqwest::{header, Body, Client, Url};
use rpc_toolkit::command;

use crate::s9pk::reader::S9pkReader;
use crate::s9pk::S9pk;
use crate::util::display_none;
use crate::{Error, ErrorKind};

async fn registry_user_pass(location: &str) -> Result<(Url, String, String), Error> {
    let mut url = Url::parse(location)?;
    let user = url.username().to_string();
    let pass = url.password().map(str::to_string);
    if user.is_empty() || url.path() != "/" {
        return Err(Error::new(
            eyre!("{location:?} is not like \"https://user@registry.example.com/\""),
            ErrorKind::ParseUrl,
        ));
    }
    let _ = url.set_username("");
    let _ = url.set_password(None);

    let pass = match pass {
        Some(p) => p,
        None => {
            let pass_prompt = format!("{} Password for {user}: ", style("?").yellow());
            tokio::task::spawn_blocking(move || rpassword::prompt_password(pass_prompt))
                .await
                .unwrap()?
        }
    };
    Ok((url, user.to_string(), pass.to_string()))
}

#[derive(serde::Serialize, Debug)]
struct Package {
    id: String,
    version: String,
    arches: Option<Vec<String>>,
}

async fn do_index(
    httpc: &Client,
    mut url: Url,
    user: &str,
    pass: &str,
    pkg: &Package,
) -> Result<(), Error> {
    url.set_path("/admin/v0/index");
    let req = httpc
        .post(url)
        .header(header::ACCEPT, "text/plain")
        .basic_auth(user, Some(pass))
        .json(pkg)
        .build()?;
    let res = httpc.execute(req).await?;
    if !res.status().is_success() {
        let info = res.text().await?;
        return Err(Error::new(eyre!("{}", info), ErrorKind::Registry));
    }
    Ok(())
}

async fn do_upload(
    httpc: &Client,
    mut url: Url,
    user: &str,
    pass: &str,
    body: Body,
) -> Result<(), Error> {
    url.set_path("/admin/v0/upload");
    let req = httpc
        .post(url)
        .header(header::ACCEPT, "text/plain")
        .basic_auth(user, Some(pass))
        .body(body)
        .build()?;
    let res = httpc.execute(req).await?;
    if !res.status().is_success() {
        let info = res.text().await?;
        return Err(Error::new(eyre!("{}", info), ErrorKind::Registry));
    }
    Ok(())
}

#[command(cli_only, display(display_none))]
pub async fn publish(
    #[arg] location: String,
    #[arg] path: PathBuf,
    #[arg(rename = "no-verify", long = "no-verify")] no_verify: bool,
    #[arg(rename = "no-upload", long = "no-upload")] no_upload: bool,
    #[arg(rename = "no-index", long = "no-index")] no_index: bool,
) -> Result<(), Error> {
    // Prepare for progress bars.
    let bytes_bar_style =
        ProgressStyle::with_template("{percent}% {wide_bar} [{bytes}/{total_bytes}] [{eta}]")
            .unwrap();
    let plain_line_style =
        ProgressStyle::with_template("{prefix:.bold.dim} {wide_msg}...").unwrap();
    let spinner_line_style =
        ProgressStyle::with_template("{prefix:.bold.dim} {spinner} {wide_msg}...").unwrap();

    // Read the file to get manifest information and check validity..
    // Open file right away so it can not change out from under us.
    let file = tokio::fs::File::open(&path).await?;

    let manifest = if no_verify {
        let pb = ProgressBar::new(1)
            .with_style(spinner_line_style.clone())
            .with_prefix("[1/3]")
            .with_message("Querying s9pk");
        pb.enable_steady_tick(Duration::from_millis(200));
        let mut s9pk = S9pk::open(&path).await?;
        let m = s9pk.as_manifest().clone();
        pb.set_style(plain_line_style.clone());
        pb.abandon();
        m
    } else {
        let pb = ProgressBar::new(1)
            .with_style(spinner_line_style.clone())
            .with_prefix("[1/3]")
            .with_message("Verifying s9pk");
        pb.enable_steady_tick(Duration::from_millis(200));
        let mut s9pk = S9pk::open(&path).await?;
        // s9pk.validate().await?;
        todo!();
        let m = s9pk.as_manifest().clone();
        pb.set_style(plain_line_style.clone());
        pb.abandon();
        m
    };
    let pkg = Package {
        id: manifest.id.to_string(),
        version: manifest.version.to_string(),
        arches: manifest.hardware_requirements.arch.clone(),
    };
    println!("{} id = {}", style(">").green(), pkg.id);
    println!("{} version = {}", style(">").green(), pkg.version);
    if let Some(arches) = &pkg.arches {
        println!("{} arches = {:?}", style(">").green(), arches);
    } else {
        println!(
            "{} No architecture listed in hardware_requirements",
            style(">").red()
        );
    }

    // Process the url and get the user's password.
    let (registry, user, pass) = registry_user_pass(&location).await?;

    // Now prepare a stream of the file which will show a progress bar as it is consumed.
    let file_size = file.metadata().await?.len();
    let file_stream = tokio_util::io::ReaderStream::new(file);
    ProgressBar::new(0)
        .with_style(plain_line_style.clone())
        .with_prefix("[2/3]")
        .with_message("Uploading s9pk")
        .abandon();
    let pb = ProgressBar::new(file_size).with_style(bytes_bar_style.clone());
    let stream_pb = pb.clone();
    let file_stream = file_stream.inspect(move |bytes| {
        if let Ok(bytes) = bytes {
            stream_pb.inc(bytes.len() as u64);
        }
    });

    let httpc = Client::builder().build().unwrap();
    // And upload!
    if no_upload {
        println!("{} Skipping upload", style(">").yellow());
    } else {
        do_upload(
            &httpc,
            registry.clone(),
            &user,
            &pass,
            Body::wrap_stream(file_stream),
        )
        .await?;
    }
    pb.finish_and_clear();

    // Also index, so it will show up in the registry.
    let pb = ProgressBar::new(0)
        .with_style(spinner_line_style.clone())
        .with_prefix("[3/3]")
        .with_message("Indexing registry");
    pb.enable_steady_tick(Duration::from_millis(200));
    if no_index {
        println!("{} Skipping index", style(">").yellow());
    } else {
        do_index(&httpc, registry.clone(), &user, &pass, &pkg).await?;
    }
    pb.set_style(plain_line_style.clone());
    pb.abandon();

    // All done
    if !no_index {
        println!(
            "{} Package {} is now published to {}",
            style(">").green(),
            pkg.id,
            registry
        );
    }
    Ok(())
}
