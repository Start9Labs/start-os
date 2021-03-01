use std::borrow::Cow;
use std::ffi::{OsStr, OsString};
use std::marker::Unpin;
use std::path::{Path, PathBuf};
use std::pin::Pin;
use std::sync::{
    atomic::{self, AtomicBool, AtomicU64},
    Arc,
};
use std::task::Context;
use std::task::Poll;
use std::time::Duration;

use failure::ResultExt as _;
use futures::stream::StreamExt;
use futures::stream::TryStreamExt;
use tokio::io::AsyncWriteExt;
use tokio::io::{AsyncRead, ReadBuf};
use tokio_compat_02::FutureExt;
use tokio_tar as tar;

use crate::config::{ConfigRuleEntry, ConfigSpec};
use crate::manifest::{ImageConfig, Manifest, ManifestV0};
use crate::util::{from_cbor_async_reader, to_yaml_async_writer, AsyncCompat, PersistencePath};
use crate::version::VersionT;
use crate::ResultExt as _;

#[derive(Fail, Debug, Clone)]
pub enum Error {
    #[fail(display = "Package File Invalid or Corrupted: {}", _0)]
    CorruptedPkgFile(&'static str),
    #[fail(display = "Invalid File Name")]
    InvalidFileName,
}

pub async fn install_name(name_version: &str, use_cache: bool) -> Result<(), crate::Error> {
    let name = name_version.split("@").next().unwrap();
    let tmp_path = Path::new(crate::TMP_DIR).join(format!("{}.s9pk", name));
    if !use_cache || !tmp_path.exists() {
        download_name(name_version).await?;
    }
    install_path(
        &tmp_path
            .as_os_str()
            .to_str()
            .ok_or(Error::InvalidFileName)
            .with_code(crate::error::FILESYSTEM_ERROR)?,
        Some(name),
    )
    .await?;
    tokio::fs::remove_file(&tmp_path)
        .await
        .with_context(|e| format!("{}: {}", tmp_path.display(), e))
        .with_code(crate::error::FILESYSTEM_ERROR)?;
    Ok(())
}

struct CountingReader<R: AsyncRead>(pub R, pub Arc<AtomicU64>);
impl<R> AsyncRead for CountingReader<R>
where
    R: AsyncRead,
{
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut ReadBuf,
    ) -> Poll<std::io::Result<()>> {
        let atomic = self.as_ref().1.clone(); // TODO: not efficient
        match unsafe { self.map_unchecked_mut(|a| &mut a.0) }.poll_read(cx, buf) {
            Poll::Ready(Ok(())) => {
                atomic.fetch_add(buf.filled().len() as u64, atomic::Ordering::SeqCst);
                Poll::Ready(Ok(()))
            }
            a => a,
        }
    }
}

pub async fn download_name(name_version: &str) -> Result<PathBuf, crate::Error> {
    let mut split = name_version.split("@");
    let name = split.next().unwrap();
    let req: Option<emver::VersionRange> = split.next().map(|a| a.parse()).transpose().no_code()?;
    if let Some(req) = req {
        download(
            &format!("{}/{}.s9pk?spec={}", &*crate::APP_REGISTRY_URL, name, req),
            Some(name),
        )
        .await
    } else {
        download(
            &format!("{}/{}.s9pk", &*crate::APP_REGISTRY_URL, name),
            Some(name),
        )
        .await
    }
}

pub async fn download(url: &str, name: Option<&str>) -> Result<PathBuf, crate::Error> {
    let url = reqwest::Url::parse(url).no_code()?;
    log::info!("Downloading {}.", url.as_str());
    let response = reqwest::get(url)
        .compat()
        .await
        .with_code(crate::error::NETWORK_ERROR)?
        .error_for_status()
        .with_code(crate::error::REGISTRY_ERROR)?;
    tokio::fs::create_dir_all(crate::TMP_DIR).await?;
    let tmp_file_path =
        Path::new(crate::TMP_DIR).join(&format!("{}.s9pk", name.unwrap_or("download")));
    let mut f = tokio::fs::File::create(&tmp_file_path).await?;
    let len: Option<u64> = response.content_length().map(|a| {
        log::info!("{}KiB to download.", a / 1024);
        a
    });
    let done = Arc::new(AtomicBool::new(false));
    let counter = Arc::new(AtomicU64::new(0));
    let mut reader = CountingReader(
        AsyncCompat(
            response
                .bytes_stream()
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))
                .into_async_read(),
        ),
        counter.clone(),
    );
    let done_handle = done.clone();
    let download_handle = tokio::spawn(async move {
        let res = tokio::io::copy(&mut reader, &mut f).await;
        done_handle.store(true, atomic::Ordering::SeqCst);
        res
    });
    let poll_handle = tokio::spawn(async move {
        loop {
            let is_done = done.load(atomic::Ordering::SeqCst);
            let downloaded_bytes = counter.load(atomic::Ordering::SeqCst);
            if !*crate::QUIET.read().await {
                if let Some(len) = len {
                    print!("\rDownloading... {}%", downloaded_bytes * 100 / len);
                } else {
                    print!("\rDownloading... {}KiB", downloaded_bytes / 1024);
                }
            }
            if is_done {
                break;
            }
            tokio::time::sleep(Duration::from_millis(10)).await;
        }
        if !*crate::QUIET.read().await {
            println!("\rDownloading... 100%");
        }
    });
    download_handle.await.unwrap()?;
    poll_handle.await.unwrap();
    Ok(tmp_file_path)
}

pub async fn install_url(url: &str, name: Option<&str>) -> Result<(), crate::Error> {
    let tmp_file_path = download(url, name).await?;
    install_path(&tmp_file_path, name).await?;
    tokio::fs::remove_file(&tmp_file_path)
        .await
        .with_context(|e| format!("{}: {}", tmp_file_path.display(), e))
        .with_code(crate::error::FILESYSTEM_ERROR)?;
    Ok(())
}

pub async fn install_path<P: AsRef<Path>>(p: P, name: Option<&str>) -> Result<(), crate::Error> {
    let path = p.as_ref();
    log::info!(
        "Starting install of {}.",
        path.file_name()
            .and_then(|a| a.to_str())
            .ok_or(Error::InvalidFileName)
            .no_code()?
    );
    let file = tokio::fs::File::open(&path)
        .await
        .with_context(|e| format!("{}: {}", path.display(), e))
        .with_code(crate::error::FILESYSTEM_ERROR)?;
    let len = file.metadata().await?.len();
    let done = Arc::new(AtomicBool::new(false));
    let counter = Arc::new(AtomicU64::new(0));
    let done_handle = done.clone();
    let name_clone = name.map(|a| a.to_owned());
    let counter_clone = counter.clone();
    let poll_handle = tokio::spawn(async move {
        loop {
            let is_done = done.load(atomic::Ordering::SeqCst);
            let installed_bytes = counter.load(atomic::Ordering::SeqCst);
            if !*crate::QUIET.read().await {
                print!("\rInstalling... {}%", installed_bytes * 100 / len);
            }
            if is_done {
                break;
            }
            tokio::time::sleep(Duration::from_millis(10)).await;
        }
        if !*crate::QUIET.read().await {
            println!("\rInstalling... 100%");
        }
    });
    let reader = CountingReader(file, counter_clone);
    let res = install(reader, name_clone.as_ref().map(|a| a.as_str())).await;
    done_handle.store(true, atomic::Ordering::SeqCst);
    res?;
    poll_handle.await.unwrap();
    if !*crate::QUIET.read().await {
        println!("Complete.");
    }
    Ok(())
}

pub async fn install<R: AsyncRead + Unpin + Send + Sync>(
    r: R,
    name: Option<&str>,
) -> Result<(), crate::Error> {
    log::info!("Extracting archive.");
    let mut pkg = tar::Archive::new(r);
    let mut entries = pkg.entries()?;
    log::info!("Opening manifest from archive.");
    let manifest = entries
        .next()
        .await
        .ok_or(Error::CorruptedPkgFile("missing manifest"))
        .no_code()??;
    crate::ensure_code!(
        manifest.path()?.to_str() == Some("manifest.cbor"),
        crate::error::GENERAL_ERROR,
        "Package File Invalid or Corrupted"
    );
    log::trace!("Deserializing manifest.");
    let manifest: Manifest = from_cbor_async_reader(manifest).await.no_code()?;
    match manifest {
        Manifest::V0(m) => install_v0(m, entries, name).await?,
    };
    Ok(())
}

pub async fn install_v0<R: AsyncRead + Unpin + Send + Sync>(
    manifest: ManifestV0,
    mut entries: tar::Entries<R>,
    name: Option<&str>,
) -> Result<(), crate::Error> {
    crate::ensure_code!(
        crate::version::Current::new()
            .semver()
            .satisfies(&manifest.os_version_required),
        crate::error::VERSION_INCOMPATIBLE,
        "OS Version Not Compatible: need {}",
        manifest.os_version_required
    );
    if let Some(name) = name {
        crate::ensure_code!(
            manifest.id == name,
            crate::error::GENERAL_ERROR,
            "Package Name Does Not Match Expected"
        );
    }

    log::info!(
        "Creating metadata directory: {}/apps/{}",
        crate::PERSISTENCE_DIR,
        manifest.id
    );
    tokio::fs::create_dir_all(
        Path::new(crate::PERSISTENCE_DIR)
            .join("apps")
            .join(&manifest.id),
    )
    .await?;

    let (ip, tor_addr, tor_key) = crate::tor::set_svc(
        &manifest.id,
        crate::tor::NewService {
            ports: manifest.ports.clone(),
            hidden_service_version: manifest.hidden_service_version,
        },
    )
    .await?;

    let recoverable = Path::new(crate::VOLUMES).join(&manifest.id).exists();

    log::info!("Creating volume {}/{}.", crate::VOLUMES, manifest.id);
    tokio::fs::create_dir_all(Path::new(crate::VOLUMES).join(&manifest.id)).await?;

    let app_dir = PersistencePath::from_ref("apps").join(&manifest.id);
    let app_dir_path = app_dir.path();
    if app_dir_path.exists() {
        tokio::fs::remove_dir_all(&app_dir_path).await?;
    }
    tokio::fs::create_dir_all(&app_dir_path).await?;
    let _lock = app_dir.lock(true).await?;
    log::info!("Saving manifest.");
    let mut manifest_out = app_dir.join("manifest.yaml").write(None).await?;
    to_yaml_async_writer(&mut *manifest_out, &Manifest::V0(manifest.clone())).await?;
    manifest_out.commit().await?;
    log::info!("Opening config spec from archive.");
    let config_spec = entries
        .next()
        .await
        .ok_or(Error::CorruptedPkgFile("missing config spec"))
        .no_code()??;
    crate::ensure_code!(
        config_spec.path()?.to_str() == Some("config_spec.cbor"),
        crate::error::GENERAL_ERROR,
        "Package File Invalid or Corrupted"
    );
    log::trace!("Deserializing config spec.");
    let config_spec: ConfigSpec = from_cbor_async_reader(config_spec).await?;
    log::info!("Saving config spec.");
    let mut config_spec_out = app_dir.join("config_spec.yaml").write(None).await?;
    to_yaml_async_writer(&mut *config_spec_out, &config_spec).await?;
    config_spec_out.commit().await?;
    log::info!("Opening config rules from archive.");
    let config_rules = entries
        .next()
        .await
        .ok_or(Error::CorruptedPkgFile("missing config rules"))
        .no_code()??;
    crate::ensure_code!(
        config_rules.path()?.to_str() == Some("config_rules.cbor"),
        crate::error::GENERAL_ERROR,
        "Package File Invalid or Corrupted"
    );
    log::trace!("Deserializing config rules.");
    let config_rules: Vec<ConfigRuleEntry> = from_cbor_async_reader(config_rules).await?;
    log::info!("Saving config rules.");
    let mut config_rules_out = app_dir.join("config_rules.yaml").write(None).await?;
    to_yaml_async_writer(&mut *config_rules_out, &config_rules).await?;
    config_rules_out.commit().await?;
    if manifest.has_instructions {
        log::info!("Opening instructions from archive.");
        let mut instructions = entries
            .next()
            .await
            .ok_or(Error::CorruptedPkgFile("missing config rules"))
            .no_code()??;
        crate::ensure_code!(
            instructions.path()?.to_str() == Some("instructions.md"),
            crate::error::GENERAL_ERROR,
            "Package File Invalid or Corrupted"
        );
        log::info!("Saving instructions.");
        let mut instructions_out = app_dir.join("instructions.md").write(None).await?;
        tokio::io::copy(&mut instructions, &mut *instructions_out)
            .await
            .with_code(crate::error::FILESYSTEM_ERROR)?;
        instructions_out.commit().await?;
    }

    log::info!("Copying over assets.");
    for asset in manifest.assets.iter() {
        let dst_path = Path::new(crate::VOLUMES)
            .join(&manifest.id)
            .join(&asset.dst);
        log::info!("Copying {} to {}", asset.src.display(), dst_path.display());
        let src_path = Path::new(&asset.src);
        log::info!("Opening {} from archive.", src_path.display());
        let mut src = entries
            .next()
            .await
            .ok_or(Error::CorruptedPkgFile("missing asset"))
            .no_code()??;
        crate::ensure_code!(
            src.path()? == src_path,
            crate::error::GENERAL_ERROR,
            "Package File Invalid or Corrupted"
        );
        let dst_path_file = dst_path.join(src_path);
        if dst_path_file.exists() && !asset.overwrite {
            log::info!("{} already exists, skipping.", dst_path_file.display());
        } else {
            if dst_path_file.exists() {
                if dst_path_file.is_dir() {
                    tokio::fs::remove_dir_all(&dst_path_file)
                        .await
                        .with_context(|e| format!("{}: {}", dst_path_file.display(), e))
                        .with_code(crate::error::FILESYSTEM_ERROR)?;
                } else {
                    tokio::fs::remove_file(&dst_path_file)
                        .await
                        .with_context(|e| format!("{}: {}", dst_path_file.display(), e))
                        .with_code(crate::error::FILESYSTEM_ERROR)?;
                }
            }
            src.unpack_in(&dst_path).await?;
            if src.header().entry_type().is_dir() {
                loop {
                    let mut file = entries
                        .next()
                        .await
                        .ok_or(Error::CorruptedPkgFile("missing asset"))
                        .no_code()??;
                    if file
                        .path()?
                        .starts_with(format!("APPMGR_DIR_END:{}", asset.src.display()))
                    {
                        break;
                    } else {
                        file.unpack_in(&dst_path).await?;
                    }
                }
            }
        }
    }

    let tag = match &manifest.image {
        ImageConfig::Tar => {
            let image_name = format!("start9/{}", manifest.id);
            let tag = format!("{}:latest", image_name);
            if tokio::process::Command::new("docker")
                .arg("images")
                .arg("-q")
                .arg(&image_name)
                .output()
                .await?
                .stdout
                .len()
                > 0
            {
                tokio::process::Command::new("docker")
                    .arg("stop")
                    .arg(&manifest.id)
                    .spawn()?
                    .wait()
                    .await?;
                tokio::process::Command::new("docker")
                    .arg("rm")
                    .arg(&manifest.id)
                    .spawn()?
                    .wait()
                    .await?;
                crate::ensure_code!(
                    tokio::process::Command::new("docker")
                        .arg("rmi")
                        .arg(&image_name)
                        .output()
                        .await?
                        .status
                        .success(),
                    crate::error::DOCKER_ERROR,
                    "Failed to Remove Existing Image"
                )
            }
            log::info!("Opening image.tar from archive.");
            let mut image = entries
                .next()
                .await
                .ok_or(Error::CorruptedPkgFile("missing image.tar"))
                .no_code()??;
            let image_path = image.path()?;
            if image_path != Path::new("image.tar") {
                return Err(crate::Error::from(format_err!(
                    "Package File Invalid or Corrupted: expected image.tar, got {}",
                    image_path.display()
                )));
            }
            log::info!(
                "Loading docker image start9/{} from image.tar.",
                manifest.id
            );
            let mut child = tokio::process::Command::new("docker")
                .arg("load")
                .stdin(std::process::Stdio::piped())
                .stdout(std::process::Stdio::inherit())
                .stderr(match log::max_level() {
                    log::LevelFilter::Error => std::process::Stdio::null(),
                    _ => std::process::Stdio::inherit(),
                })
                .spawn()?;
            let mut child_in = child.stdin.take().unwrap();
            tokio::io::copy(&mut image, &mut child_in).await?;
            child_in.flush().await?;
            child_in.shutdown().await?;
            drop(child_in);
            crate::ensure_code!(
                child.wait().await?.success(),
                crate::error::DOCKER_ERROR,
                "Failed to Load Docker Image From Tar"
            );
            tag
        }
    };
    log::info!("Creating docker container: {} from {}.", manifest.id, tag);
    let volume_arg = format!(
        "type=bind,src={}/{},dst={}",
        crate::VOLUMES,
        manifest.id,
        manifest.mount.display()
    );
    let mut args = vec![
        Cow::Borrowed(OsStr::new("create")),
        Cow::Borrowed(OsStr::new("--restart")),
        Cow::Borrowed(OsStr::new("no")),
        Cow::Borrowed(OsStr::new("--name")),
        Cow::Borrowed(OsStr::new(&manifest.id)),
        Cow::Borrowed(OsStr::new("--mount")),
        Cow::Borrowed(OsStr::new(&volume_arg)),
        Cow::Borrowed(OsStr::new("--net")),
        Cow::Borrowed(OsStr::new("start9")),
        Cow::Borrowed(OsStr::new("--ip")),
        Cow::Owned(OsString::from(format!("{}", ip))),
    ];
    if let (Some(ref tor_addr), Some(ref tor_key)) = (&tor_addr, &tor_key) {
        args.extend(
            std::iter::empty()
                .chain(std::iter::once(Cow::Borrowed(OsStr::new("--env"))))
                .chain(std::iter::once(Cow::Owned(OsString::from(format!(
                    "TOR_ADDRESS={}",
                    tor_addr
                )))))
                .chain(std::iter::once(Cow::Borrowed(OsStr::new("--env"))))
                .chain(std::iter::once(Cow::Owned(OsString::from(format!(
                    "TOR_KEY={}",
                    tor_key
                ))))),
        );
    }
    if let Some(shm_size_mb) = manifest.shm_size_mb {
        args.push(Cow::Borrowed(OsStr::new("--shm-size")));
        args.push(Cow::Owned(OsString::from(format!("{}m", shm_size_mb))));
    }
    args.push(Cow::Borrowed(OsStr::new(&tag)));
    crate::ensure_code!(
        std::process::Command::new("docker")
            .args(&args)
            .stdout(std::process::Stdio::null())
            .stderr(match log::max_level() {
                log::LevelFilter::Error => std::process::Stdio::null(),
                _ => std::process::Stdio::inherit(),
            })
            .status()?
            .success(),
        crate::error::DOCKER_ERROR,
        "Failed to Create Docker Container"
    );
    tokio::fs::create_dir_all(Path::new(crate::VOLUMES).join(&manifest.id).join("start9")).await?;
    if let Some(public) = manifest.public {
        tokio::fs::create_dir_all(Path::new(crate::VOLUMES).join(&manifest.id).join(public))
            .await?;
    }
    if let Some(shared) = manifest.shared {
        tokio::fs::create_dir_all(Path::new(crate::VOLUMES).join(&manifest.id).join(shared))
            .await?;
    }
    log::info!("Updating app list.");
    crate::apps::add(
        &manifest.id,
        crate::apps::AppInfo {
            title: manifest.title.clone(),
            version: manifest.version.clone(),
            tor_address: tor_addr.clone(),
            configured: false,
            recoverable,
            needs_restart: false,
        },
    )
    .await?;
    let config = crate::apps::config(&manifest.id).await?;
    if let Some(cfg) = config.config {
        if config.spec.matches(&cfg).is_ok() {
            crate::apps::set_configured(&manifest.id, true).await?;
        }
    } else {
        let empty_config = crate::config::Config::default();
        if config.spec.matches(&empty_config).is_ok() {
            crate::config::configure(&manifest.id, Some(empty_config), None, false).await?;
        }
    }
    for (dep_id, dep_info) in manifest.dependencies.0 {
        if dep_info.mount_shared
            && crate::apps::list_info().await?.get(&dep_id).is_some()
            && crate::apps::manifest(&dep_id).await?.shared.is_some()
            && crate::apps::status(&dep_id, false).await?.status
                != crate::apps::DockerStatus::Stopped
        {
            crate::apps::set_needs_restart(&dep_id, true).await?;
        }
    }

    Ok(())
}
