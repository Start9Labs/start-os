use std::path::{Path, PathBuf};
use std::str::FromStr;

use clap::Parser;
use ed25519_dalek::SigningKey;
use futures::future::{BoxFuture, FutureExt};
use serde::{Deserialize, Serialize};
use tokio::process::Command;

use crate::PackageId;
use crate::context::CliContext;
use crate::developer::write_developer_key;
use crate::prelude::*;
use crate::util::Invoke;

/// Per-workspace state directory, created at the workspace root. Its presence is
/// the marker that both `init-package` and start-cli walk up from cwd to find;
/// `schema` (in config.yaml) lets a future change migrate the contract.
pub const STARTOS_DIR: &str = ".startos";
/// Workspace-scoped ed25519 signing key (PKCS#8 PEM). Generated once and never
/// overwritten — it signs the packages built in this workspace.
pub const BUILD_KEY_FILE: &str = "build-key";
/// Workspace config: named host and registry targets the packager switches
/// between. Scaffolded with defaults; the `host` entries are placeholders to
/// point at your own StartOS boxes.
const CONFIG_FILE: &str = "config.yaml";
const WORKSPACE_CONFIG_CONTENTS: &str = r#"schema: 1
host:
  default: https://dev-vm.local
  prod: https://prodbox.local
registry:
  default: https://alpha-registry-x.start9.com
  beta: https://beta-registry.start9.com
  prod: https://registry.start9.com
"#;

/// Default source for the packaging guide (which also carries the package
/// template). Re-point the `start-docs` remote afterward to track a fork; the
/// session-start sync follows whatever remote is configured.
const START_DOCS_URL: &str = "https://github.com/Start9Labs/start-docs.git";
/// Workspace-relative path to the cloned guide.
const START_DOCS_DIR: &str = "start-docs";
/// Symlink target for the workspace `AGENTS.md` — the guide's canonical copy, so
/// a sync keeps the workspace context current with no extra step.
const AGENTS_SYMLINK_TARGET: &str = "start-docs/packaging/AGENTS.md";
/// Path to the package template inside the cloned guide.
const TEMPLATE_SUBPATH: &str = "packaging/package-template";

/// Claude Code does not auto-read `AGENTS.md`, so the workspace `CLAUDE.md`
/// imports both it and the user's local prefs.
const CLAUDE_MD_CONTENTS: &str = "@AGENTS.md\n@AGENTS.local.md\n";

/// Created once and never overwritten by a sync — the user's own context.
const AGENTS_LOCAL_STUB: &str = r#"# AGENTS.local.md — your workspace preferences

<!--
Notes and preferences for AI assistants working in this workspace. This file is yours;
syncing start-docs never overwrites it. Put anything you want always in scope here — the
registry you publish to, the packages you're focused on, local conventions, and so on.
-->
"#;

#[derive(Deserialize, Serialize, Parser)]
#[group(skip)]
pub struct InitWorkspaceParams {
    #[arg(help = "help.arg.workspace-path")]
    path: Option<PathBuf>,
}

/// Prepare a directory so any AI assistant working in it has the latest packaging
/// context in scope. Clones the guide, links the context files, and provisions
/// `.startos/` (a workspace signing key + target config). Idempotent: a re-run only
/// fills in what's missing; updates to the guide happen via the session-start sync
/// documented in `AGENTS.md`.
pub async fn init_workspace(
    _: CliContext,
    InitWorkspaceParams { path }: InitWorkspaceParams,
) -> Result<(), Error> {
    let root = if let Some(path) = path {
        path
    } else {
        std::env::current_dir().with_kind(ErrorKind::Filesystem)?
    };
    tokio::fs::create_dir_all(&root)
        .await
        .with_ctx(|_| (ErrorKind::Filesystem, root.display().to_string()))?;
    let root = tokio::fs::canonicalize(&root)
        .await
        .with_ctx(|_| (ErrorKind::Filesystem, root.display().to_string()))?;

    // Refuse nesting: a marker strictly above `root` would put a workspace inside a
    // workspace. A marker at `root` itself is just a re-run, which is allowed.
    if let Some(outer) = root.parent().map(find_workspace_root).transpose()?.flatten() {
        return Err(Error::new(
            eyre!(
                "{}",
                t!(
                    "s9pk.init.nested-workspace",
                    path = outer.display().to_string()
                )
            ),
            ErrorKind::InvalidRequest,
        ));
    }

    // Provision the guide. Clone only when absent — refreshing is the session-start
    // sync's job, not this command's, so an existing checkout is left untouched.
    let docs = root.join(START_DOCS_DIR);
    if !docs.exists() {
        eprintln!("{}", t!("s9pk.init.cloning-start-docs"));
        Command::new("git")
            .arg("clone")
            .arg("--depth")
            .arg("1")
            .arg("--branch")
            .arg("master")
            .arg(START_DOCS_URL)
            .arg(&docs)
            .capture(false)
            .invoke(ErrorKind::Git)
            .await?;
    }

    // Symlink (not a copy) so a guide sync keeps the workspace AGENTS.md current.
    // symlink_metadata treats a broken link as present, so a re-run never clobbers.
    let agents = root.join("AGENTS.md");
    if tokio::fs::symlink_metadata(&agents).await.is_err() {
        tokio::fs::symlink(AGENTS_SYMLINK_TARGET, &agents)
            .await
            .with_ctx(|_| (ErrorKind::Filesystem, agents.display().to_string()))?;
    }
    write_if_absent(&root.join("AGENTS.local.md"), AGENTS_LOCAL_STUB).await?;
    write_if_absent(&root.join("CLAUDE.md"), CLAUDE_MD_CONTENTS).await?;
    // .startos/ is the workspace marker (see find_workspace_root) and holds the
    // signing key + target config. Written last, so only a fully provisioned
    // directory counts as a workspace. The build-key is generated once and never
    // regenerated — overwriting it would change the workspace's signing identity.
    let startos = root.join(STARTOS_DIR);
    tokio::fs::create_dir_all(&startos)
        .await
        .with_ctx(|_| (ErrorKind::Filesystem, startos.display().to_string()))?;
    write_if_absent(&startos.join(CONFIG_FILE), WORKSPACE_CONFIG_CONTENTS).await?;
    let build_key = startos.join(BUILD_KEY_FILE);
    if tokio::fs::symlink_metadata(&build_key).await.is_err() {
        write_developer_key(
            &SigningKey::generate(&mut ssh_key::rand_core::OsRng::default()),
            &build_key,
        )
        .await?;
    }

    println!(
        "{}",
        t!(
            "s9pk.init.workspace-ready",
            path = root.display().to_string()
        )
    );
    Ok(())
}

#[derive(Deserialize, Serialize, Parser)]
#[group(skip)]
pub struct InitPackageParams {
    #[arg(help = "help.arg.package-display-name")]
    name: String,
}

/// Scaffold a new package from the workspace's bundled template, interpolating the
/// display name and a normalized ID, then `npm install` the result. Leaves a
/// `TODO.md` worklist that drives the package from clone to release-ready.
pub async fn init_package(
    _: CliContext,
    InitPackageParams { name }: InitPackageParams,
) -> Result<(), Error> {
    let cwd = std::env::current_dir().with_kind(ErrorKind::Filesystem)?;
    let root = find_workspace_root(&cwd)?.ok_or_else(|| {
        Error::new(
            eyre!("{}", t!("s9pk.init.not-in-workspace")),
            ErrorKind::InvalidRequest,
        )
    })?;

    // Normalize to a candidate ID, then validate it through the manifest's own
    // rules rather than re-implementing them.
    let id = normalize_id(&name);
    PackageId::from_str(&id).map_err(|_| {
        Error::new(
            eyre!(
                "{}",
                t!("s9pk.init.invalid-package-name", name = name.as_str())
            ),
            ErrorKind::InvalidId,
        )
    })?;

    let dst = cwd.join(format!("{id}-startos"));
    if dst.exists() {
        return Err(Error::new(
            eyre!(
                "{}",
                t!("s9pk.init.package-exists", path = dst.display().to_string())
            ),
            ErrorKind::InvalidRequest,
        ));
    }

    let template = root.join(START_DOCS_DIR).join(TEMPLATE_SUBPATH);
    if !template.exists() {
        return Err(Error::new(
            eyre!(
                "{}",
                t!(
                    "s9pk.init.template-missing",
                    path = template.display().to_string()
                )
            ),
            ErrorKind::NotFound,
        ));
    }

    copy_template_interpolated(&template, &dst, &id, &name).await?;

    eprintln!("{}", t!("s9pk.init.installing-deps"));
    Command::new("npm")
        .arg("install")
        .current_dir(&dst)
        .capture(false)
        .invoke(ErrorKind::Network)
        .await?;

    println!(
        "{}",
        t!(
            "s9pk.init.package-created",
            id = id,
            path = dst.display().to_string()
        )
    );
    Ok(())
}

/// Walk up from `start` (inclusive) for the nearest workspace — a directory
/// containing a `.startos` dir. (start-cli's own config/key discovery walks up
/// the same way; the outermost match may be the global `~/.startos`.)
fn find_workspace_root(start: &Path) -> Result<Option<PathBuf>, Error> {
    let mut dir = start.to_path_buf();
    loop {
        match std::fs::metadata(dir.join(STARTOS_DIR)) {
            Ok(meta) if meta.is_dir() => return Ok(Some(dir)),
            // Present but not a directory — not a marker; keep walking up.
            Ok(_) => {}
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
            // EACCES (or any other IO error) on an ancestor — treat as "no
            // accessible workspace here" and stop walking.
            Err(_) => return Ok(None),
        }
        if !dir.pop() {
            return Ok(None);
        }
    }
}

/// Normalize a human display name to a candidate package ID: lowercase ASCII
/// alphanumerics, runs of whitespace/underscore/hyphen collapsed to a single
/// hyphen, every other character dropped, no leading or trailing hyphen. Validity
/// (non-empty, matches the manifest's ID rules) is the caller's check.
fn normalize_id(name: &str) -> String {
    let mut out = String::new();
    let mut pending_sep = false;
    for ch in name.chars() {
        if ch.is_ascii_alphanumeric() {
            if pending_sep && !out.is_empty() {
                out.push('-');
            }
            pending_sep = false;
            out.push(ch.to_ascii_lowercase());
        } else if ch.is_whitespace() || ch == '_' || ch == '-' {
            pending_sep = true;
        }
        // any other character is dropped without forcing a separator
    }
    out
}

/// Recursively copy `src` to `dst`, replacing `{{id}}`/`{{name}}` in text files.
/// `{{id}}` is already validated as safe; `{{name}}` is escaped for TypeScript
/// string literals when writing `.ts` files. Non-UTF-8 files are copied verbatim.
fn copy_template_interpolated<'a>(
    src: &'a Path,
    dst: &'a Path,
    id: &'a str,
    name: &'a str,
) -> BoxFuture<'a, Result<(), Error>> {
    async move {
        tokio::fs::create_dir_all(dst)
            .await
            .with_ctx(|_| (ErrorKind::Filesystem, dst.display().to_string()))?;
        let mut entries = tokio::fs::read_dir(src)
            .await
            .with_ctx(|_| (ErrorKind::Filesystem, src.display().to_string()))?;
        while let Some(entry) = entries
            .next_entry()
            .await
            .with_kind(ErrorKind::Filesystem)?
        {
            let file_name = entry.file_name();
            // Defensive: never carry build/VCS cruft into the scaffold.
            if matches!(
                file_name.to_str(),
                Some("node_modules") | Some(".git") | Some("javascript")
            ) {
                continue;
            }
            let from = entry.path();
            let to = dst.join(&file_name);
            let file_type = entry.file_type().await.with_kind(ErrorKind::Filesystem)?;
            if file_type.is_dir() {
                copy_template_interpolated(&from, &to, id, name).await?;
            } else if file_type.is_file() {
                let bytes = tokio::fs::read(&from)
                    .await
                    .with_ctx(|_| (ErrorKind::Filesystem, from.display().to_string()))?;
                let escape_for_ts = from.extension().and_then(|e| e.to_str()) == Some("ts");
                let rendered = match String::from_utf8(bytes) {
                    Ok(text) => interpolate(&text, id, name, escape_for_ts).into_bytes(),
                    Err(e) => e.into_bytes(),
                };
                tokio::fs::write(&to, rendered)
                    .await
                    .with_ctx(|_| (ErrorKind::Filesystem, to.display().to_string()))?;
            }
            // symlinks (none expected in the template) are skipped
        }
        Ok(())
    }
    .boxed()
}

fn interpolate(content: &str, id: &str, name: &str, escape_for_ts: bool) -> String {
    let name = if escape_for_ts {
        name.replace('\\', "\\\\").replace('\'', "\\'")
    } else {
        name.to_owned()
    };
    content.replace("{{id}}", id).replace("{{name}}", &name)
}

/// Write `contents` to `path` only if nothing is there yet (a broken symlink
/// counts as present, so a re-run never clobbers).
async fn write_if_absent(path: &Path, contents: &str) -> Result<(), Error> {
    if tokio::fs::symlink_metadata(path).await.is_err() {
        tokio::fs::write(path, contents)
            .await
            .with_ctx(|_| (ErrorKind::Filesystem, path.display().to_string()))?;
    }
    Ok(())
}
