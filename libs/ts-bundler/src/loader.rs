use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use deno_ast::MediaType;
use deno_ast::{ParseParams, SourceTextInfo};
use deno_core::futures::TryFutureExt;
use deno_core::ModuleSpecifier;
use deno_core::{
    anyhow::{bail, Error},
    futures::FutureExt,
    url,
};
use deno_emit::{LoadFuture, Loader};
use deno_graph::source::LoadResponse;

pub const BOOTSTRAP: &str = "file:///bootstrap.ts";
pub const BOOTSTRAP_CODE: &str = include_str!("./loader/bootstrap.ts");

#[derive(Clone, Debug)]
pub struct ModsLoader {
    project_root: Arc<PathBuf>,
}
impl ModsLoader {
    pub fn from(path: impl Into<PathBuf>) -> Self {
        Self {
            project_root: Arc::new(path.into()),
        }
    }
}
impl Loader for ModsLoader {
    fn load(&mut self, module_specifier: &ModuleSpecifier, _is_dyn_import: bool) -> LoadFuture {
        if module_specifier.as_str() == BOOTSTRAP {
            return Box::pin(async move {
                let code = BOOTSTRAP_CODE.to_string();
                let parsed = deno_ast::parse_module(ParseParams {
                    specifier: BOOTSTRAP.to_string(),
                    text_info: SourceTextInfo::from_string(code),
                    capture_tokens: false,
                    scope_analysis: false,
                    maybe_syntax: None,
                    media_type: MediaType::TypeScript,
                })?;
                let content = parsed.transpile(&Default::default())?.text.into();
                Ok(Some(LoadResponse::Module {
                    specifier: BOOTSTRAP.to_string().parse().unwrap(),
                    content,
                    maybe_headers: None,
                }))
            });
        }

        if module_specifier.as_str() == "file:///loadModule.js" {
            let specifier = module_specifier.clone();
            return Box::pin(async move { Ok(Some(LoadResponse::BuiltIn { specifier })) });
        }

        if module_specifier.as_str() == "file:///deno_global.js" {
            let specifier = module_specifier.clone();
            return Box::pin(async move { Ok(Some(LoadResponse::BuiltIn { specifier })) });
        }

        let project_root = self.project_root.clone();
        let module_specifier = module_specifier.clone();
        async move {
            let code = normalize_to_js(&project_root, &module_specifier).await?;
            let module = LoadResponse::Module {
                content: code.into(),
                specifier: module_specifier,
                maybe_headers: None,
            };
            Ok(Some(module))
        }
        .boxed_local()
    }
}

fn requires_transpilation(url: &url::Url) -> bool {
    match MediaType::from(url) {
        MediaType::JavaScript
        | MediaType::Mjs
        | MediaType::Cjs
        | MediaType::Json
        | MediaType::Wasm
        | MediaType::TsBuildInfo
        | MediaType::SourceMap
        | MediaType::Unknown => false,
        MediaType::Jsx
        | MediaType::TypeScript
        | MediaType::Mts
        | MediaType::Cts
        | MediaType::Dts
        | MediaType::Dmts
        | MediaType::Dcts
        | MediaType::Tsx => true,
    }
}

async fn normalize_to_js(
    project_root: &Path,
    module_specifier: &url::Url,
) -> Result<String, Error> {
    let media_type = MediaType::from(module_specifier);
    let should_transpile = requires_transpilation(module_specifier);
    let code_raw = if let Ok(path) = module_specifier.to_file_path() {
        let path = tokio::fs::canonicalize(project_root.join(path)).await?;
        Ok::<_, Error>(std::fs::read_to_string(project_root.join(&path))?)
    } else if let Ok(code) = reqwest::get(module_specifier.clone())
        .and_then(|r| async move { Ok(r.text().await?) })
        .await
    {
        Ok(code)
    } else {
        bail!("Expecting that the module specifier is a file path or url");
    }?;
    Ok(if should_transpile {
        let parsed = deno_ast::parse_module(ParseParams {
            specifier: module_specifier.to_string(),
            text_info: SourceTextInfo::from_string(code_raw),
            media_type,
            capture_tokens: false,
            scope_analysis: false,
            maybe_syntax: None,
        })?;
        let transpiled = parsed.transpile(&Default::default())?.text;
        transpiled
    } else {
        code_raw
    })
}
