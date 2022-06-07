use std::{
    path::{Path, PathBuf},
    pin::Pin,
    sync::Arc,
};

use deno_ast::MediaType;
use deno_ast::{ParseParams, SourceTextInfo};
use deno_core::resolve_import;
use deno_core::ModuleLoader;
use deno_core::ModuleSource;
use deno_core::ModuleSourceFuture;
use deno_core::ModuleSpecifier;
use deno_core::ModuleType;
use deno_core::{
    anyhow::{anyhow, bail},
    futures::FutureExt,
};
use deno_core::{error::AnyError, futures::TryFutureExt};
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

        let project_root = self.project_root.clone();
        let module_specifier = module_specifier.clone();
        println!("BLUJ {:?} {:?}", project_root, module_specifier);
        async move {
            let mut code;
            let should_transpile: bool;
            let module_type: ModuleType;
            let media_type: MediaType;
            if let Ok(path) = module_specifier.to_file_path() {
                media_type = MediaType::from(&path);
                (module_type, should_transpile) = match MediaType::from(&path) {
                    MediaType::JavaScript | MediaType::Mjs | MediaType::Cjs => {
                        (ModuleType::JavaScript, false)
                    }
                    MediaType::Jsx => (ModuleType::JavaScript, true),
                    MediaType::TypeScript
                    | MediaType::Mts
                    | MediaType::Cts
                    | MediaType::Dts
                    | MediaType::Dmts
                    | MediaType::Dcts
                    | MediaType::Tsx => (ModuleType::JavaScript, true),
                    MediaType::Json => (ModuleType::Json, false),
                    _ => bail!("Unknown extension {:?}", path.extension()),
                };
                // TODO make this better
                let path = format!("./{}", path.to_string_lossy());
                let expected_path = project_root.join(&path);

                println!("BLUJ expected_path {:?} ", expected_path);
                code = std::fs::read_to_string(&project_root.join(&path))?;
            } else if let Ok(code_in) = reqwest::get(module_specifier.clone())
                .and_then(|r| async move { Ok(r.text().await?) })
                .await
            {
                (module_type, should_transpile) = match MediaType::from(&module_specifier) {
                    MediaType::JavaScript | MediaType::Mjs | MediaType::Cjs => {
                        (ModuleType::JavaScript, false)
                    }
                    MediaType::Jsx => (ModuleType::JavaScript, true),
                    MediaType::TypeScript
                    | MediaType::Mts
                    | MediaType::Cts
                    | MediaType::Dts
                    | MediaType::Dmts
                    | MediaType::Dcts
                    | MediaType::Tsx => (ModuleType::JavaScript, true),
                    MediaType::Json => (ModuleType::Json, false),
                    _ => bail!("Unknown extension {:?}", module_specifier),
                };
                media_type = MediaType::from(&module_specifier);
                code = code_in;
            } else {
                bail!("Expecting that the module specifier is a file path or url");
            }

            code = if should_transpile {
                let parsed = deno_ast::parse_module(ParseParams {
                    specifier: module_specifier.to_string(),
                    text_info: SourceTextInfo::from_string(code),
                    media_type,
                    capture_tokens: false,
                    scope_analysis: false,
                    maybe_syntax: None,
                })?;
                println!("BLUJ successfully parsed");
                let transpiled = parsed.transpile(&Default::default())?.text;
                println!("BLUJ successfully transpiled");
                transpiled
            } else {
                println!("BLUJ will not transpile");
                code
            };
            let content: Arc<str> = code.into();
            let module = LoadResponse::Module {
                content,
                specifier: module_specifier,
                maybe_headers: None,
            };
            Ok(Some(module))
        }
        .boxed_local()
    }
}
