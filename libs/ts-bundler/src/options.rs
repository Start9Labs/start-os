use deno_ast::EmitOptions;
use deno_emit::{BundleOptions, BundleType};

lazy_static::lazy_static! {
    pub static ref BUNDLE_OPTIONS: BundleOptions = BundleOptions {
        bundle_type: BundleType::Module,
        emit_options: EMIT_OPTIONS.clone(),
        emit_ignore_directives: true,
    };

    pub static ref EMIT_OPTIONS: EmitOptions = EmitOptions {
        emit_metadata: true,
        imports_not_used_as_values: deno_ast::ImportsNotUsedAsValues::Preserve,
        inline_source_map: true,
        inline_sources: true,
        jsx_automatic: false,
        jsx_development: false,
        jsx_factory: String::from("React.createElement"),
        jsx_fragment_factory: String::from("React.Fragment"),
        jsx_import_source: None,
        source_map: true,
        var_decl_imports: false,
        transform_jsx: false,
    };
}
