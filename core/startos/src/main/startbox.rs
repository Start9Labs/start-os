use startos::bins::MultiExecutable;

fn main() {
    startos::net::static_server::UI_CELL
        .set(include_dir::include_dir!(
            "$CARGO_MANIFEST_DIR/../../web/dist/static/ui"
        ))
        .ok();
    startos::net::static_server::SETUP_WIZARD_CELL
        .set(include_dir::include_dir!(
            "$CARGO_MANIFEST_DIR/../../web/dist/static/setup-wizard"
        ))
        .ok();
    startos::net::static_server::INSTALL_WIZARD_CELL
        .set(include_dir::include_dir!(
            "$CARGO_MANIFEST_DIR/../../web/dist/static/install-wizard"
        ))
        .ok();
    startos::db::model::public::DB_UI_SEED_CELL
        .set(include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../web/patchdb-ui-seed.json"
        )))
        .ok();
    MultiExecutable::default()
        .enable_startd()
        .enable_start_cli()
        .execute()
}
