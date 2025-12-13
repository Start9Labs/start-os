use startos::bins::MultiExecutable;

fn main() {
    startos::tunnel::context::TUNNEL_UI_CELL
        .set(include_dir::include_dir!(
            "$CARGO_MANIFEST_DIR/../../web/dist/static/start-tunnel"
        ))
        .ok();
    MultiExecutable::default()
        .enable_start_tunnel()
        .enable_start_tunneld()
        .execute()
}
