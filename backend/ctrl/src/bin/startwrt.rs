use startwrt_ctrl::bins::MultiExecutable;

fn main() {
    MultiExecutable::default()
        .enable_daemon()
        .enable_cli()
        .set_default("startwrt-cli")
        .execute()
}
