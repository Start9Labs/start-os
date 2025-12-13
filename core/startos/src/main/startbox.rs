use startos::bins::MultiExecutable;

fn main() {
    MultiExecutable::default()
        .enable_startd()
        .enable_start_cli()
        .execute()
}
