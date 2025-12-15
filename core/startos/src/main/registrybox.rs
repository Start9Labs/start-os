use startos::bins::MultiExecutable;

fn main() {
    MultiExecutable::default()
        .enable_start_registry()
        .enable_start_registryd()
        .execute()
}
