use startos::bins::MultiExecutable;

fn main() {
    MultiExecutable::default()
        .enable_start_tunnel()
        .enable_start_tunneld()
        .execute()
}
