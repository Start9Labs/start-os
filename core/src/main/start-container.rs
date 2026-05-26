use startos::bins::MultiExecutable;

fn main() {
    MultiExecutable::default()
        .enable_start_container()
        .enable_unshare_userns()
        .set_default("start-container")
        .execute()
}
