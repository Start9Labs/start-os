use startos::bins::MultiExecutable;

fn main() {
    MultiExecutable::default()
        .enable_start_container()
        .set_default("start-container")
        .execute()
}
