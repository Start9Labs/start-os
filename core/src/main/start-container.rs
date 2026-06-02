use startos::bins::MultiExecutable;

fn main() {
    // unshare-userns must be a multi-call applet, not a CLI subcommand:
    // it runs `unshare(CLONE_NEWUSER)`, which fails EINVAL on a
    // multi-threaded process, so it has to execute in a pristine
    // single-threaded process before any tokio runtime starts. A CLI
    // subcommand runs inside the multi-threaded runtime and can't.
    MultiExecutable::default()
        .enable_start_container()
        .enable_unshare_userns()
        .set_default("start-container")
        .execute()
}
