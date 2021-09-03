use embassy::Error;

async fn inner_main() -> Result<(), Error> {
    // host setup flow if needed

    // mount disk
    embassy::volume::disk::mount("/dev/sda", "/mnt/embassy-os-crypt").await?; // TODO: by uuid

    // unlock disk

    // mount /var/log/journal

    // sync ssh

    // sync wifi

    // hostname-set
    embassy::hostname::sync_hostname().await?;

    Ok(())
}

fn main() {
    let rt = tokio::runtime::Runtime::new().expect("failed to initialize runtime");
    match rt.block_on(inner_main()) {
        Ok(_) => (),
        Err(e) => {
            drop(rt);
            eprintln!("{}", e.source);
            log::debug!("{:?}", e.source);
            drop(e.source);
            std::process::exit(e.kind as i32)
        }
    }
}
