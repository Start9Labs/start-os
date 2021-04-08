use embassy::Error;

async fn inner_main() -> Result<(), Error> {
    // os sync
    embassy::volume::disk::mount("/dev/sda", "/mnt/embassy-os-crypt").await?;

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
