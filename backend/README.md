# EmbassyOS Backend

- Requirements:
  - [Install Rust](https://rustup.rs)
  - Recommended: [rust-analyzer](https://rust-analyzer.github.io/)
  - [Docker](https://docs.docker.com/get-docker/)
  - [Rust ARM64 Build Container](https://github.com/Start9Labs/rust-arm-builder)
- Scripts (run withing the `./backend` directory)
  - `build-prod.sh` - compiles a release build of the artifacts for running on ARM64
  - `build-dev.sh` - compiles a development build of the artifacts for running on ARM64
- A Linux computer or VM

## Structure

The EmbassyOS backend is broken up into 4 different binaries:

- embassyd: This is the main workhorse of EmbassyOS - any new functionality you want will likely go here
- embassy-init: This is the component responsible for allowing you to set up your device, and handles system initialization on startup
- embassy-cli: This is a CLI tool that will allow you to issue commands to embassyd and control it similarly to the UI
- embassy-sdk: This is a CLI tool that aids in building and packaging services you wish to deploy to the Embassy

Finally there is a library `embassy` that supports all four of these tools.

See [here](/backend/Cargo.toml) for details.

## Building

You can build the entire operating system image using `make` from the root of the EmbassyOS project. This will subsequently invoke the build scripts above to actually create the requisite binaries and put them onto the final operating system image.

## Questions

If you have questions about how various pieces of the backend system work. Open an issue and tag the following people

- dr-bonez
- ProofOfKeags
