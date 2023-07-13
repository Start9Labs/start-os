# StartOS Backend

- Requirements:
  - [Install Rust](https://rustup.rs)
  - Recommended: [rust-analyzer](https://rust-analyzer.github.io/)
  - [Docker](https://docs.docker.com/get-docker/)
  - [Rust ARM64 Build Container](https://github.com/Start9Labs/rust-arm-builder)
- Scripts (run within the `./backend` directory)
  - `build-prod.sh` - compiles a release build of the artifacts for running on
    ARM64
- A Linux computer or VM

## Structure

The StartOS backend is packed into a single binary `startbox` that is symlinked under 
several different names for different behaviour:

- startd: This is the main workhorse of StartOS - any new functionality you
  want will likely go here
- start-cli: This is a CLI tool that will allow you to issue commands to
  startd and control it similarly to the UI
- start-sdk: This is a CLI tool that aids in building and packaging services
  you wish to deploy to StartOS

Finally there is a library `startos` that supports all of these tools.

See [here](/backend/Cargo.toml) for details.

## Building

You can build the entire operating system image using `make` from the root of
the StartOS project. This will subsequently invoke the build scripts above to
actually create the requisite binaries and put them onto the final operating
system image.

## Questions

If you have questions about how various pieces of the backend system work. Open
an issue and tag the following people

- dr-bonez
