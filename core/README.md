# StartOS Backend

- Requirements:
  - [Install Rust](https://rustup.rs)
  - Recommended: [rust-analyzer](https://rust-analyzer.github.io/)
  - [Docker](https://docs.docker.com/get-docker/)

## Structure

- `startos`: This contains the core library for StartOS that supports building `startbox`.
- `helpers`: This contains utility functions used across both `startos` and `js-engine`
- `models`: This contains types that are shared across `startos`, `js-engine`, and `helpers`

## Artifacts

The StartOS backend is packed into a single binary `startbox` that is symlinked under
several different names for different behavior:

- `startd`: This is the main daemon of StartOS
- `start-cli`: This is a CLI tool that will allow you to issue commands to
  `startd` and control it similarly to the UI
- `start-sdk`: This is a CLI tool that aids in building and packaging services
  you wish to deploy to StartOS

## Questions

If you have questions about how various pieces of the backend system work. Open
an issue and tag the following people

- dr-bonez
