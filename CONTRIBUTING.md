# Contributing to StartOS

This guide is for contributing to the StartOS. If you are interested in packaging a service for StartOS, visit the [service packaging guide](https://github.com/Start9Labs/ai-service-packaging). If you are interested in promoting, providing technical support, creating tutorials, or helping in other ways, please visit the [Start9 website](https://start9.com/contribute).

## Collaboration

- [Matrix](https://matrix.to/#/#dev-startos:matrix.start9labs.com)

## Project Structure

```bash
/
├── assets/              # Screenshots for README
├── build/               # Auxiliary files and scripts for deployed images
├── container-runtime/   # Node.js program managing package containers
├── core/                # Rust backend: API, daemon (startd), CLI (start-cli)
├── debian/              # Debian package maintainer scripts
├── image-recipe/        # Scripts for building StartOS images
├── patch-db/            # (submodule) Diff-based data store for frontend sync
├── sdk/                 # TypeScript SDK for building StartOS packages
└── web/                 # Web UIs (Angular)
```

See component READMEs for details:

- [`core`](core/README.md)
- [`web`](web/README.md)
- [`build`](build/README.md)
- [`patch-db`](https://github.com/Start9Labs/patch-db)

## Environment Setup

### Installing Dependencies (Debian/Ubuntu)

> Debian/Ubuntu is the only officially supported build environment.
> MacOS has limited build capabilities and Windows requires [WSL2](https://learn.microsoft.com/en-us/windows/wsl/install).

```sh
sudo apt update
sudo apt install -y ca-certificates curl gpg build-essential
curl -fsSL https://download.docker.com/linux/debian/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
echo "deb [arch=$(dpkg-architecture -q DEB_HOST_ARCH) signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/debian bookworm stable" | sudo tee /etc/apt/sources.list.d/docker.list
sudo apt update
sudo apt install -y sed grep gawk jq gzip brotli containerd.io docker-ce docker-ce-cli docker-compose-plugin qemu-user-static binfmt-support squashfs-tools git debspawn rsync b3sum
sudo mkdir -p /etc/debspawn/
echo "AllowUnsafePermissions=true" | sudo tee /etc/debspawn/global.toml
sudo usermod -aG docker $USER
sudo su $USER
docker run --privileged --rm tonistiigi/binfmt --install all
docker buildx create --use
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh # proceed with default installation
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/master/install.sh | bash
source ~/.bashrc
nvm install 24
nvm use 24
nvm alias default 24 # this prevents your machine from reverting back to another version
```

### Cloning the Repository

```sh
git clone --recursive https://github.com/Start9Labs/start-os.git --branch next/major
cd start-os
```

### Development Mode

For faster iteration during development:

```sh
. ./devmode.sh
```

This sets `ENVIRONMENT=dev` and `GIT_BRANCH_AS_HASH=1` to prevent rebuilds on every commit.

## Building

All builds can be performed on any operating system that can run Docker.

This project uses [GNU Make](https://www.gnu.org/software/make/) to build its components.

### Requirements

- [GNU Make](https://www.gnu.org/software/make/)
- [Docker](https://docs.docker.com/get-docker/) or [Podman](https://podman.io/)
- [NodeJS v20.16.0](https://docs.npmjs.com/downloading-and-installing-node-js-and-npm)
- [Rust](https://rustup.rs/) (nightly for formatting)
- [sed](https://www.gnu.org/software/sed/), [grep](https://www.gnu.org/software/grep/), [awk](https://www.gnu.org/software/gawk/)
- [jq](https://jqlang.github.io/jq/)
- [gzip](https://www.gnu.org/software/gzip/), [brotli](https://github.com/google/brotli)

### Environment Variables

| Variable             | Description                                                                                         |
| -------------------- | --------------------------------------------------------------------------------------------------- |
| `PLATFORM`           | Target platform: `x86_64`, `x86_64-nonfree`, `aarch64`, `aarch64-nonfree`, `riscv64`, `raspberrypi` |
| `ENVIRONMENT`        | Hyphen-separated feature flags (see below)                                                          |
| `PROFILE`            | Build profile: `release` (default) or `dev`                                                         |
| `GIT_BRANCH_AS_HASH` | Set to `1` to use git branch name as version hash (avoids rebuilds)                                 |

**ENVIRONMENT flags:**

- `dev` - Enables password SSH before setup, skips frontend compression
- `unstable` - Enables assertions and debugging with performance penalty
- `console` - Enables tokio-console for async debugging

**Platform notes:**

- `-nonfree` variants include proprietary firmware and drivers
- `raspberrypi` includes non-free components by necessity
- Platform is remembered between builds if not specified

### Make Targets

#### Building

| Target        | Description                                    |
| ------------- | ---------------------------------------------- |
| `iso`         | Create full `.iso` image (not for raspberrypi) |
| `img`         | Create full `.img` image (raspberrypi only)    |
| `deb`         | Build Debian package                           |
| `all`         | Build all Rust binaries                        |
| `uis`         | Build all web UIs                              |
| `ui`          | Build main UI only                             |
| `ts-bindings` | Generate TypeScript bindings from Rust types   |

#### Deploying to Device

For devices on the same network:

| Target                               | Description                                     |
| ------------------------------------ | ----------------------------------------------- |
| `update-startbox REMOTE=start9@<ip>` | Deploy binary + UI only (fastest)               |
| `update-deb REMOTE=start9@<ip>`      | Deploy full Debian package                      |
| `update REMOTE=start9@<ip>`          | OTA-style update                                |
| `reflash REMOTE=start9@<ip>`         | Reflash as if using live ISO                    |
| `update-overlay REMOTE=start9@<ip>`  | Deploy to in-memory overlay (reverts on reboot) |

For devices on different networks (uses [magic-wormhole](https://github.com/magic-wormhole/magic-wormhole)):

| Target              | Description          |
| ------------------- | -------------------- |
| `wormhole`          | Send startbox binary |
| `wormhole-deb`      | Send Debian package  |
| `wormhole-squashfs` | Send squashfs image  |

### Creating a VM

Install virt-manager:

```sh
sudo apt update
sudo apt install -y virt-manager
sudo usermod -aG libvirt $USER
sudo su $USER
virt-manager
```

Follow the screenshot walkthrough in [`assets/create-vm/`](assets/create-vm/) to create a new virtual machine. Key steps:

1. Create a new virtual machine
2. Browse for the ISO — create a storage pool pointing to your `results/` directory
3. Select "Generic or unknown OS"
4. Set memory and CPUs
5. Create a disk and name the VM

Build an ISO first:

```sh
PLATFORM=$(uname -m) ENVIRONMENT=dev make iso
```

#### Other

| Target                   | Description                                 |
| ------------------------ | ------------------------------------------- |
| `format`                 | Run code formatting (Rust nightly required) |
| `test`                   | Run all automated tests                     |
| `test-core`              | Run Rust tests                              |
| `test-sdk`               | Run SDK tests                               |
| `test-container-runtime` | Run container runtime tests                 |
| `clean`                  | Delete all compiled artifacts               |

## Testing

```bash
make test                    # All tests
make test-core               # Rust tests (via ./core/run-tests.sh)
make test-sdk                # SDK tests
make test-container-runtime  # Container runtime tests

# Run specific Rust test
cd core && cargo test <test_name> --features=test
```

## Code Formatting

```bash
# Rust (requires nightly)
make format

# TypeScript/HTML/SCSS (web)
cd web && npm run format
```

## Code Style Guidelines

### Formatting

Run the formatters before committing. Configuration is handled by `rustfmt.toml` (Rust) and prettier configs (TypeScript).

### Documentation & Comments

**Rust:**

- Add doc comments (`///`) to public APIs, structs, and non-obvious functions
- Use `//` comments sparingly for complex logic that isn't self-evident
- Prefer self-documenting code (clear naming, small functions) over comments

**TypeScript:**

- Document exported functions and complex types with JSDoc
- Keep comments focused on "why" rather than "what"

**General:**

- Don't add comments that just restate the code
- Update or remove comments when code changes
- TODOs should include context: `// TODO(username): reason`

### Commit Messages

Use [Conventional Commits](https://www.conventionalcommits.org/):

```
<type>(<scope>): <description>

[optional body]

[optional footer]
```

**Types:**

- `feat` - New feature
- `fix` - Bug fix
- `docs` - Documentation only
- `style` - Formatting, no code change
- `refactor` - Code change that neither fixes a bug nor adds a feature
- `test` - Adding or updating tests
- `chore` - Build process, dependencies, etc.

**Examples:**

```
feat(web): add dark mode toggle
fix(core): resolve race condition in service startup
docs: update CONTRIBUTING.md with style guidelines
refactor(sdk): simplify package validation logic
```
