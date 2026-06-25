# Environment Setup

Before building service packages, you need to install several development tools on your workstation. This page lists each prerequisite and how to install it. The final section — [Set Up Your Packaging Workspace](#set-up-your-packaging-workspace) — scaffolds the AI-assisted workspace that all packaging is designed around.

## StartOS Device

You must have a computer running StartOS to test your packages. Follow the [installation guide](/start-os/installing-startos.html) to install StartOS on a physical device or VM.

## Docker

[Docker](https://docs.docker.com/get-docker/) is essential for building and managing container images that will be used for the final `.s9pk` build. It handles pulling base images and building custom container images from Dockerfiles.

Follow the [official Docker installation guide](https://docs.docker.com/engine/install/) for your platform.

## Make

[Make](https://www.gnu.org/software/make/) is a build automation tool used to execute build scripts defined in Makefiles and coordinate the packaging workflow (building and installing s9pk binaries to StartOS).

**Linux (Debian-based)**:

```sh
sudo apt install build-essential
```

**macOS**:

```sh
xcode-select --install
```

## Node.js v22 (Latest LTS)

[Node.js](https://nodejs.org/en/) is required for compiling TypeScript code used in StartOS package configurations.

The recommended installation method is [nvm](https://github.com/nvm-sh/nvm):

```sh
nvm install 22
nvm use 22
```

You can also download Node.js directly from [nodejs.org](https://nodejs.org/).

## SquashFS

SquashFS is used to create compressed filesystem images that package your compiled service code.

**Linux (Debian-based)**:

```sh
sudo apt install squashfs-tools squashfs-tools-ng
```

**macOS** (requires [Homebrew](https://brew.sh/)):

```sh
brew install squashfs
```

## Start CLI

[start-cli](https://github.com/Start9Labs/start-os) is the core development toolkit for building StartOS packages. It provides package validation, s9pk file creation, and development workflow management.

Install using the automated installer script:

```sh
curl -fsSL https://start9.com/start-cli/install.sh | sh
```

## Verification

After installation, verify all tools are available:

```sh
docker --version
make --version
node --version
mksquashfs -version
start-cli --version
```

> [!TIP]
> If any command is not found, revisit the installation steps for that tool and ensure it is on your system PATH.

<!-- sdk-2.0: Hidden until start-cli ships `init-workspace` / `init-package` (SDK 2.0). These commands don't exist in the released CLI yet — restore this entire section, and the references marked `sdk-2.0:` in quick-start.md and README.md, when 2.0 publishes.

## Set Up Your Packaging Workspace

StartOS packaging is designed to be done with an AI coding agent. `start-cli` scaffolds an AI-ready **packaging workspace** in one command — a directory that holds the packaging guide and an agent-context file, so any assistant you open there already knows how to build a StartOS package. If you use [Claude Code](https://docs.anthropic.com/en/docs/claude-code), Start9 recommends the Opus 4.7 or later model.

### Create the workspace

```sh
start-cli s9pk init-workspace my-workspace
cd my-workspace
```

This clones the packaging guide into `start-docs/`, sets up the agent-context files (`AGENTS.md`, your own `AGENTS.local.md`, and a `CLAUDE.md` that loads both), and creates a `.startos/` directory that marks the workspace and holds your package-signing key and host/registry config:

```
my-workspace/
├── .startos/              ← workspace marker: build-key (signs your packages) + config.yaml (hosts, registries)
├── AGENTS.md              ← agent context (symlink into start-docs), read by AI assistants
├── AGENTS.local.md        ← your own notes, kept across guide updates
├── CLAUDE.md              ← loads AGENTS.md + AGENTS.local.md (Claude Code)
└── start-docs/            ← the packaging guide, read locally
```

The context lives once, at the workspace root — it is never copied into your package repos. Open the workspace in your AI tool and it picks up `AGENTS.md` / `CLAUDE.md` automatically.

### Create a package

From the workspace root, scaffold a new package:

```sh
start-cli s9pk init-package "My Service"
```

This creates `my-service-startos/` (the name is normalized to the package ID) as a barebones, buildable hello-world clone with a `TODO.md` checklist. Point your agent at that `TODO.md` and work it top to bottom to take the package from clone to release-ready. Your workspace now looks like:

```
my-workspace/
├── .startos/
├── AGENTS.md
├── AGENTS.local.md
├── CLAUDE.md
├── start-docs/
└── my-service-startos/    ← your new package
```

To work on an existing package instead, clone it into the workspace alongside `start-docs/`.

### Hosts and registries

The `.startos/config.yaml` created with the workspace defines named **host** targets (your StartOS boxes) and **registry** targets:

```yaml
schema: 1
host:
  default: https://dev-vm.local
  prod: https://prodbox.local
registry:
  default: https://alpha-registry-x.start9.com
  beta: https://beta-registry.start9.com
  prod: https://registry.start9.com
```

The `registry` entries are Start9's, pre-filled; edit the `host` entries to point at your own boxes.

Any `start-cli` command takes `-H`/`--host` and `-r`/`--registry`. Pass a **profile name** to use one of these entries, or a **URL** to target something directly:

```sh
start-cli -H prod <command>                  # uses host.prod
start-cli -r beta <command>                  # uses registry.beta
start-cli -H https://my-box.local <command>  # a URL works too
```

With no flag, the `default` entry is used. `start-cli` finds this config by walking up from the current directory, so it works anywhere inside the workspace.

> [!NOTE]
> `make install` and `make publish` read a single `host:` / `registry:` URL from the global `~/.startos/config.yaml` instead of these per-workspace profiles. See [Makefile](./makefile.md).

### Keep it current

The guide, the package template, and the agent context all live in `start-docs/`, so syncing it refreshes everything at once. Pull it at the start of each session:

```sh
git -C start-docs pull --ff-only
```

There's no separate update command — re-running `init-workspace` on an existing workspace just fills in anything missing, and your `AGENTS.local.md` is never touched.

-->

