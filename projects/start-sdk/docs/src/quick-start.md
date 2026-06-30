# Quick Start

This guide walks you through scaffolding a new service package, building it, and installing it on StartOS. The scaffold is a working **Hello World** service — your starting point for packaging any app.

> [!NOTE]
> Complete [Environment Setup](./environment-setup.md) first — including [creating your packaging workspace](./environment-setup.md#set-up-your-packaging-workspace). `start-cli s9pk init-package` only runs inside a workspace.

## Scaffold the Package

From the root of the workspace you created during Environment Setup, scaffold a new package:

```sh
start-cli s9pk init-package "Hello World"
```

`init-package` normalizes the display name to a package ID, creates `hello-world-startos/` from the bundled template — a barebones, buildable Hello World clone — and runs `npm install` for you. It leaves a `TODO.md` checklist that takes the package from clone to release-ready.

Your workspace now looks like:

```
my-workspace/
├── .startos/
├── AGENTS.md
├── AGENTS.local.md
├── CLAUDE.md
├── start-technologies/
└── hello-world-startos/    ← your new package
```

> [!TIP]
> Already have a package repo? Clone it into the workspace alongside `start-technologies/` and build it the same way.

## Build the Package

```sh
cd hello-world-startos
make
```

Dependencies were already installed by `init-package`, so this goes straight to building. `make` produces a `.s9pk` for each architecture (`hello-world_x86_64.s9pk`, `hello-world_aarch64.s9pk`); run `make universal` instead for a single `hello-world.s9pk` that installs on any device. See [Makefile](./makefile.md) for all build targets.

## Install to StartOS

### Option 1: Sideload via UI

Open the `Sideload` tab and upload the `.s9pk` matching your device's architecture.

### Option 2: Direct Install (Local Network)

See [Installation](./makefile.md#installation).

## Next Steps

With Hello World running on your server, you're ready to package your own service. Open `hello-world-startos/` in your AI assistant and point it at the `TODO.md` checklist — it takes the package from Hello World clone to a real service (descriptions, image, icon, interfaces, daemons, docs).

Then browse the [Recipes](./recipes.md) to find the patterns your service needs — each describes an approach and points you to reference docs and real package code.
