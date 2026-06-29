# Quick Start

This guide walks you through creating your own service package repository from the [Hello World](https://github.com/Start9Labs/hello-world-startos) template, building it, and installing it on StartOS.

> [!NOTE]
> Ensure you have completed every step of [Environment Setup](./environment-setup.md) before beginning.

## Create Your Repository

Create your own repository from the [Hello World template](https://github.com/Start9Labs/hello-world-startos), named with the `<service-name>-startos` convention (e.g., `nextcloud-startos`).

On GitHub, click **"Use this template → Create new repository"**, then clone it:

```sh
git clone https://github.com/YOUR-USERNAME/YOUR-REPO.git
cd YOUR-REPO
```

Not using GitHub? Clone the template directly and re-initialize it as your own repository — then push it anywhere:

```sh
git clone --depth 1 https://github.com/Start9Labs/hello-world-startos.git my-service-startos
cd my-service-startos
rm -rf .git && git init && git add . && git commit -m "Initial commit from hello-world template"
```

## Build the Package

Install dependencies and build the package:

```sh
npm install
make
```

This generates a `hello-world.s9pk` file in the project root.

## Install to StartOS

### Option 1: Sideload via UI

Open the `Sideload` tab and upload the `.s9pk`.

### Option 2: Direct Install (Local Network)

See [Installation](./makefile.md#installation).

## Next Steps

With Hello World running on your server, you're ready to package your own service. Browse the [Recipes](./recipes.md) to find the patterns your service needs — each recipe describes the approach and points you to reference docs and real package code.

If you set up a [packaging workspace](./environment-setup.md#set-up-your-packaging-workspace) during environment setup, point your agent at the recipe for your first task and let it work from there.

