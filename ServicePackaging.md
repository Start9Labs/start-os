# EmbassyOS Service Packaging Guide

Welcome!  The following guide will provide the prerequisites, introduce a brief overview of the packaging process, use an example demonstrating how to package a service, and finally describe the submission process.  This essentially describes how you can take an existing app (or one you have written yourself), and wrap it up such that it can be added to an EmbassyOS Marketplace!

## Prerequisites

### EmbassyOS (EOS)

It is **HIGHLY RECOMMENDED** to have a copy of EmbassyOS for testing your packaged service.  

There are 3 options for this:
    1. build from [source](https://github.com/Start9Labs/embassy-os/blob/master/BuildGuide.md)
    2. follow the [DIY guide](https://docs.start9.com/getting-started/diy.html#diy) to build on a Raspberry Pi
    3. [purchase](https://docs.start9.com/getting-started/purchasing.html#purchasing) a device or copy of the OS

### Development Environment

Once you have EOS installed, you'll want to set up your development system with the necessary software.

At minimum you will need the following:
    1. [docker](https://docs.docker.com/get-docker)
    2. [docker-buildx](https://docs.docker.com/buildx/working-with-buildx/)
    3. ***PLACEHOLDER FOR EOS-SDK***

The following are recommended:
    4. [cargo](https://doc.rust-lang.org/cargo/)
    5. [yq](https://mikefarah.gitbook.io/yq/) (version 4)
    6. [make](https://www.gnu.org/software/make/)
    7. [rust-musl-cross](***PLACEHOLDER FOR NEW MUSL-CROSS REPO***) (For cross compiling Rust to Alpine, not needed otherwise)

## Overview

### Components

Simply, the package is made up of the following parts:
    1. Image - Each service is running in a Docker image.  Best results will come from an arm based linux; [Alpine](https://www.alpinelinux.org/) is highly recommended.
    2. Volume - Each service gets a volume, allocated by EOS.  The volume directory (for seeding data into the volume) is required: /root/volumes/<service-id>
    3. Dependencies - Rules and requirements of your service, which appear as simple UI elements, such as inputs, toggles, and drop-downs.  These are enforced by validations and clear user instructions.  EmbassyOS has a unique and powerful system for managing dependencies which allows anyone to have the power of systems administrators without the advanced skillset.
    4. Manifest - Describes the service and its requirements.  This is for the marketplace listing, install considerations, license, donation address, and dependency requirements, and additional info.
    5. Config - EOS makes a service's configuration available to the user in the GUI and must be valid regardless of user skill.
    6. .s9pk Bundle - The image, config, manifest, and icon files get bundled into a .s9pk package.  This is the file a user downloads from the Marketplace, at which point EOS un-packages and installs the service.

Check [here](https://docs.start9.com/contributing/services/overview.html) for a detailed overview of package components.

### Service Wrapper Repo and Submodules

See [here](https://docs.start9.com/contributing/services/wrapper.html) for how to structure your service wrapper's git repository.

Git submodules allow the use of another project while in the working project directory.  In this case, you can use an existing app's git repo in order to source its code into your service wrapper.

Simply run:
```git submodule add <link_to_source_project>```

## Example - Hello World

Okay, let's actually package a service!  For this example, we're going to use an example service [Hello World](https://github.com/Start9Labs/hello-world).  This repository can also be used as a template to quickly get started with your service.  This will give a good overview of service packaging, but obviously your app will be different.  This will assume a Linux development environment with all the recommended dependencies listed above.  To get started quickly, we'll use Start9's wrapper template.

### Clone the Template Repo and Edit the Manifest

1. Clone and rename the repo (or alternatively, use the template generation button found on the github [repo](https://github.com/Start9Labs/hello-world-wrapper))
```
git clone https://github.com/Start9Labs/hello-world-wrapper
cd hello-world-wrapper
```

2. Edit the `README.md` to explain what the service is, what dependencies are required, build/install/contribute instructions, and any other information you'd like.

3. Edit the `manifest` file.  This must be in `.json`, `.toml`, or `.yaml` format and in `kebab-case` style.  You can see descriptions of each key (and some notes) in our 'Hello World' example `manifest.yaml` below:

```
# v0.3.0 and up Manifest example written in .yaml (.toml and .json are also acceptable)

id: hello-world
title: "Hello World"
version: 0.2.0 # Service version
release-notes: "Upgrade to EmbassyOS 2.16.0 and then to v0.3.0"
license: mit
wrapper-repo: "https://github.com/Start9Labs/hello-world-wrapper"
upstream-repo: "https://github.com/Start9Labs/hello-world-wrapper" # There is no upstream repo in this example
support-site: "https://docs.start9.com/"
marketing-site: "https://start9.com/"
build: ["make"] # Series of commands to build into an s9pk, in this case we are using make and all the build commands are in the makefile
min-os-version: "0.3.0" # Minimum required version of EmbassyOS
description:
  short: Example service
  long: |
    Hello World is a simple example of a service wrapper that launches a web interface to say hello and nothing more.
assets:
  license: LICENSE # default = LICENSE.md
  icon: icon.png # default = icon.png
  instructions: docs/instructions.md # default = INSTRUCTIONS.md
  docker-images: image.tar # default = image.tar
main:
  type: docker
  image: main
  entrypoint: "/usr/local/bin/docker_entrypoint.sh"
  args: []
  mounts: {} # Specifies where to put volumes, if there are any.  Empty in this example
health-checks: {} # Health check config would go here
config: ~ # Configuration options, none for hello-world, but see below example for format:
#    get:
#     type: docker
#     image: compat
#     entrypoint: compat
#     args: 
#       - "config"
#       - "get"
#       - "/root/.hello-world/start9/config.yaml"
#       - "/mnt/assets/config_spec.yaml"
#     mounts:
#       compat: "/mnt/assets"
#       main: "/root"
#     io-format: yaml
#   set:
#     type: docker
#     image: compat
#     entrypoint: compat
#     args:
#       - "config"
#       - "set"
#       - "/root/.hello-world/start9/config.yaml"
#     mounts:
#       main: "/root"
#     io-format: yaml
dependencies: {} # Service pre-requisites, none for hello-world, but see below example (which would make BTC Proxy a dependency) for format:
  # btc-rpc-proxy:
  #   version: ">=0.3.2.1 <0.4.0"
  #   recommended: true
  #   critical: false
  #   optional: Can alternatively configure an external bitcoin node.
  #   description: Used to fetch validated blocks.
  #   config:
  #     check: 
  #       type: docker
  #       image: compat
  #       system: true
  #       # the compat image will contain a tool to check the config rules against the specified dependency
  #       entrypoint: compat
  #       args:
  #         - "dependency"
  #         - "check"
  #         - "/mnt/assets/btc-rpc-proxy.rules.yaml"
  #       mounts:
  #         compat: "/mnt/assets"  
  #     auto-configure:
  #       type: docker
  #       image: compat
  #       # if true, the denoted image is prebuilt and comes stock with EOS
  #       # uncommon: if you want something not prebuilt with EOS, you can bundle multiple docker images into the `image.tar` during the `make` build process
  #       system: true
  #       entrypoint: compat
  #       args:
  #         - "dependency"
  #         - "auto-configure"
  #         - "/mnt/assets/btc-rpc-proxy.rules.yaml"
  #       mounts:
  #         compat: "/mnt/assets"  
volumes: # this is the image where data will go from 0.2.x
  main:
    type: data # this is the image where files from the project asset directory will go
  compat:
    type: assets # this is a pointer volume, where the image is specified in `<pointer-id>.volume-id` and the mount point is specificed in `main.mounts.<pointer-id>`
interfaces:
  main:
    name: Network Interface
    description: Specifies the interface to listen on for HTTP connections.
    tor-config:
      port-mapping:
        80: "80"
    lan-config:
      80:
        ssl: false
        mapping: 80
    ui: true
    protocols:
      - tcp
      - http
alerts: {}
backup:
  create:
    type: docker
    image: compat # default backup process of the compat docker image is duplicity - EOS will have access to the image defined here
    system: true 
    entrypoint: compat # command to run the backup executable, in this case, duplicity
    args: # arguments to pass into the entrypoint, in this case duplicity - in this example, the full command run will be: `duplicity hello-world file:///mnt/backup /root`
      - duplicity
      - hello-world
      - /mnt/backup
      - /root
    mounts:
      # BACKUP is the default volume that is used for backups.  This is whatever backup drive is mounted to the defice, or a network filesystem.  
      # The value here donates where the data mount point will be.  Backup drive is mounted to this location, which contains previous backups.
      BACKUP: "/mnt/backup" 
      main: "/root"
  restore:
    type: docker
    image: compat
    system: true
    entrypoint: compat
    args:
      - duplicity
      - hello-world
      - /root
      - /mnt/backup
    mounts:
      # See above comments under `backup: -> mounts:`
      BACKUP: "/mnt/backup"
      main: "/root"
actions: {} # Commands that can be issued from the UI.  None for hello-world, but see below example (resetting a root user) for format:
  # reset-root-user:
  #   name: Reset Root User
  #   description: Resets your root user (the first user) to username "admin" and a random password; restores any lost admin privileges.
  #   warning: This will invalidate existing sessions and password managers if you have them set up.
  #   allowed-statuses:
  #     - stopped
  #   implementation:
  #     type: docker
  #     image: main
  #     system: true
  #     entrypoint: docker_entrypoint.sh
  #     args: ["reset-root-user"]
  #     mounts:
  #       main: "/root"
```

Note the `dependencies` and `volumes` sections, which may access another service, for example, File Browser, such that files stored on a user's Embassy can be accessed in your service.

For details on all the different possible dependency, type, and subtype definitions available for the `manifest` file, please see [here](https://docs.start9.com/contributing/services/manifest.html).

### Edit the Dockerfile and Entrypoint

Next, it's time to edit the `Dockerfile`.  This defines how to build the image for the package by declaring the environment, building stages, and mounting the package to the volume specified in the `Manifest`.

1. We start by importing a base image, in this case Alpine, as recommended.

`FROM arm64v8/alpine:3.12`

2. Next we issue some commands, which in this example simply updates repositories, installs required software, and finally creates a directory for nginx.

```
RUN apk update
RUN apk add tini

RUN mkdir /run/nginx
```

3. Next we will add the cross-compiled binary of `hello-world` to `/usr/local/bin/` and add the `docker_entrypoint.sh` file from the repository.  Then we set permissions for `docker_entrypoint.sh`.

```
ADD ./hello-world/target/aarch64-unknown-linux-musl/release/hello-world /usr/local/bin/hello-world
ADD ./docker_entrypoint.sh /usr/local/bin/docker_entrypoint.sh
RUN chmod a+x /usr/local/bin/docker_entrypoint.sh
```

4. Next we set a working directory, expose a port, and set the location of the entrypoint.

```
WORKDIR /root

EXPOSE 80

ENTRYPOINT ["/usr/local/bin/docker_entrypoint.sh"]
```

5. Great, let's take a look at our final Embassy Pages `Dockerfile`:

```
FROM arm64v8/alpine:3.12

RUN apk update
RUN apk add tini

ADD ./hello-world/target/aarch64-unknown-linux-musl/release/hello-world /usr/local/bin/hello-world
ADD ./docker_entrypoint.sh /usr/local/bin/docker_entrypoint.sh
RUN chmod a+x /usr/local/bin/docker_entrypoint.sh

WORKDIR /root

EXPOSE 80

ENTRYPOINT ["/usr/local/bin/docker_entrypoint.sh"]
```

6. Okay, let's move on to our `docker_entrypoint.sh` file.  This is a script that defines what to do when the service starts.  It will need to complete any environment setup (such as folder substructure), sets any environment variables, and executes the run command.  If you have built a `configurator`, it will also execute here.  Let's take a look at our (extremely basic) Hello World example:

```
#!/bin/sh

export HOST_IP=$(ip -4 route list match 0/0 | awk '{print $3}')

exec tini hello-world
```

7. We've defined the file, exported the IP address, and run the program.

For a more detailed `docker_entrypoint.sh`, please check out the [filebrowser-wrapper](https://github.com/Start9Labs/filebrowser-wrapper/blob/master/docker_entrypoint.sh).  Additional details on the `Dockerfile` and `entrypoint` can be found [here](https://docs.start9.com/contributing/services/docker.html).

### Makefile (Optional)

Here, we will create a `Makefile`, which is optional, but recommended as it outlines the build and streamlines additional developer contributions.  Alternatively, you could use any other build orchestration tool, such as `nix`, `bash`, `python`, `perl`, `ruby`, etc instead of `make`.

Our example `Makefile` is agin fairly simple for Hello World.  Let's take a look:

```
ASSETS := $(shell yq e '.assets.[].src' manifest.yaml)
ASSET_PATHS := $(addprefix assets/,$(ASSETS))
VERSION := $(shell toml get hello-world/Cargo.toml package.version)
HELLO_WORLD_SRC := $(shell find ./hello-world/src) hello-world/Cargo.toml hello-world/Cargo.lock
S9PK_PATH=$(shell find . -name hello-world.s9pk -print)

.DELETE_ON_ERROR:

all: verify

verify: hello-world.s9pk $(S9PK_PATH)
		embassy-sdk verify $(S9PK_PATH)

# embassy-sdk pack errors come from here, check your manifest, config, instructions, and icon
hello-world.s9pk: manifest.yaml assets/compat/config_spec.yaml config_rules.yaml image.tar docs/instructions.md $(ASSET_PATHS)
		embassy-sdk pack

image.tar: Dockerfile docker_entrypoint.sh hello-world/target/aarch64-unknown-linux-musl/release/hello-world
		DOCKER_CLI_EXPERIMENTAL=enabled docker buildx build --tag start9/hello-world --platform=linux/arm64 -o type=docker,dest=image.tar .

hello-world/target/aarch64-unknown-linux-musl/release/hello-world: $(HELLO_WORLD_SRC)
		docker run --rm -it -v ~/.cargo/registry:/root/.cargo/registry -v "$(shell pwd)"/hello-world:/home/rust/src start9/rust-musl-cross:aarch64-musl cargo +beta build --release
		docker run --rm -it -v ~/.cargo/registry:/root/.cargo/registry -v "$(shell pwd)"/hello-world:/home/rust/src start9/rust-musl-cross:aarch64-musl musl-strip target/aarch64-unknown-linux-musl/release/hello-world

manifest.yaml: hello-world/Cargo.toml
		yq e -i '.version = $(VERSION)' manifest.yaml

```
1. The first 5 lines set environment variables.

2. The next line simply removes the progress of a `make` process if it fails.
`.DELETE_ON_ERROR:`

3. The `all` step is run when the `make` command is issued.  This attempts the `verify` step, which requires that the `hello-world.s9pk` must first be built, which first requires the `image.tar`, and so on.  Meaning each step essentially requires the next .

4. So the `.s9pk` is created with the `embassy-sdk pack` command, supplied with the `manifest`, `config_spec`, previously created `image.tar`, and `instructions.md`.  Your project may likely also contain a `config_rules` file.  Some of these files we have not yet edited, but that will come shortly.

5. The `image.tar` is built below this, the cross-compiled `hello-world` source code, and `manifest` at the bottom.

For more details on creating a `Makefile` for your project, please check [here](https://docs.start9.com/contributing/services/makefile.html).

### Service Config Specification and Rules

Most self-hosted packages require a configuration.  With EmbassyOS, these config options are provided to the user in a friendly GUI, and invalid configs are not permitted.  This allows the user to manage their software without a lot of technical skill, and minimal risk of borking their software.  Two files are created in this process:

`config_spec.yaml` for specifying all the config options your package depends on to run

`config_rules.yaml` for defining the ruleset that defines dependencies between config variables

These are stored in `assets/compat/` for 0.2.x compatibility, and in `/assets/` for anything built for v0.3.0 and up (almost certainly what you're doing).  These files contain a detailed mapping of configuration options with acceptable values, defaults, and relational rule-sets.  Hello World has no configuration, as you can see [here](https://github.com/Start9Labs/hello-world-wrapper/blob/0.3.0/assets/compat/config_spec.yaml).  Instead, let's take a look at our `config_spec` for Embassy Pages, which actually has some config options:

```
homepage:
  name: Homepage
  description: The page that will be displayed when your Embassy Pages .onion address is visited. Since this page is technically publicly accessible, you can choose to which type of page to display.
  type: union
  default: welcome
  tag:
    id: type
    name: Type
    variant-names:
      welcome: Welcome
      index: Subdomain Index
      filebrowser: Web Page
      redirect: Redirect
      fuck-off: Fuck Off
  variants:
    welcome: {}
    index: {}
    filebrowser:
      directory:
        type: string
        name: Directory Path
        description: The path to the directory in File Browser that contains the static files of your website. For example, a value of "websites/resume_site" would tell Embassy Pages to look for that directory in File Browser.
        pattern: "^(\\.|[a-zA-Z0-9_ -][a-zA-Z0-9_ .-]*|([a-zA-Z0-9_ .-][a-zA-Z0-9_ -]+\\.*)+)(/[a-zA-Z0-9_ -][a-zA-Z0-9_ .-]*|/([a-zA-Z0-9_ .-][a-zA-Z0-9_ -]+\\.*)+)*/?$"
        pattern-description: Must be a valid relative file path
        nullable: false
    redirect:
      target:
        type: string
        name: Target Subdomain
        description: The name of the subdomain to redirect users to. This must be a valid subdomain site within your Embassy Pages. 
        pattern: '^[a-z-]+$'
        pattern-description: May contain only lowercase characters and hyphens.
        nullable: false
    fuck-off: {}
subdomains:
  type: list
  name: Subdomains
  description: The websites you want to serve.
  default: []
  range: '[0, *)'
  subtype: object
  spec:
    unique-by: name
    display-as: "{{name}}"
    spec:
      name:
        type: string
        nullable: false
        name: Subdomain name
        description: The subdomain of your Embassy Pages .onion address to host the website on. For example, a value of "me" would produce a website hosted at http://me.myaddress.onion.
        pattern: "^[a-z-]+$"
        pattern-description: "May contain only lowercase characters and hyphens"
      settings:
        type: union
        name: Settings
        description: The desired behavior you want to occur when the subdomain is visited. You can either redirect to another subdomain, or load a web page from File Browser.
        default: filebrowser
        tag:
          id: type
          name: Type
          variant-names:
            filebrowser: Web Page
            redirect: Redirect
        variants:
          filebrowser:
            directory:
              type: string
              name: Directory Path
              description: The path to the directory in File Browser that contains the static files of your website. For example, a value of "websites/resume_site" would tell Embassy Pages to look for that directory in File Browser.
              pattern: "^(\\.|[a-zA-Z0-9_ -][a-zA-Z0-9_ .-]*|([a-zA-Z0-9_ .-][a-zA-Z0-9_ -]+\\.*)+)(/[a-zA-Z0-9_ -][a-zA-Z0-9_ .-]*|/([a-zA-Z0-9_ .-][a-zA-Z0-9_ -]+\\.*)+)*/?$"
              pattern-description: Must be a valid relative file path
              nullable: false
          redirect:
            target:
              type: string
              name: Target Subdomain
              description: The subdomain of your Embassy Pages .onion address to redirect to. This should be the name of another subdomain on Embassy Pages. Leave empty to redirect to the homepage.
              pattern: '^[a-z-]+$'
              pattern-description: May contain only lowercase characters and hyphens.
              nullable: false
```
We essentially have 2 config options (homepage and subdomains), with all of their specifications nested below them.  Looking at the homepage, it contains a `union` type, which is a necessary dependency, which can be of 5 variants (welcome, index, filebrowser, redirect, or fuck-off).  The below images show how this is displayed in the UI.

***IMAGE PLACEHODLER***

***IMAGE PLACEHODLER***

For all the possible types, please check our detailed documentation [here](https://docs.start9.com/contributing/services/config.html#types).

In our example, there is *no need* for a `config_rules` file.  This is because there is not a rule-set required to define dependencies between config variables.  An example of when this would be required would be the following code, from the [LND wrapper](https://github.com/Start9Labs/lnd-wrapper/blob/master/config_rules.yaml):

```
---
- rule: '!(max-chan-size?) OR !(min-chan-size?) OR (#max-chan-size > #min-chan-size)'
  description: "Maximum Channel Size must exceed Minimum Channel Size"
```

Here we see that a Maximum Channel Size MUST be one of 3 possible options in order to be a valid config.

### Properties

Next we need to create the Properties section for our package, to display any relevant info.  Te result of this step is a `stats.yaml` file, which is only populated at runtime.

***THE STATS.YAML IS APPARENTLY BEING DEPRECATED, THIS SECTION NEEDS COMMENT***

### Instructions

Instructions are the basic directions or any particular details that you would like to convey to the user to help get them on their way.  Each wrapper repo should contain a `docs` directory which can include anything you'd like, but specifically if you include an `instructions.md` file, formatted in Markdown language, it will be displayed simply for the user as shown below.

***PLACEHOLDER FOR IMAGE***

You can find the `instructions.md` file for Embassy Pages [here](https://github.com/Start9Labs/embassy-pages-wrapper/tree/master/docs) if you are interested.

### Backups

Everything in the root folder of the mounted system directory will be stored in an EOS backup.  If you want to ignore any particular files for backup, you can create a `.backupignore` file and add the relative paths of any directories you would like ignored.  

## Submission Process

When you have built and tested your project for EmbassyOS, please send Start9 a submission with the project repository to dev@start9labs.com. After being reviewed for security and compatibility, the service will be deployed to the marketplace and available for all EmbassyOS users to download.

If you are deploying to an alternative marketplace, please shout it out in our community channels!