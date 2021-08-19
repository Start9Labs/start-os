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
    7. [rust-musl-cross](***PLACEHOLDER FOR NEW MUSL-CROSS REPO***) (If using an Alpine image, which is highly recommended)

## Overview

### Components

Simply, the package is made up of the following parts:
    1. Image - Each service is running in a Docker image.  Best results will come from an arm based linux; [Alpine](https://www.alpinelinux.org/) is highly recommended.
    2. Volume - Each service gets a volume, allocated by EOS.  The volume directory (for seeding data into the volume) is typically: /root/volumes/<service-id>
    3. Dependencies - Rules and requirements of your service, which appear as simple UI elements, such as inputs, toggles, and drop-downs.  These are enforced by validations and clear user instructions.  EmbassyOS has a unique and powerful system for managing dependencies which allows anyone to have the power of systems administrators without the advanced skillset.
    4. Manifest - Describes the service and its requirements.  This is for the marketplace listing, install considerations, license, donation address, and dependency requirements, and additional info.
    5. Config - EOS makes a service's configuration available to the user in the GUI and must be valid regardless of user skill.
    6. .s9pk Bundle - The config and manifest files get bundled into a .s9pk package.  This is the file a user downloads from the Marketplace, at which point EOS un-packages and installs the service.

Check [here](https://docs.start9.com/contributing/services/overview.html) for a detailed overview of package components.

### Service Wrapper Repo and Submodules

See [here](https://docs.start9.com/contributing/services/wrapper.html) for how to structure your service wrapper's git repository.

Git submodules allow the use of another project while in the working project directory.  In this case, you can use an existing app's git repo in order to source its code into your service wrapper.

Simply run:
```git submodule add <link_to_source_project>```

## Example - Embassy Pages

Okay, let's actually package a service!  For this example, we're going to use the existing EOS service [Embassy Pages](https://github.com/Start9Labs/embassy-pages-wrapper).  This will give a good overview of service packaging, but obviously your app will be different.  This will assume a Linux development environment with all the recommended dependencies listed above.  To get started quickly, we'll use Start9's wrapper template.

### Clone the Template Repo and Edit the Manifest

1. Clone and rename the repo (or alternatively, use the template generation button found on the github [repo](https://github.com/Start9Labs/hello-world-wrapper))
```
git clone https://github.com/Start9Labs/hello-world-wrapper
mv hello-world-wrapper embassy-pages-wrapper && cd embassy-pages-wrapper
```

2. Edit the `README.md` to explain what the service is, what dependencies are required, and build/install instructions.

3. Edit the `manifest` file.  This must be in `.json`, `.toml`, or `.yaml` format and in `kebab-case` style.  You can see our Embassy Pages example `manifest.toml` below:

```
id = "embassy-pages"
title = "Embassy Pages"
version = "0.1.3"
release-notes = "Upgrade to EmbassyOS v0.3.0"
license = "nginx"
wrapper-repo = "https://github.com/Start9Labs/embassy-pages-wrapper"
upstream-repo = "http://hg.nginx.org/nginx/"
build = ["make"]
min-os-version = "0.3.0"

[description]
long = "Embassy Pages is a simple web server that uses directories inside File Browser to serve Tor websites."
short = "Create Tor websites, hosted on your Embassy."

[assets]
license = "LICENSE"
icon = "icon.png"
instructions = "instructions.md"
docker-images = "image.tar"

[main]
type = "docker"
image = "main"
entrypoint = "/usr/local/bin/docker_entrypoint.sh"
args = []
mounts = { filebrowser = "/mnt/filebrowser" }
io-format = "yaml"

[health-checks]

[config.get]
type = "docker"
image = "compat"
system = true
entrypoint = "compat"
args = ["config", "get", "/root/start9/config.yaml", "/mnt/assets/config_spec.yaml"]
mounts = { compat = "/mnt/assets", main = "/root" }
io-format = "yaml"

[config.set]
type = "docker"
image = "compat"
system = true
entrypoint = "compat"
args = ["config", "set", "/root/start9/config.yaml"]
mounts = { main = "/root" }
io-format = "yaml"

[dependencies.filebrowser]
version = "^2.14.1.1"
description = "Used to upload files to serve."
critical = false
recommended = true

[volumes.main]
type = "data"

[volumes.filebrowser]
type = "pointer"
package-id = "filebrowser"
volume-id = "main"
path = "/"
readonly = true

[volumes.compat]
type = "assets"

[interfaces.main]
name = "Homepage"
description = "The homepage... (TODO)"
tor-config = { port-mapping = { "80" = "80" } }
ui = true
protocols = ["tcp", "http"]

[backup.create]
type = "docker"
image = "compat"
system = true
entrypoint = "true"
args = []
mounts = {}

[backup.restore]
type = "docker"
image = "compat"
system = true
entrypoint = "true"
args = []
mounts = {}
```

Note the `[dependencies]` and `[volumes]` sections, which include File Browser such that files stored on a user's Embassy can be accessed and hosted onto a Page.

For details on all the different possible dependency, type, and subtype definitions available for the `manifest` file, please see [here](https://docs.start9.com/contributing/services/manifest.html).

### Edit the Dockerfile and Entrypoint

Next, it's time to edit the `Dockerfile`.  This defines how to build the image for the package by declaring the environment, building stages, and mounting the package to the volume specified in the `Manifest`.

1. We start by importing a base image, in this case Alpine, as recommended.

`FROM alpine:3.13`

2. Next we issue some commands, which add a repository, update, install some required software, and finally create a directory for the nginx software.

```
RUN echo https://dl-cdn.alpinelinux.org/alpine/edge/community >> /etc/apk/repositories
RUN apk update
RUN apk add tini
RUN apk add bash nginx yq

RUN mkdir /run/nginx
```

3. Next we will add the `docker_entrypoint.sh` file and `www` directory from the repository as well as copying over some `.css` files for styling.  Then we set the permissions for `docker_entrypoint.sh`.

```
ADD ./docker_entrypoint.sh /usr/local/bin/docker_entrypoint.sh
ADD www /var/www
RUN cp /var/www/assets/main.css /var/www/fuck-off/main.css
RUN cp /var/www/assets/main.css /var/www/index/main.css
RUN cp /var/www/assets/main.css /var/www/welcome/main.css
RUN chmod a+x /usr/local/bin/docker_entrypoint.sh
```

4. Next we set a working directory, expose a port, and set the location of the entrypoint.

```
WORKDIR /root

EXPOSE 80

ENTRYPOINT ["/usr/local/bin/docker_entrypoint.sh"]
```

5. That's it!  Let's take a look at our final Embassy Pages `Dockerfile`:

```
FROM alpine:3.13

RUN echo https://dl-cdn.alpinelinux.org/alpine/edge/community >> /etc/apk/repositories
RUN apk update
RUN apk add tini
RUN apk add bash nginx yq

RUN mkdir /run/nginx

ADD ./docker_entrypoint.sh /usr/local/bin/docker_entrypoint.sh
ADD www /var/www
RUN cp /var/www/assets/main.css /var/www/fuck-off/main.css
RUN cp /var/www/assets/main.css /var/www/index/main.css
RUN cp /var/www/assets/main.css /var/www/welcome/main.css
RUN chmod a+x /usr/local/bin/docker_entrypoint.sh

WORKDIR /root

EXPOSE 80

ENTRYPOINT ["/usr/local/bin/docker_entrypoint.sh"]
```

Additional details on the `Dockerfile` can be found [here](https://docs.start9.com/contributing/services/docker.html).

### Makefile (Optional)

Here, we will create a `Makefile`, which is optional, but recommended as it outlines the build and streamlines additional developer contributions.  Alternatively, you could use the `nix` specification instead of `make`.

Our example `Makefile` is fairly simple for Embassy Pages.  Let's take a look:

```
.DELETE_ON_ERROR:

all: embassy-pages.s9pk

install: embassy-pages.s9pk
	appmgr install embassy-pages.s9pk

embassy-pages.s9pk: manifest.toml assets/compat/config_spec.yaml image.tar instructions.md
	embassy-sdk pack

image.tar: Dockerfile docker_entrypoint.sh
	DOCKER_CLI_EXPERIMENTAL=enabled docker buildx build --tag start9/embassy-pages/main:0.1.3 --platform=linux/arm/v7 -o type=docker,dest=image.tar .
```

1. The first line simply removes the progress of a `make` process if it fails.
`.DELETE_ON_ERROR:`

2. The `all` step is run when the `make` command is issued.  This means that the `embassy-pages.s9pk` must first be built, and so on.

3. Each step essentially requires the next, with the `image.tar` being required for all the above steps with the `docker` command, which is supplied with the `Dockerfile` and `docker_entrypoint.sh` files.  

4. Next the `.s9pk` is created with the `embassy-sdk pack` command, supplied with the `manifest`, `config_spec`, previously created `image.tar`, and `instructions.md`.  Your project may likely also contain a `config_rules` file.  Some of these files we have not yet edited, but that will come shortly.

For more details on creating a `Makefile` for your project, please check [here](https://docs.start9.com/contributing/services/makefile.html).

### Service Config Specification and Rules

Most self-hosted packages require a configuration.  With EmbassyOS, these config options are provided to the user in a friendly GUI, and invalid configs are not permitted.  This allows the user to manage their software without a lot of technical skill.  Two files are created in this process:

`config_spec.yaml` for specifying all the config options your package depends on to run

`config_rules.yaml` for defining the ruleset that defines dependencies between config variables

These files contain a detailed mapping of configuration options with acceptable values, defaults, and relational rule-sets.  Let's take a look at our `config_spec` for Embassy Pages:

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

In our example, there is *no need* for a `config_rules` file.  This is because there is not a rule-set required to define dependencies between config variables.  An example of when this would be required would be the following code, from the LND wrapper:

```
---
- rule: '!(max-chan-size?) OR !(min-chan-size?) OR (#max-chan-size > #min-chan-size)'
  description: "Maximum Channel Size must exceed Minimum Channel Size"
```

Here we see that a Maximum Channel Size MUST be one of 3 possible options in order to be a valid config.

### Properties




## Submission Process

When you have built and tested your project for EmbassyOS, please send Start9 a submission with the project repository to dev@start9labs.com. After being reviewed for security and compatibility, the service will be deployed to the marketplace and available for all EmbassyOS users to download.

If you are deploying to an alternative marketplace, please shout it out in our community channels!