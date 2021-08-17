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

### Makefile (Optional)

Here, we will create a Makefile, which is optional, but recommended as it outlines the build and streamlines additional developer contributions.  Alternatively, you could use the `nix` specification.

1. 

## Submission Process

When you have built and tested your project for EmbassyOS, please send Start9 a submission with the project repository to dev@start9labs.com. After being reviewed for security and compatibility, the service will be deployed to the marketplace and available for all EmbassyOS users to download.

If you are deploying to an alternative marketplace, please shout it out in our community channels!