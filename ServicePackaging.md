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

5. Great, let's take a look at our final Embassy Pages `Dockerfile`:

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

6. Okay, let's move on to our `docker_entrypoint.sh` file.  This is a bash script that defines what to do when the service starts.  It will need to complete any environment setup (such as folder substructure), sets any environment variables, and executes the run command.  Let's take a look at our Pages example:

```
#!/bin/bash

export HOST_IP=$(ip -4 route list match 0/0 | awk '{print $3}')

echo start9/public > .backupignore
echo start9/shared >> .backupignore

home_type=$(yq e '.homepage.type' start9/config.yaml)
subdomains=($(yq e '.subdomains.[].name' start9/config.yaml))
```

7. First we've defined the file as a bash, exported the IP address, chosen folders to ignore during backup, and set the homepage and subdomains types from our config.

***I'VE ABANDONED THIS SECTION AFTER DISCUSSION LEADING TO THE IDEA THAT THIS MAY NOT BE A GOOD EXAMPLE FILE***

```
read -r -d "" build_site_desc <<EOT
{
    "description": "Subdomain link for the site " + .,
    "masked": false,
    "copyable": true,
    "qr": false,
    "type": "string",
    "value": . + ".$TOR_ADDRESS"
}
EOT
yq e ".subdomains.[].name | {.: $build_site_desc}" start9/config.yaml > start9/stats.yaml
yq e -i '{"value": . }' start9/stats.yaml
yq e -i '.type="object"' start9/stats.yaml
yq e -i '.description="The available subdomains."' start9/stats.yaml
yq e -i '{"Subdomains": . }' start9/stats.yaml
yq e -i '{"data": .}' start9/stats.yaml
yq e -i '.version = 2' start9/stats.yaml
if [ ! -s start9/stats.yaml ] ; then
    rm start9/stats.yaml
fi

bucket_size=64
for subdomain in "${subdomains[@]}"; do
    suffix=".${TOR_ADDRESS}"
    len=$(( ${#suffix} + ${#subdomain} ))
    if [[ $len -ge $bucket_size ]]; then
        bucket_size=$(( $bucket_size * 2 ))
    fi
done

if [[ $home_type = "index" ]]; then
    if [ ${#subdomains} -ne 0 ]; then
        cp /var/www/index/index-prefix.html /var/www/index/index.html
        for subdomain in "${subdomains[@]}"; do
            echo "      <li><a target=\"_blank\" href=\"http://${subdomain}.${TOR_ADDRESS}\">${subdomain}</a></li>" >> /var/www/index/index.html
        done
        cat /var/www/index/index-suffix.html >> /var/www/index/index.html
    else
        cp /var/www/index/empty.html /var/www/index/index.html
    fi
fi

echo "server_names_hash_bucket_size ${bucket_size};" > /etc/nginx/conf.d/default.conf

if [[ $home_type = "redirect" ]]; then
    target=$(yq e '.homepage.target' start9/config.yaml)
    cat >> /etc/nginx/conf.d/default.conf <<EOT
server {
  listen 80;
  listen [::]:80;
  server_name ${TOR_ADDRESS};
  return 301 http://${target}.${TOR_ADDRESS}$request_uri;
}
EOT
elif [[ $home_type = "filebrowser" ]]; then
    directory=$(yq e '.homepage.directory' start9/config.yaml)
    cat >> /etc/nginx/conf.d/default.conf <<EOT
server {
  autoindex on;
  listen 80;
  listen [::]:80;
  server_name ${TOR_ADDRESS};
  root "/root/start9/public/filebrowser/${directory}";
}
EOT
else
    cat >> /etc/nginx/conf.d/default.conf <<EOT
server {
  listen 80;
  listen [::]:80;
  server_name ${TOR_ADDRESS};
  root "/var/www/${home_type}";
}
EOT
fi

for subdomain in "${subdomains[@]}"; do
    subdomain_type=$(yq e ".subdomains.[] | select(.name == \"$subdomain\") | .settings |.type" start9/config.yaml)
    if [[ $subdomain_type == "filebrowser" ]]; then
        directory="$(yq e ".subdomains.[] | select(.name == \"$subdomain\") | .settings | .directory" start9/config.yaml)"
        cat >> /etc/nginx/conf.d/default.conf <<EOT
server {
  autoindex on;
  listen 80;
  listen [::]:80;
  server_name ${subdomain}.${TOR_ADDRESS};
  root "/root/start9/public/filebrowser/${directory}";
}
EOT
    elif [ $subdomain_type = "redirect" ]; then
        if [ "$(yq e ".subdomains.[] | select(.name == \"$subdomain\") | .settings | .target == ~" start9/config.yaml)" = "true"]; then
            cat >> /etc/nginx/conf.d/default.conf <<EOT
server {
  listen 80;
  listen [::]:80;
  server_name ${subdomain}.${TOR_ADDRESS};
  return 301 http://${TOR_ADDRESS}$request_uri;
}
EOT
        else
            target="$(yq e ".subdomains.[] | select(.name == \"$subdomain\") | .settings | .target" start9/config.yaml)"
            cat >> /etc/nginx/conf.d/default.conf <<EOT
server {
  listen 80;
  listen [::]:80;
  server_name ${subdomain}.${TOR_ADDRESS};
  return 301 http://${target}.${TOR_ADDRESS}$request_uri;
}
EOT
        fi
    fi
done

exec tini -- nginx -g "daemon off;"
```

Additional details on the `Dockerfile` and `entrypoint` can be found [here](https://docs.start9.com/contributing/services/docker.html).

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