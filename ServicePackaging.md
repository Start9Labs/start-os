# EmbassyOS Service Packaging Guide

Welcome!  The following guide will use an example to demonstrate how to package up a service for EmbassyOS.  This essentially means you will take an existing app (or one you have written yourself), and wrap it up into an .s9pk so that it can be added to an Embassy Marketplace.

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

Simply, the package is made up of the following parts:
    1. Image - Each service is running in a Docker image.  Best results will come from an arm based linux; [Alpine](https://www.alpinelinux.org/) is highly recommended.
    2. Volume - Each service gets a volume, allocated by EOS.  The volume directory (for seeding data into the volume) is typically: /root/volumes/<service-id>
    3. Dependencies - Rules and requirements of your service, which appear as simple UI elements, such as inputs, toggles, and drop-downs.  These are enforced by validations and clear user instructions.  EmbassyOS has a unique and powerful system for managing dependencies which allows anyone to have the power of systems administrators without the advanced skillset.
    4. Manifest - Describes the service and its requirements.  This is for the marketplace listing, install considerations, license, donation address, and dependency requirements, and additional info.
    5. Config - EOS makes a service's configuration available to the user in the GUI and must be valid regardless of user skill.
    6. .s9pk Bundle - The config and manifest files get bundled into a .s9pk package.  This is the file a user downloads from the Marketplace, at which point EOS un-packages and installs the service.

Check [here](https://docs.start9.com/contributing/services/overview.html) for a detailed overview of package components.

## Service Wrapper Repo

See [here](https://docs.start9.com/contributing/services/wrapper.html) for the advised structure for your service wrapper's git repository.

## Example - Embassy Pages

Okay, let's actually package a service!  For this example, we're going to use the existing EOS service Embassy Pages.  This will give a good overview of service packaging, but obviously your app will be different.

### Clone Start9's Service Wrapper Template



## Submission Process

When you have built and tested your project for EmbassyOS, please send Start9 a submission with the project repository to dev@start9labs.com. After being reviewed for security and compatibility, the service will be deployed to the marketplace and available for all EmbassyOS users to download.

If you are deploying to an alternative marketplace, please shout it out in our community channels!