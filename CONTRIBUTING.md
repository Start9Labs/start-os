<!-- omit in toc -->
# Contributing to Embassy OS

First off, thanks for taking the time to contribute! ❤️

All types of contributions are encouraged and valued. See the [Table of Contents](#table-of-contents) for different ways to help and details about how this project handles them. Please make sure to read the relevant section before making your contribution. It will make it a lot easier for us maintainers and smooth out the experience for all involved. The community looks forward to your contributions. 🎉

> And if you like the project, but just don't have time to contribute, that's fine. There are other easy ways to support the project and show your appreciation, which we would also be very happy about:
> - Star the project
> - Tweet about it
> - Refer this project in your project's readme
> - Mention the project at local meetups and tell your friends/colleagues
> - Buy an [Embassy](https://start9labs.com)

<!-- omit in toc -->
## Table of Contents

- [I Have a Question](#i-have-a-question)
- [I Want To Contribute](#i-want-to-contribute)
  - [Reporting Bugs](#reporting-bugs)
  - [Suggesting Enhancements](#suggesting-enhancements)
  - [Your First Code Contribution](#your-first-code-contribution)
    - [Setting Up Your Development Environment](#setting-up-your-development-environment)
    - [Building The Image](#building-the-image)
  - [Improving The Documentation](#improving-the-documentation)
- [Styleguides](#styleguides)
  - [Formatting](#formatting)
  - [Atomic Commits](#atomic-commits)
  - [Commit Messages](#commit-messages)
  - [Pull Requests](#pull-requests)
  - [Rebasing Changes](#rebasing-changes)
- [Join The Discussion](#join-the-discussion)
- [Join The Project Team](#join-the-project-team)



## I Have a Question

> If you want to ask a question, we assume that you have read the available [Documentation](https://docs.start9labs.com).

Before you ask a question, it is best to search for existing [Issues](https://github.com/Start9Labs/embassy-os/issues) that might help you. In case you have found a suitable issue and still need clarification, you can write your question in this issue. It is also advisable to search the internet for answers first.

If you then still feel the need to ask a question and need clarification, we recommend the following:

- Open an [Issue](https://github.com/Start9Labs/embassy-os/issues/new).
- Provide as much context as you can about what you're running into.
- Provide project and platform versions, depending on what seems relevant.

We will then take care of the issue as soon as possible.

<!--
You might want to create a separate issue tag for questions and include it in this description. People should then tag their issues accordingly.

Depending on how large the project is, you may want to outsource the questioning, e.g. to Stack Overflow or Gitter. You may add additional contact and information possibilities:
- IRC
- Slack
- Gitter
- Stack Overflow tag
- Blog
- FAQ
- Roadmap
- E-Mail List
- Forum
-->

## I Want To Contribute

> ### Legal Notice <!-- omit in toc -->
> When contributing to this project, you must agree that you have authored 100% of the content, that you have the necessary rights to the content and that the content you contribute may be provided under the project license.

### Reporting Bugs

<!-- omit in toc -->
#### Before Submitting a Bug Report

A good bug report shouldn't leave others needing to chase you up for more information. Therefore, we ask you to investigate carefully, collect information and describe the issue in detail in your report. Please complete the following steps in advance to help us fix any potential bug as fast as possible.

- Make sure that you are using the latest version.
- Determine if your bug is really a bug and not an error on your side e.g. using incompatible environment components/versions (Make sure that you have read the [documentation](https://docs.start9.com). If you are looking for support, you might want to check [this section](#i-have-a-question)).
- To see if other users have experienced (and potentially already solved) the same issue you are having, check if there is not already a bug report existing for your bug or error in the [bug tracker](https://github.com/Start9Labs/embassy-os/issues?q=label%3Abug).
- Also make sure to search the internet (including Stack Overflow) to see if users outside of the GitHub community have discussed the issue.
- Collect information about the bug:
  - Stack trace (Traceback)
  - Client OS, Platform and Version (Windows/Linux/macOS/iOS/Android, Firefox/Tor Browser/Consulate)
  - Version of the interpreter, compiler, SDK, runtime environment, package manager, depending on what seems relevant.
  - Possibly your input and the output
  - Can you reliably reproduce the issue? And can you also reproduce it with older versions?

<!-- omit in toc -->
#### How Do I Submit a Good Bug Report?

> You must never report security related issues, vulnerabilities or bugs to the issue tracker, or elsewhere in public. Instead sensitive bugs must be sent by email to <security@start9labs.com>.
<!-- You may add a PGP key to allow the messages to be sent encrypted as well. -->

We use GitHub issues to track bugs and errors. If you run into an issue with the project:

- Open an [Issue](https://github.com/Start9Labs/embassy-os/issues/new/choose) selecting the appropriate type. 
- Explain the behavior you would expect and the actual behavior.
- Please provide as much context as possible and describe the *reproduction steps* that someone else can follow to recreate the issue on their own. This usually includes your code. For good bug reports you should isolate the problem and create a reduced test case.
- Provide the information you collected in the previous section.

Once it's filed:

- The project team will label the issue accordingly.
- A team member will try to reproduce the issue with your provided steps. If there are no reproduction steps or no obvious way to reproduce the issue, the team will ask you for those steps and mark the issue as `Question`. Bugs with the `Question` tag will not be addressed until they are answered.
- If the team is able to reproduce the issue, it will be marked a scoping level tag, as well as possibly other tags (such as `Security`), and the issue will be left to be [implemented by someone](#your-first-code-contribution).

<!-- You might want to create an issue template for bugs and errors that can be used as a guide and that defines the structure of the information to be included. If you do so, reference it here in the description. -->


### Suggesting Enhancements

This section guides you through submitting an enhancement suggestion for Embassy OS, **including completely new features and minor improvements to existing functionality**. Following these guidelines will help maintainers and the community to understand your suggestion and find related suggestions.

<!-- omit in toc -->
#### Before Submitting an Enhancement

- Make sure that you are using the latest version.
- Read the [documentation](https://docs.start9.com) carefully and find out if the functionality is already covered, maybe by an individual configuration.
- Perform a [search](https://github.com/Start9Labs/embassy-os/issues) to see if the enhancement has already been suggested. If it has, add a comment to the existing issue instead of opening a new one.
- Find out whether your idea fits with the scope and aims of the project. It's up to you to make a strong case to convince the project's developers of the merits of this feature. Keep in mind that we want features that will be useful to the majority of our users and not just a small subset. If you're just targeting a minority of users, consider writing an add-on/plugin library.

<!-- omit in toc -->
#### How Do I Submit a Good Enhancement Suggestion?

Enhancement suggestions are tracked as [GitHub issues](https://github.com/Start9Labs/embassy-os/issues).

- Use a **clear and descriptive title** for the issue to identify the suggestion.
- Provide a **step-by-step description of the suggested enhancement** in as many details as possible.
- **Describe the current behavior** and **explain which behavior you expected to see instead** and why. At this point you can also tell which alternatives do not work for you.
- You may want to **include screenshots and animated GIFs** which help you demonstrate the steps or point out the part which the suggestion is related to. You can use [this tool](https://www.cockos.com/licecap/) to record GIFs on macOS and Windows, and [this tool](https://github.com/colinkeenan/silentcast) or [this tool](https://github.com/GNOME/byzanz) on Linux. <!-- this should only be included if the project has a GUI -->
- **Explain why this enhancement would be useful** to most Embassy OS users. You may also want to point out the other projects that solved it better and which could serve as inspiration.

<!-- You might want to create an issue template for enhancement suggestions that can be used as a guide and that defines the structure of the information to be included. If you do so, reference it here in the description. -->

### Project Structure
EmbassyOS is composed of the following components. Please visit the README for each component to understand the dependency requirements and installation instructions.
- [`ui`](ui/README.md) (Typescript Ionic Angular) is the code that is deployed to the browser to provide the user interface for EmbassyOS.
- [`backend`] (backend/README.md) (Rust) is a command line utility, daemon, and software development kit that sets up and manages services and their environments, provides the interface for the ui, manages system state, and provides utilities for packaging services for EmbassyOS. 
- `patch-db` - A diff based data store that is used to synchronize data between the front and backend
  - Notably, `patch-db` has a [client](patch-db/client/README.md) with its own dependency and installation requirements. 
- `rpc-toolkit` - A library for generating an rpc server with cli bindings from Rust functions
- `system-images` - (Docker, Rust) A suite of utility Docker images that are preloaded with EmbassyOS to assist with functions relating to services (eg. configuration, backups, health checks)
- [`setup-wizard`] (ui/README.md)- Code for the user interface that is displayed during the setup and recovery process for EmbassyOS.
- [`diagnostic-ui`] (diagnostic-ui/README.md) - Code for the user interface that is displayed when something has gone wrong with starting up EmbassyOS, which provides helpful debugging tools.
### Your First Code Contribution

#### Setting up your development environment

First, clone the EmbassyOS repository and from the project root, pull in the submodules for dependent libraries.

```
git clone https://github.com/Start9Labs/embassy-os.git
git submodule update --init --recursive
```

Depending on which component of the ecosystem you are interested in contributing to, follow the installation requirements listed in that component's README (linked [above](#project-structure))

#### Building The Image
This step is for setting up an environment in which to test your code changes if you do not yet have a EmbassyOS.

- Requirements
  - `ext4fs` (available if running on the Linux kernel)
  - [Docker](https://docs.docker.com/get-docker/)
  - GNU Make
- Building
  - see setup instructions [here](build/README.md)
  - run `make` from the project root

### Improving The Documentation
You can find the repository for Start9's documentation [here](https://github.com/Start9Labs/documentation). If there is something you would like to see added, let us know, or create an issue yourself. Welcome are contributions for lacking or incorrect information, broken links, requested additions, or general style improvements. 

Contributions in the form of setup guides for integrations with external applications are highly encouraged. If you struggled through a process and would like to share your steps with others, check out the docs for each [service](https://github.com/Start9Labs/documentation/blob/master/source/user-manuals/available-services/index.rst) we support. The wrapper repos contain sections for adding integration guides, such as this [one](https://github.com/Start9Labs/bitcoind-wrapper/tree/master/docs). These not only help out others in the community, but inform how we can create a more seamless and intuitive experience.

## Styleguides
### Formatting
Each component of EmbassyOS contains its own style guide. Code must be formatted with the formatter designated for each component. These are outlined within each component folder's README.

### Atomic Commits
Commits [should be atomic](https://en.wikipedia.org/wiki/Atomic_commit#Atomic_commit_convention) and diffs should be easy to read.
Do not mix any formatting fixes or code moves with actual code changes.

### Commit Messages
If a commit touches only 1 component, prefix the message with the affected component. i.e. `backend: update to tokio v0.3`.

### Pull Requests
The body of a pull request should contain sufficient description of what the changes do, as well as a justification.
You should include references to any relevant [issues](https://github.com/Start9Labs/embassy-os/issues).

### Rebasing Changes
When a pull request conflicts with the target branch, you may be asked to rebase it on top of the current target branch. The git rebase command will take care of rebuilding your commits on top of the new base.

This project aims to have a clean git history, where code changes are only made in non-merge commits. This simplifies auditability because merge commits can be assumed to not contain arbitrary code changes.

## Join The Discussion
Current or aspiring contributors? Join our community developer [Matrix channel](https://matrix.to/#/#community-dev:matrix.start9labs.com).

Just interested in or using the project? Join our community [Telegram](https://t.me/start9_labs) or [Matrix](https://matrix.to/#/#community:matrix.start9labs.com).

## Join The Project Team
Interested in becoming a part of the Start9 Labs team? Send an email to <jobs@start9labs.com>

<!-- omit in toc -->
## Attribution
This guide is based on the **contributing-gen**. [Make your own](https://github.com/bttger/contributing-gen)!
