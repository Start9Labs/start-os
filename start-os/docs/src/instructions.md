# Instructions

Every service installed on StartOS ships with an **Instructions** page, accessible from the **Instructions** tab in the service details page sidebar. Instructions are written by the package's developer for you, the person actually running the service — they explain what the service is, walk you through getting it set up on StartOS, and tell you how to use it once it's running.

## When to Read Them

The first time you install a service is the most useful moment to open Instructions. Many services have a small number of setup steps that need to happen before they're truly usable — creating an admin password, importing a config, installing a dependency, registering a domain, and so on. Instructions are where the developer documents that path, in the order you should follow it.

After initial setup, Instructions remain a quick reference for day-to-day use: which interfaces the service exposes, which actions in the sidebar do what, and any limitations you should know about.

## What's In Them

A well-written Instructions page typically covers:

- **What the service is** — a short description of what it does.
- **What you get on StartOS** — how this packaged version looks and behaves on your server.
- **Getting set up** — numbered steps to take the service from a fresh install to a usable state.
- **Using the service** — the interfaces, actions, and tasks you'll work with.
- **Limitations** — anything that behaves differently from the upstream version of the software.
- **External links** — the project's home page, upstream documentation, and support channels for when you need to go deeper.

## Instructions vs. README

A package's GitHub README is a technical document for developers and contributors — how the package is built, how it differs internally from the upstream service. Instructions, by contrast, are written for **you**: practical, step-by-step, focused on what to click and what to expect. If you ever want both perspectives, the README lives in the package's repository, linked from the **About** tab.

## Missing or Empty Instructions

If a service was installed before its developer added an Instructions page (or hasn't published instructions yet), the tab will display a notice that no instructions are provided. In that case, check the service's **About** tab for links to the package repository and upstream docs, or wait for an update that includes instructions.
