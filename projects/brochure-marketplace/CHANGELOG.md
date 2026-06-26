# Changelog

All notable changes to the brochure (public Start9 Marketplace, marketplace.start9.com) are documented here.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/). This project is a continuously deployed website (it ships on every merge to `master`) and is not independently versioned; changes are tracked under dated/Unreleased headings rather than semantic version tags.

## [Unreleased]

This cycle (StartOS 0.4.0-beta.10) the brochure was brought into the StartOS
monorepo and rebuilt on the shared marketplace components, so it now ships the
identical service browsing UI as the in-OS marketplace.

### Added

- **Continuous deploy to marketplace.start9.com (ba4be114a).** A GitHub Actions workflow builds the brochure and rsyncs it to the shared VPS whenever its build inputs change on `master`, replacing the manual deploy (with an `index.html` guard before the destructive sync).
- **Unified on the shared marketplace components (619b65d62).** The service detail/preview, drawer tile, and registry picker now come from `@start9labs/marketplace` behind an `AbstractMarketplaceService` (the brochure implements it against `localStorage`), so the public site renders the identical detail UI as the OS. Browsing is query-param driven (`?registry`/`id`/`flavor`/`search`), so links are shareable and open an arbitrary service in an arbitrary registry — including unsaved ones, with a "Save this registry" affordance.
- **Custom registries by bare domain (#3349).** A custom registry can be added by domain alone (https default; http for `.onion`), via the same shared `registryUrl()` normalizer the OS uses.
- **"Package a service" link and refreshed categories.** A "Package a service" link (under "Get a Start9 server", linking to the packaging docs) sits beneath the sidebar categories; empty categories are hidden, and the category icon set is refreshed for the current taxonomy (AI, Bitcoin, Crypto, Finance, Media, Networking, Productivity, Social).

### Fixed

- **The site loads content again (63088d14c).** The brochure spoke the StartOS host-proxy RPC dialect (POSTing `registry.*` methods to the bare registry URL), which `registry.start9.com` 405s. It now calls each registry's own endpoint (`POST <registry>/rpc/v0` with bare `info` / `package.get` methods), verified against `registry.start9.com` and `community-registry.start9.com` with correct CORS for the marketplace origin.
- **Unifying on shared components fixed four long-standing brochure bugs by construction (619b65d62):** the `[object Object]` subtitle, services listing themselves as alternative implementations, the detail opening as a centered modal instead of a sidebar drawer, and the inability to add custom registries.
- **Proper social preview cards (a4e6ae604).** Switched to `summary_large_image` with full Open Graph hints (`og:image` dimensions/alt, `og:site_name`, `og:locale`) and a canonical 1200×630 preview image, so X, Telegram, Discord, Signal, Facebook, LinkedIn, and iMessage render a large banner reliably; dropped the stale cache-bust query and non-standard `twitter:*` tags.
- **`config.json` is now committed (76c616d24)** via a `.gitignore` negation, so the deploy build can resolve registry URLs.
- **Shared-lib rendering is consistent across hosts (c972e4d3b),** and the install flow no longer references the removed package `alerts` (#3333).

### Changed

- Migrated into the start-os monorepo as an application project of the `shared-libs/web` Angular workspace. Build and serve via the `brochure` Angular project (`npm run build:brochure` / `npm run start:brochure` from `shared-libs/web`); deploys automatically to marketplace.start9.com on merge to `master`.
- **Consolidated into the StartOS web workspace (da6a89e05)** earlier in the cycle: the previously-standalone brochure source moved in as a workspace app consuming the in-repo `shared`/`marketplace` libraries and the browser-safe SDK (`baseDist`) directly, retiring its published-SDK patch hack (the `postinstall` patch hook, `scripts/patch-start-sdk.js`, and the pinned `@start9labs/start-sdk` dependency).
- **Conformity polish (99f487292):** i18n alignment, category derivation, and parity with the in-OS marketplace twin.
