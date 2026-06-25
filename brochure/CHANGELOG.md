# Changelog

All notable changes to the brochure (public Start9 Marketplace, marketplace.start9.com) are documented here.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/). This project is a continuously deployed website (it ships on every merge to `master`) and is not independently versioned; changes are tracked under dated/Unreleased headings rather than semantic version tags.

## [Unreleased]

### Changed

- Migrated into the start-os monorepo as an application project of the `shared/web` Angular workspace. Build and serve via the `brochure` Angular project (`npm run build:brochure` / `npm run start:brochure` from `shared/web`); deploys automatically to marketplace.start9.com on merge to `master`.
