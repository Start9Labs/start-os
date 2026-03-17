# SDK — TypeScript Service Packaging

TypeScript SDK for packaging services for StartOS (`@start9labs/start-sdk`).

## Structure

- `base/` — Core types, ABI definitions, effects interface (`@start9labs/start-sdk-base`)
- `package/` — Full SDK for package developers, re-exports base

## Releasing

When bumping the SDK version (in `package/package.json`), always update `CHANGELOG.md`:
1. Add a new version heading at the top of the file
2. Use the format: `## <sdk-version> — StartOS <os-version> (<date>)`
3. Categorize entries under `### Added`, `### Changed`, `### Fixed`, or `### Removed`
