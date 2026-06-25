# CLAUDE.md

## Operating rules

- **Bumping the SDK version requires a `CHANGELOG.md` entry.** When changing `package/package.json`'s version, add a heading at the top of `CHANGELOG.md` in the form `## <sdk-version> — StartOS <os-version> (<date>)` and categorize entries under `### Added`, `### Changed`, `### Fixed`, or `### Removed`. Don't skip this — releases without a changelog entry get caught in review.
- **Don't bump if the current latest hasn't published.** Before changing `package/package.json`'s version, check NPM. If the current latest hasn't shipped yet, edit it in place rather than adding a new version — this may mean promoting a patch revision to a minor if the change warrants it.
- **Web and container-runtime consume the *built* SDK** (`baseDist/` and `dist/`), not the source. After editing `base/` or `package/`, run `make baseDist dist` before checking the consumers.
