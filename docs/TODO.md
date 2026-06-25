# TODO

Pending documentation work for this repo.

## Packaging

- [ ] **Restore the SDK 2.0 packaging-workspace docs when `@start9labs/start-sdk` 2.0 publishes.** The `start-cli s9pk init-workspace` / `init-package` commands don't exist in the released CLI yet, so the workspace-setup material is temporarily hidden behind HTML comments. Grep for `sdk-2.0:` to find every spot to un-hide:
  - `packaging/src/environment-setup.md` — the whole "Set Up Your Packaging Workspace" section.
  - `packaging/src/quick-start.md` — the "Next Steps" pointer to the workspace.
  - `packaging/src/README.md` — the intro line recommending `init-workspace` (restore the original wording, drop the interim line above it).
- [ ] **Audit and refactor the packaging docs.** General pass for accuracy, redundancy, and structure — e.g. overlapping setup paths, drift between recipes / reference / real packages, and anything that assumes unreleased tooling.
