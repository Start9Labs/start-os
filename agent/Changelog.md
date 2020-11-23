# 0.2.5

- Upgrade to GHC 8.10.2 / Stackage nightly-2020-09-29
- Remove internet connectivity check from startup sequence
- Move ssh setup to synchronizers
- Adds new dependency management structure
- Changes version implementation from semver to new "emver" implementation
- Adds autoconfigure feature
- Remaps "Restarting" container status to "Crashed" for better UX
- Persists logs after restart
- Rewrites nginx ssl conf during UI upgrade
- Implements better caching strategy for static assets