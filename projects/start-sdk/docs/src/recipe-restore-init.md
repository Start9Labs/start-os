# Handle Restore from Backup

After restoring from backup, a service may need to re-register with external systems, fix file paths, or regenerate ephemeral state. The `setupOnInit` hook receives `kind === 'restore'` in this case — distinct from `'install'` (fresh) and `null` (rebuild).

## Solution

In `setupOnInit`, check for `kind === 'restore'` and run restore-specific logic: re-register with external systems, fix file paths, mark state for reindexing, or create tasks alerting the user to post-restore steps. For setup shared between install and restore but not container rebuild, use `kind !== null`.

**Reference:** [Initialization](init.md) · [Tasks](tasks.md)

## Examples

See `startos/init/` in: [lnd](https://github.com/Start9Labs/lnd-startos), [nextcloud](https://github.com/Start9Labs/nextcloud-startos), [bitcoin-core](https://github.com/Start9Labs/bitcoin-core-startos), [synapse](https://github.com/Start9Labs/synapse-startos)
