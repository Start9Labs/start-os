# Require Setup Before Starting

Some services need the user to complete a step before the service can start — choosing a backend, setting a permanent hostname, entering API credentials. A critical task with a hidden action blocks startup until the user acts.

## Solution

In `setupOnInit` (on install), call `sdk.action.createOwnTask()` with severity `'critical'` pointing to a hidden action. The action collects user input via `InputSpec` and persists the choice to a file model. Because the task is critical, the service cannot start until the user completes it. Use `allowedStatuses: 'only-stopped'` on the action.

**Reference:** [Initialization](init.md) · [Tasks](tasks.md) · [Actions](actions.md)

## Examples

See `startos/init/` and `startos/actions/` in: [albyhub](https://github.com/Start9Labs/albyhub-startos), [lnbits](https://github.com/Start9Labs/lnbits-startos), [lnd](https://github.com/Start9Labs/lnd-startos), [synapse](https://github.com/Start9Labs/synapse-startos), [vaultwarden](https://github.com/Start9Labs/vaultwarden-startos), [openclaw](https://github.com/Start9Labs/openclaw-startos), [lightning-terminal](https://github.com/Start9Labs/lightning-terminal-startos), [start9-pages](https://github.com/Start9Labs/start9-pages-startos)
