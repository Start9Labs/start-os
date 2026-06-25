# Run a One-Shot Command

Before the main daemon starts, you may need to fix file ownership, run database migrations, or perform other idempotent setup. Oneshots run to completion and block dependent daemons until they finish.

## Solution

Use `.addOneshot()` in the daemon chain. Oneshots run to completion and block dependent daemons via the `requires` array. Use `exec.command` for simple shell commands (e.g., `chown`) or `exec.fn` for complex async logic. Oneshots run on every service start, not just once — they must be idempotent. A post-startup oneshot can depend on a daemon (`requires: ['app']`) to run after the app is healthy.

**Reference:** [Main](main.md)

## Examples

See `startos/main.ts` in: [ghost](https://github.com/Start9Labs/ghost-startos) (chown-mysql), [immich](https://github.com/Start9Labs/immich-startos) (configure-libraries), [nextcloud](https://github.com/Start9Labs/nextcloud-startos) (chown), [btcpayserver](https://github.com/Start9Labs/btcpayserver-startos)
