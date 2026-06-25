# Run Multiple Containers

Complex services often need multiple processes — an application server plus a database, a web frontend plus a backend API, or an app plus a cache layer. Each container gets its own subcontainer, daemon definition, health check, and dependency chain.

## Solution

Create multiple `SubContainer` instances in `setupMain()` — one per image (e.g., app, database, cache). Chain `.addDaemon()` calls for each. Use the `requires` array to control startup order — daemons wait for their dependencies' health checks to pass before starting. Each daemon gets its own volume mounts, env vars, and health check.

**Reference:** [Main](main.md)

## Examples

See `startos/main.ts` in: [am-i-exposed](https://github.com/Start9Labs/am-i-exposed-startos) (2 containers), [bitcoin-core](https://github.com/Start9Labs/bitcoin-core-startos) (4), [btcpayserver](https://github.com/Start9Labs/btcpayserver-startos) (4), [cln](https://github.com/Start9Labs/cln-startos) (2), [ghost](https://github.com/Start9Labs/ghost-startos) (2), [immich](https://github.com/Start9Labs/immich-startos) (4), [jitsi](https://github.com/Start9Labs/jitsi-startos) (5), [mempool](https://github.com/Start9Labs/mempool-startos) (3), [monerod](https://github.com/Start9Labs/monerod-startos) (2), [nextcloud](https://github.com/Start9Labs/nextcloud-startos) (3), [searxng](https://github.com/Start9Labs/searxng-startos) (3), [simplex](https://github.com/Start9Labs/simplex-startos) (2), [spliit](https://github.com/Start9Labs/spliit-startos) (2), [synapse](https://github.com/Start9Labs/synapse-startos) (3), [vaultwarden](https://github.com/Start9Labs/vaultwarden-startos) (2), [bitcoin-explorer](https://github.com/Start9Labs/bitcoin-explorer-startos) (2), [mcaptcha](https://github.com/Start9Labs/mcaptcha-startos) (3)
