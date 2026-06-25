# Expose a Web UI

Every service with a browser interface needs at least one HTTP interface. This is the most basic networking pattern — bind a port, create an interface descriptor, and export it.

## Solution

In `setupInterfaces()`, create a `MultiHost` with `sdk.MultiHost.of(effects, 'ui')`, bind an HTTP port with `multi.bindPort(port, { protocol: 'http', preferredExternalPort: 80 })`, create a `'ui'` type interface with `sdk.createInterface()` setting `masked: false`, and export it. Return the receipt array.

If the app has no login of its own, gate it at the edge with `addSsl.auth` instead of building auth into the service — see [Authenticating at the Proxy](interfaces.md#authenticating-at-the-proxy).

**Reference:** [Interfaces](interfaces.md)

## Examples

See `startos/interfaces.ts` in: [hello-world](https://github.com/Start9Labs/hello-world-startos), [actual-budget](https://github.com/Start9Labs/actual-budget-startos), [filebrowser](https://github.com/Start9Labs/filebrowser-startos), [uptime-kuma](https://github.com/Start9Labs/uptime-kuma-startos), [spliit](https://github.com/Start9Labs/spliit-startos)
