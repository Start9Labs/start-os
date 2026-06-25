# Interfaces

`setupInterfaces()` defines the network interfaces your service exposes and how they are made available to the user. This function runs on service install, update, and config save.

## Single Interface

For a service with one web interface:

```typescript
import { i18n } from './i18n'
import { sdk } from './sdk'

export const setInterfaces = sdk.setupInterfaces(async ({ effects }) => {
  const multi = sdk.MultiHost.of(effects, 'ui')
  const origin = await multi.bindPort(80, {
    protocol: 'http',
    preferredExternalPort: 80,
  })

  const ui = sdk.createInterface(effects, {
    name: i18n('Web Interface'),
    id: 'ui',
    description: i18n('The main web interface'),
    type: 'ui',
    masked: false,
    schemeOverride: null,
    username: null,
    path: '',
    query: {},
  })

  return [await origin.export([ui])]
})
```

## Multiple Interfaces

Expose multiple paths (e.g., web UI and admin panel) from the same port:

```typescript
export const setInterfaces = sdk.setupInterfaces(async ({ effects }) => {
  const multi = sdk.MultiHost.of(effects, 'web')
  const origin = await multi.bindPort(80, {
    protocol: 'http',
    preferredExternalPort: 80,
  })

  const ui = sdk.createInterface(effects, {
    name: i18n('Web UI'),
    id: 'ui',
    description: i18n('The web interface'),
    type: 'ui',
    masked: false,
    schemeOverride: null,
    username: null,
    path: '',
    query: {},
  })

  const admin = sdk.createInterface(effects, {
    name: i18n('Admin Panel'),
    id: 'admin',
    description: i18n('Admin interface'),
    type: 'ui',
    masked: false,
    schemeOverride: null,
    username: null,
    path: '/admin/',
    query: {},
  })

  return [await origin.export([ui, admin])]
})
```

Expose interfaces on separate ports:

```typescript
export const setInterfaces = sdk.setupInterfaces(async ({ effects }) => {
  const receipts = []

  // Web UI — HTTP
  const uiMulti = sdk.MultiHost.of(effects, 'ui')
  const uiOrigin = await uiMulti.bindPort(80, {
    protocol: 'http',
    preferredExternalPort: 80,
  })
  const ui = sdk.createInterface(effects, {
    name: i18n('Web Interface'),
    id: 'ui',
    description: i18n('The main browser interface'),
    type: 'ui',
    masked: false,
    schemeOverride: null,
    username: null,
    path: '',
    query: {},
  })
  receipts.push(await uiOrigin.export([ui]))

  // API — HTTPS with SSL termination
  const apiMulti = sdk.MultiHost.of(effects, 'api')
  const apiOrigin = await apiMulti.bindPort(8080, {
    protocol: 'https',
    preferredExternalPort: 8080,
    addSsl: {
      alpn: null,
      preferredExternalPort: 8080,
      addXForwardedHeaders: false,
    },
  })
  const api = sdk.createInterface(effects, {
    name: i18n('REST API'),
    id: 'api',
    description: i18n('Programmatic access'),
    type: 'api',
    masked: true,
    schemeOverride: null,
    username: null,
    path: '',
    query: {},
  })
  receipts.push(await apiOrigin.export([api]))

  // Peer — raw TCP (not HTTP)
  const peerMulti = sdk.MultiHost.of(effects, 'peer')
  const peerOrigin = await peerMulti.bindPort(9735, {
    protocol: null,
    addSsl: null,
    preferredExternalPort: 9735,
    secure: { ssl: false },
  })
  const peer = sdk.createInterface(effects, {
    name: i18n('Peer Interface'),
    id: 'peer',
    description: i18n('Peer-to-peer network connections'),
    type: 'p2p',
    masked: true,
    schemeOverride: null,
    username: null,
    path: '',
    query: {},
  })
  receipts.push(await peerOrigin.export([peer]))

  return receipts
})
```

The key steps are:

1. Create a `MultiHost` and bind a port with protocol and options
2. Create one or more interfaces using `sdk.createInterface()`
3. Export the interfaces from the origin and return the receipt(s)

## bindPort Options

| Option | Type | Description |
|--------|------|-------------|
| `protocol` | `'http'` \| `'https'` \| `null` | The protocol. Use `null` for raw TCP (non-HTTP). |
| `preferredExternalPort` | `number` | The port users will see in their URLs. |
| `addSsl` | `object` \| `null` | SSL termination options for HTTPS. Set to `null` for no SSL. |
| `addSsl.alpn` | `string` \| `null` | ALPN protocol negotiation (e.g., `'h2'`). Usually `null`. |
| `addSsl.preferredExternalPort` | `number` | External port for SSL connections. |
| `addSsl.addXForwardedHeaders` | `boolean` | Whether to add `X-Forwarded-*` headers. |
| `addSsl.auth` | `ProxyAuth` \| `null` | Optional auth gate enforced by the OS reverse proxy. See [Authenticating at the Proxy](#authenticating-at-the-proxy). |
| `secure` | `{ ssl: boolean }` \| `null` | For non-HTTP protocols, whether the connection is secure. |

## Interface Options

```typescript
sdk.createInterface(effects, {
  name: i18n('Display Name'),      // Shown in UI (wrap with i18n)
  id: 'unique-id',                 // Used in sdk.serviceInterface.getOwn()
  description: i18n('Description'),// Shown in UI (wrap with i18n)
  type: 'ui',                      // 'ui', 'api', or 'p2p'
  masked: false,                   // Hide URLs with sensitive credentials?
  schemeOverride: null,            // Override URL scheme (see below)
  username: null,                  // Auth username embedded in URL
  path: '/some/path/',             // URL path
  query: {},                       // URL query params
})
```

| Option | Type | Description |
|--------|------|-------------|
| `name` | `string` | Display name shown to the user. Wrap with `i18n()`. |
| `id` | `string` | Unique identifier. Used to retrieve this interface in [main.ts](./main.md) via `sdk.serviceInterface.getOwn()`. |
| `description` | `string` | Description shown to the user. Wrap with `i18n()`. |
| `type` | `'ui'`, `'api'`, or `'p2p'` | `'ui'` for browser interfaces, `'api'` for programmatic endpoints, `'p2p'` for peer-to-peer connections. |
| `masked` | `boolean` | If `true`, the interface URL is shown as a copyable secret. Use for URLs containing credentials or tokens. |
| `schemeOverride` | `{ ssl: string \| null; noSsl: string \| null }` \| `null` | Override the URL scheme for custom protocols. For example, `{ ssl: 'lndconnect', noSsl: 'lndconnect' }` produces `lndconnect://` URLs. Use `null` for standard `http`/`https`. |
| `username` | `string` \| `null` | Username embedded in the URL (e.g., for `smp://fingerprint:password@host`). |
| `path` | `string` | URL path appended to the base address (e.g., `'/admin/'`). |
| `query` | `object` | URL query parameters as key-value pairs (e.g., `{ macaroon: 'abc123' }`). |

> [!TIP]
> The `id` you assign to an interface is what you use in `main.ts` to retrieve hostnames for that interface. For example, if you set `id: 'ui'`, you would call `sdk.serviceInterface.getOwn(effects, 'ui')` to get its address information. See [Main](./main.md#getting-hostnames) for details.

## TLS Termination

StartOS terminates TLS at the platform edge and proxies plain HTTP to your container. This has two important consequences any time your service generates URLs or makes scheme decisions:

**1. Inside the container, every request arrives over HTTP.** A reverse proxy like nginx will see `$scheme == "http"`, the `X-Forwarded-Proto` header is not authoritative by default, and there is no TLS certificate to terminate. Do not configure in-container HTTPS — StartOS is already doing it.

**2. The browser loaded the page over `https://`.** Any URL your service emits for the browser to consume (login redirects, API endpoints in a `config.json`, OAuth callbacks, absolute links in HTML) must use `https://`. If you emit `http://` or derive the scheme from `$scheme`, the browser will block the request as [mixed active content](https://developer.mozilla.org/en-US/docs/Web/Security/Mixed_content).

**Hardcode `https://` for browser-facing URLs** rather than interpolating `$scheme` or reading the protocol from the incoming request:

```nginx
# BAD — $scheme is always "http" inside the container
return 200 '{"api_url":"$scheme://$host/api"}';

# GOOD — match what the browser actually sees
return 200 '{"api_url":"https://$host/api"}';
```

This applies to any configuration file generated in `setupMain` or any runtime response that includes absolute URLs — not just nginx. When in doubt, hardcode `https://`.

## Authenticating at the Proxy

For protocols that StartOS fronts with its reverse proxy (`http`, `https`, `ws`, `wss`), you can gate an interface with HTTP authentication by setting `addSsl.auth`. The OS reverse proxy validates the `Authorization` header on every incoming request _before_ forwarding it to your container. Requests that fail get `401 Unauthorized` with a `WWW-Authenticate` challenge and never reach your service. You do **not** need to build auth into the service or run a sidecar proxy — the platform enforces it at the edge.

`auth` takes a `ProxyAuth`, which is one of two shapes:

```typescript
// Basic — one or more username/password pairs; any match passes
const uiOrigin = await uiMulti.bindPort(uiPort, {
  protocol: 'http',
  addSsl: {
    auth: {
      type: 'basic',
      credentials: [{ username: 'admin', password }],
      realm: null, // advertised in the WWW-Authenticate challenge; defaults to "StartOS"
    },
  },
})

// Bearer — any of the listed tokens is accepted as `Authorization: Bearer <token>`
const apiOrigin = await apiMulti.bindPort(apiPort, {
  protocol: 'https',
  addSsl: {
    auth: { type: 'bearer', tokens: [apiToken], realm: null },
  },
})
```

| `ProxyAuth` field | Type | Description |
|--------|------|-------------|
| `type` | `'basic'` \| `'bearer'` | The auth scheme the proxy enforces. |
| `credentials` (basic) | `Array<{ username, password }>` | Accepted pairs. Any match passes. The matched `username` is forwarded upstream as `X-Forwarded-User`. |
| `tokens` (bearer) | `Array<string>` | Accepted bearer tokens. Any match passes. |
| `realm` | `string` \| `null` | Realm advertised in the 401 `WWW-Authenticate` challenge. Defaults to `"StartOS"`. Use a stable realm across bindings that share credentials so browsers reuse them. |

Setting `auth` implies HTTP-aware proxying, so it is only valid on the SSL-variant protocols above — not on raw TCP (`protocol: null`).

> [!NOTE]
> The `username` field on `createInterface` is unrelated to this gate — it only embeds a username in the _displayed_ URL (e.g. `https://user@host/`). The enforced credential check is `addSsl.auth`.

### Generating and rotating credentials

Don't hard-code the password. Generate it at install time and let the user rotate it through an action. Store the credential in a [file model](./file-models.md) such as `store.json` and read it reactively in `setupInterfaces` — when the action rewrites the stored value, `setupInterfaces` re-runs and the proxy picks up the new credential automatically:

```typescript
export const setInterfaces = sdk.setupInterfaces(async ({ effects }) => {
  const password = await storeJson.read((s) => s.uiPassword).const(effects)

  const uiMulti = sdk.MultiHost.of(effects, 'ui-multi')
  const uiOrigin = await uiMulti.bindPort(uiPort, {
    protocol: 'http',
    addSsl: {
      auth: { type: 'basic', credentials: [{ username: 'admin', password }], realm: null },
    },
  })

  const ui = sdk.createInterface(effects, {
    name: i18n('Web UI'),
    id: 'ui',
    description: i18n('The web interface'),
    type: 'ui',
    masked: false,
    schemeOverride: null,
    username: null,
    path: '',
    query: {},
  })

  return [await uiOrigin.export([ui])]
})
```

Seed `uiPassword` with a generated value during [install init](./init.md) so the gate is active from first start, and pair it with a `reset-password` action that rewrites the stored value and surfaces it to the user once. See [Reset Password](./recipe-reset-password.md).
