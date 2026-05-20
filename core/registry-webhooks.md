# Registry Webhooks

The registry binary (`registrybox`) signs outbound webhooks with an Ed25519 keypair generated on first start. Each event is POSTed to every configured subscriber URL and signed with the registry's private key; the public key rides in the request header as the registry's identity. Consumers maintain an allowlist of trusted public keys — there is no shared secret to coordinate. The subscriber set is managed at runtime via RPC and persisted in patch-db (keyed by URL); there is no config-file webhook setting.

## Subsystem layout

- `src/registry/webhook/event.rs` — Source of truth for the event taxonomy: the `RegistryEventData` tagged enum (one variant per topic) and its payload structs, plus the `RegistryEvent` envelope (id + occurredAt + flattened event data). Adding or changing a topic happens here; every other layer follows.
- `src/registry/webhook/mod.rs` — Storage and delivery-attempt types (`WebhookEventRecord`, `WebhookLog`, `DeliveryAttempt`) and `generate_signing_key()` for bootstrap. Re-exports the event types.
- `src/registry/webhook/dispatcher.rs` — Long-running task that drains the event channel, reads the current subscriber set from the DB, persists each event to `WebhookLog`, then signs and POSTs to **each** subscriber, appending a `DeliveryAttempt` per delivery. Also exposes `deliver` (takes a single subscriber `Url`) and `append_attempt` as helpers reused by the replay RPC.
- `src/registry/webhook/api.rs` — `webhook.subscriber.{add,remove,list}`, `webhook.list`, `webhook.replay`, and `webhook.pubkey` RPCs.

The dispatcher is spawned once from `RegistryContext::init` and receives events over an unbounded `mpsc` channel whose sender (`event_tx`) lives on `RegistryContextSeed`. The signing key is generated lazily on first init and cached on the seed (`webhook_signing_key`). The subscriber set is *not* cached on the seed — the dispatcher re-reads it from the DB for each event, so `subscriber add`/`remove` take effect on the next emission without a restart.

## Bootstrap

`RegistryContext::init` calls `db.mutate(...)` after migrations to check `webhook_signing_key` on the database. If it's `None`, `generate_signing_key()` produces a fresh `ed25519_dalek::SigningKey`, the wrapped `Pem<SigningKey>` is persisted, and the private key is stashed on the seed for the dispatcher. Subsequent boots load the existing key. The bootstrap is deterministic-per-registry: the same key is used for the lifetime of the database (no rotation today; if rotation becomes necessary it would be a deliberate operator action via a new RPC, since rotating invalidates consumer allowlists).

The persisted key is a private key — the `db dump` RPC (admin-only) does expose it. Backups include it; restoring a backup preserves the registry's webhook identity.

## Emission

Mutation handlers send a `RegistryEvent` on `ctx.event_tx` **after** their `db.mutate(...)` call returns, and only when `revision.is_some()` — no-op mutations (e.g. re-uploading an identical s9pk) do not produce an event. Each handler constructs a `RegistryEventData::<Variant>(<Payload>{...})` from values captured before the closure consumed them, then wraps it via `RegistryEvent::new(data)` which stamps a fresh `id`/`occurredAt`.

`RegistryEventData` is a `#[serde(tag = "topic", content = "data")]` enum, so the wire shape is unchanged — the discriminant `topic` and the typed `data` payload land at the top level of the envelope alongside `id`/`occurredAt`.

Current variants (declared in `webhook/event.rs`):

| Variant | Topic | Site | Payload struct |
|---|---|---|---|
| `PackageVersionAdd` | `package.version.add` | `src/registry/package/add.rs::add_package` | `PackageVersionAddData { packageId, version, isFirstVersion, isUpdate, urls, metadata }` |
| `PackageRemove` | `package.remove` | `src/registry/package/add.rs::remove_package` | `PackageRemoveData { packageId, version?, sighash? }` |
| `OsVersionAdd` | `os.version.add` | `src/registry/os/version/mod.rs::add_version` | `OsVersionAddData { version, headline, releaseNotes, sourceVersion, isUpdate }` |
| `OsVersionRemove` | `os.version.remove` | `src/registry/os/version/mod.rs::remove_version` | `OsVersionRemoveData { version }` |

`isFirstVersion` is computed by inspecting the pre-mutation state inside the closure (count of versions for this package id). `isUpdate` is true when the specific version key already existed.

Adding a new topic: add a variant to `RegistryEventData` with its payload struct in `webhook/event.rs` and emit `RegistryEvent::new(RegistryEventData::<Variant>(<Payload>{...}))` from the relevant handler. The dispatcher is topic-agnostic; no registration is needed. TS bindings regenerate on `make ts-bindings`, giving subscribers a discriminated union they can switch over.

## Wire format

Each delivery is a JSON `POST` with four headers:

- `x-startos-registry-pubkey` — base64 of the SPKI-DER-encoded Ed25519 public key (about 60 ASCII characters). This is the registry's identity. Consumers look it up in an allowlist of trusted registries before verifying. SPKI-DER (rather than the raw 32-byte form) so consumers can hand the decoded bytes directly to any standard crypto library — e.g. Node's `crypto.createPublicKey({ key, format: 'der', type: 'spki' })` — without re-wrapping. `webhook.pubkey` emits the same form, so an allowlist loaded from its output matches the header value byte-for-byte.
- `x-startos-registry-signature` — base64 of the 64-byte Ed25519 signature over the raw body, signed with the registry's private key.
- `x-startos-registry-topic` — the event topic string.
- `x-startos-registry-event-id` — the `Guid` of the event (32-char base32). Stable across replays — dedupe on this.

Body envelope (`RegistryEvent`):

```json
{
  "id": "<guid>",
  "topic": "package.version.add",
  "occurredAt": "2026-05-19T18:22:01Z",
  "data": { ... }
}
```

The body is identity-free; the pubkey header carries identity. Replays produce a byte-identical body and signature.

Consumers verify by: (1) reading the pubkey header, (2) rejecting if not in their allowlist, (3) verifying the signature over the raw body using the pubkey, (4) deduping on `event-id`. No shared secret is required.

## Persistence

State lives at `registry_database.webhook_log.events: BTreeMap<Guid, WebhookEventRecord>`. Each record holds the original `RegistryEvent` plus an append-only `attempts: Vec<DeliveryAttempt>`. With multiple subscribers a single event accrues one attempt per subscriber per delivery round (the initial fan-out plus any replays).

A `DeliveryAttempt` records the target `subscriber` URL, timestamp, HTTP status (or `None` for network/serialization failure), error string, duration in milliseconds, and a `replay: bool` flag. Successful deliveries set `error = None`; non-2xx responses populate `error` with the status line.

The subscriber set lives at `registry_database.webhook_subscribers: BTreeSet<Url>` — the URL is the subscriber's identity.

Schema is additive: `webhook_log`, `webhook_signing_key`, and `webhook_subscribers` all have `#[serde(default)]`, so existing registry databases deserialize without a migration. The signing key materializes on first init via the bootstrap path above.

The log grows unbounded — there is no retention or pruning. At human-driven registry mutation rates this is fine, but if call volume grows a pruning RPC (or an age-based GC) is the natural follow-up.

## Subscribers

Subscribers are managed at runtime, not via config. Three admin RPCs under `webhook.subscriber`:

- `webhook.subscriber.add { url }` — validates the URL scheme is `http`/`https` and inserts it into `webhook_subscribers`. Idempotent (it's a set).
- `webhook.subscriber.remove { url }` — removes the URL. Idempotent.
- `webhook.subscriber.list` — returns the current set.

That's it — no shared secret, no operator-chosen identifier. The registry's identity is its public key (which the operator publishes via `registry webhook pubkey` and each consumer adds to its allowlist).

When the set is empty, the dispatcher drains and drops events without touching patch-db. Nothing is persisted while there are no subscribers.

## Replay semantics

`webhook.replay { eventId }` reads the persisted event and re-delivers it to **every** current subscriber, calling `dispatcher::deliver` with `replay: true` once per subscriber and appending each resulting `DeliveryAttempt` to the event's record. It returns the `Vec<DeliveryAttempt>` for the round. The `event-id` header is unchanged, which means a well-behaved consumer that dedupes by event id will accept the replay only if it never saw the original — i.e. replay is an idempotent re-try, not a force-re-announce. Replaying to all subscribers (rather than a targeted one) is harmless precisely because of this dedupe: subscribers that already received the event ignore the repeat.

Replay errors if no subscribers are configured (`registry.webhook.no-subscriber`) or the event id is not in the log (`registry.webhook.event-not-found`).

## Pubkey RPC

`webhook.pubkey` is unauthenticated (the public key is, by definition, public) and returns the registry's webhook verifying key as base64-encoded Ed25519 SPKI DER — the same form sent in the `x-startos-registry-pubkey` header on every webhook delivery, so consumers can paste it straight into their allowlist with no re-encoding. Operators share the output with each consumer they want to authorize.
