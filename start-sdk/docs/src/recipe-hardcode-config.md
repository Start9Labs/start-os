# Hardcode Config Values

Some settings must be fixed for the service to work on StartOS — ports, data paths, bind addresses, auth modes. Use `z.literal().catch()` in your file model schema to enforce these values. Any manual edit or stale config is automatically corrected on the next read.

## Solution

In your zod schema, use `z.literal(value).catch(value)` for fields that must never change (ports, bind addresses, data paths, auth modes). The literal type prevents writes with different values, and `.catch()` auto-corrects existing files on the next `merge()`. Every nested object needs its own `.catch()` with full defaults — zod cannot cascade through nested objects, so if the outer object is missing, the inner `.catch()` values are never reached.

**Reference:** [File Models](file-models.md)

## Examples

See `startos/fileModels/` in: [bitcoin-core](https://github.com/Start9Labs/bitcoin-core-startos), [cln](https://github.com/Start9Labs/cln-startos), [lnd](https://github.com/Start9Labs/lnd-startos), [monerod](https://github.com/Start9Labs/monerod-startos), [synapse](https://github.com/Start9Labs/synapse-startos), [tor](https://github.com/Start9Labs/tor-startos), [nostr-rs-relay](https://github.com/Start9Labs/nostr-rs-relay-startos), [simplex](https://github.com/Start9Labs/simplex-startos)
