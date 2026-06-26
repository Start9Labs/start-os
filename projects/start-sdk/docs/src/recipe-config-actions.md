# Create Configuration Actions

Many services need user-configurable settings — log levels, feature toggles, resource limits. On StartOS, these are presented as actions with input forms. The user fills out the form, and the handler writes the values to a file model.

## Solution

Use `sdk.Action.withInput()` with an `InputSpec` built from `Value.select()`, `Value.number()`, `Value.toggle()`, `Value.triState()`, etc. The prefill function reads current values from a file model with `.read().once()`. The handler writes new values with `fileModel.merge()`, which preserves any keys not in the input.

**Reference:** [Actions](actions.md) · [File Models](file-models.md)

## Examples

See `startos/actions/` in: [bitcoin-core](https://github.com/Start9Labs/bitcoin-core-startos), [cln](https://github.com/Start9Labs/cln-startos), [lnd](https://github.com/Start9Labs/lnd-startos), [electrs](https://github.com/Start9Labs/electrs-startos), [fulcrum](https://github.com/Start9Labs/fulcrum-startos), [nostr-rs-relay](https://github.com/Start9Labs/nostr-rs-relay-startos), [monerod](https://github.com/Start9Labs/monerod-startos), [searxng](https://github.com/Start9Labs/searxng-startos), [ghost](https://github.com/Start9Labs/ghost-startos), [gitea](https://github.com/Start9Labs/gitea-startos), [nextcloud](https://github.com/Start9Labs/nextcloud-startos), [synapse](https://github.com/Start9Labs/synapse-startos), [vaultwarden](https://github.com/Start9Labs/vaultwarden-startos), [btcpayserver](https://github.com/Start9Labs/btcpayserver-startos), [mempool](https://github.com/Start9Labs/mempool-startos), [public-pool](https://github.com/Start9Labs/public-pool-startos), [garage](https://github.com/Start9Labs/garage-startos), [filebrowser](https://github.com/Start9Labs/filebrowser-startos)
