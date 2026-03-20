# Backend — StartWRT Rust Crates

Rust workspace powering the StartWRT router daemon and CLI. Three crates: `ctrl` (RPC server + CLI), `uciedit` (UCI parser library), `uciedit_macros` (proc macros for typed UCI sections).

## Architecture

### Transport

Single HTTP POST endpoint on `0.0.0.0:3301`. JSON-RPC 2.0 via [rpc-toolkit](https://github.com/Start9Labs/rpc-toolkit) (Start9Labs library). Session cookie auth.

### Server vs CLI

Both `startwrt-ctrld` (daemon) and `startwrt-cli` (CLI) share the same handler code via the `CtrlContext` trait:

```rust
pub trait CtrlContext: Context + Clone {
    fn uci_root(&self) -> PathBuf;   // /etc/config/ on router, configurable for CLI
    fn effectful(&self) -> bool;      // CLI: --configs-only skips service reloads
}
```

- **ServerContext**: always reads `/etc/config/`, always reloads services
- **CliContext**: configurable root, cookie persistence in `~/.startwrt/.cookies.json`, calls server via HTTP

### RPC Method Tree

All methods assembled in `main_api()`:

```
auth.login          auth.logout         auth.verify-password    auth.reset-password
profiles.list       profiles.get        profiles.create         profiles.set
ethernet.get        ethernet.set
wifi.get            wifi.set            wifi.blackout-get       wifi.blackout-set
uci.get             uci.set             (generic — being replaced by smart endpoints)
file.get            file.set            dir.get
exec                                    (being phased out)
```

See [API_CONTRACT.md](../API_CONTRACT.md) for the complete target contract including endpoints that still need to be built.

## Modules

### profiles.rs — Security Profile CRUD

The core module. Each profile creates:
- A **VLAN** on the LAN bridge (unique tag)
- A **network interface** bound to that VLAN
- A **firewall zone** with forwarding rules
- A **DHCP server** for the profile's subnet

Orchestrates changes across four UCI configs: `startwrt`, `network`, `firewall`, `dhcp`. Uses retry loops (4 attempts) for conflict resolution.

Key types:
- `Profile<Id>` — gateway IP, outbound route, `LanAccess` (All/SameProfile/OtherProfiles), `WanAccess` (All/None/Whitelist/Blacklist)
- `ProfileId` — fullname + interface name + VLAN tag
- `Lookup` — index for resolving profiles across configs

### wifi.rs — WiFi Configuration

Manages SSID, per-radio settings (band, channel, enabled, broadcast), and multi-password config. Each password maps to a profile via dynamic VLAN (`WifiDynamicVlan::REQUIRED`). The admin password uses `ALLOWED`.

Also manages WiFi blackout schedules via crontab (`/etc/crontabs/root`).

Creates `WifiStation` (password entries) and `WifiVlan` (VLAN mapping) sections in the `wireless` UCI config.

### ethernet.rs — Ethernet Port Assignment

Maps physical ports to profiles via bridge VLAN assignments on the LAN bridge (`br-lan`). Also manages WAN port designation (which port is the uplink). Reads/writes `network` UCI config for `NetworkBridgeVlan` and `NetworkVlanPort` sections.

### auth.rs — Session Authentication

- Sessions stored as JSON in `/etc/startwrt/sessions.json`
- Password validated against `/etc/shadow` (or `STARTWRT_DEV_PASSWORD` env var for dev)
- Tokens: base32-encoded random bytes, stored as SHA-256 hashes
- 1-day session expiry
- Rate limiting: 3 login attempts per 20 seconds

### uci.rs, files.rs, exec.rs — Generic/Legacy

Low-level UCI, file, and shell access. The frontend currently uses these for features that lack smart endpoints (WAN, LAN, devices, published ports, outbound VPNs, SSH keys). **Being replaced** — see the migration table in the root CLAUDE.md.

### middleware/auth.rs — HTTP Session Middleware

Axum middleware applied to all RPC endpoints. Extracts session cookie, validates token, rate-limits login, injects `sessionHash` into params when needed. Returns RPC error code 34 on auth failure (frontend auto-logs out on this code).

### error.rs — Error Types

`ErrorKind` enum covers: IO, UCI parse/write errors, and domain errors (`InterfaceNameConflict`, `LanOwnerExists`, `MissingProfile`, `CorruptedProfile`, `CorruptedWifi`, `DuplicateVlanTag`, `DuplicatePassword`, `WanPortWithProfile`). Converts to `RpcError` for JSON-RPC responses.

## UCI Library (uciedit)

### Parsing

Arena-based zero-copy parser. `Config::parse()` reads a UCI file into a tree of `Section`s, each containing `Line`s (option, list, comment, empty). Preserves all formatting and comments.

`parse_all(root, arena, names)` loads multiple configs at once with file locking and conflict detection.

### Writing

`Config::dump()` writes back to disk using atomic temp-file + rename. `dump_all()` does the same for multiple configs. Conflict detection compares the file's modified timestamp against what was read — if the file changed since parsing, the write fails and the caller can retry.

### Typed Sections

The `TypedSection` trait (derived via `#[derive(TypedSection)]`) provides type-safe access to UCI config sections:

```rust
#[derive(TypedSection)]
#[uci(ty = "zone")]
struct FirewallZone<'a> {
    name: Token<'a>,
    input: Token<'a>,
    output: Token<'a>,
    forward: Token<'a>,
    #[uci(default)]
    network: Vec<Token<'a>>,
}
```

Macro attributes:
- `#[uci(ty = "zone")]` — UCI section type name
- `#[uci(rename = "type")]` — field name differs from UCI option name
- `#[uci(default)]` — use Default::default() if option missing
- `#[uci(default_value = expr)]` — custom default
- `#[uci(inpt)]` — use inpt parser instead of FromStr

All typed sections live in `uciedit/src/openwrt.rs`:
- **Firewall**: `FirewallZone`, `FirewallRule`, `FirewallForwarding`
- **Network**: `NetworkInterface`, `NetworkDevice`, `NetworkBridgeVlan`, `NetworkVlanPort`
- **WiFi**: `WifiDevice`, `WifiInterface`, `WifiVlan`, `WifiStation`
- **DHCP**: `Dhcp`

## Common Pitfalls

- **UCI conflicts.** Multiple concurrent writes to the same config file will fail. Always use `uciedit`'s retry mechanism. Profile creation already does this (4 retries).
- **Service reloads.** After writing UCI configs, call `/etc/init.d/<service> reload`. Without this, changes don't take effect until reboot. The `effectful` flag on `CtrlContext` controls whether reloads happen.
- **Arena lifetimes.** `uciedit` uses arena allocation — all `Token<'a>` and `Section<'a>` values borrow from the arena. Don't try to outlive it.
- **Bridge VLAN vs swconfig.** StartWRT requires DSA (Distributed Switch Architecture) with bridge VLAN filtering. Older `swconfig`-based switches are not supported.
- **wpad package.** The default `wpad-basic-mbedtls` doesn't support `wpa_psk_file` keyids. Must install `wpad-wolfssl` and `hostapd-utils`.
