# StartWRT

Router OS for home self-hosting. OpenWrt fork with a Rust backend and Angular frontend. The core concept is **Security Profiles** — every device on the network receives a profile (via WiFi password, Ethernet port, or VPN server) that governs its LAN access, WAN access, DNS, and VPN routing. Under the hood, each profile is a VLAN with its own subnet, firewall zone, and DHCP server.

## Repository Layout

```
├── backend/                 # Rust workspace (see backend/CLAUDE.md)
│   ├── ctrl/                # RPC server + CLI (axum, port 3301)
│   ├── uciedit/             # UCI config parser/serializer library
│   ├── uciedit_macros/      # #[derive(TypedSection)] proc macro
│   ├── firstboot_config/    # Template UCI configs for factory reset
│   └── config_experiments/  # Historical manual UCI test configs
│
├── web/                     # Angular 21 frontend (see web/CLAUDE.md)
└── API_CONTRACT.md          # Complete Rust-typed RPC endpoint contract
```

## How It Works

### Data Flow

```
Frontend (Angular) → JSON-RPC 2.0 POST → axum (port 3301) → SessionAuth middleware → handler
  handler reads UCI files via uciedit → maps to domain types → JSON response
  handler writes UCI files via uciedit → reloads services (/etc/init.d/... reload)
```

### Security Profiles

Each profile creates a VLAN on the LAN bridge, a network interface, a firewall zone with forwarding rules, and a DHCP server. Devices receive a profile based on how they join:

- **WiFi**: `wpa_psk_file` with per-password VLAN assignment (dynamic VLAN, no RADIUS). One SSID, many passwords.
- **Ethernet**: Bridge VLAN port assignments — each physical port tagged to a profile's VLAN.
- **Inbound VPN**: Each WireGuard server interface bound to a profile.

### UCI as Source of Truth

All router state lives in OpenWrt UCI config files (`/etc/config/`). The `uciedit` crate parses them with arena-based zero-copy parsing, writes them atomically (temp file + rename), and detects conflicts via timestamps. Typed sections are derived with `#[derive(TypedSection)]` for compile-time safety.

## Smart Endpoint Migration

The frontend is migrating from directly managing UCI files (via generic `uci.get`/`uci.set`/`exec`) to purpose-built RPC methods:

| Feature | Status |
|---------|--------|
| Auth, System, Profiles, WiFi, Ethernet, Inbound VPN | Smart endpoints exist |
| WAN, LAN, DDNS, Devices, Published Ports, Outbound VPN, SSH Keys | Needs smart endpoints |

See [API_CONTRACT.md](API_CONTRACT.md) for the complete contract with Rust types.

## Key Design Decisions

1. **One SSID, many passwords.** Per-password VLAN assignment. Requires `wpad-wolfssl` (not the default `wpad-basic-mbedtls`).

2. **VLANs for isolation.** Layer 2 isolation via bridge VLAN filtering (DSA). An earlier nftables approach was abandoned because in-hardware switching bypasses the OS.

3. **UCI as source of truth.** No separate database. The backend reads, modifies, and writes UCI files atomically.

4. **Frontend knows nothing about UCI.** The goal is a clean RPC contract where the frontend sends domain objects and the backend handles all UCI serialization, service restarts, and system commands.

5. **Typed UCI parsing.** `#[derive(TypedSection)]` eliminates runtime parsing errors. Define a struct, annotate with `#[uci(ty = "...")]`, derive, done.
