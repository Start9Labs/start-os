# AI Agent TODOs

Pending tasks for AI agents. Remove items when completed.

## Unreviewed CLAUDE.md Sections

- [ ] Architecture - Web (`/web`) - @MattDHill

## Features

- [ ] Support preferred external ports besides 443 - @dr-bonez

  **Problem**: Currently, port 443 is the only preferred external port that is actually honored. When a
  service requests `preferred_external_port: 8443` (or any non-443 value) for SSL, the system ignores
  the preference and assigns a dynamic-range port (49152-65535). The `preferred_external_port` is only
  used as a label for Tor mappings and as a trigger for the port-443 special case in `update()`.

  **Goal**: Honor `preferred_external_port` for both SSL and non-SSL binds when the requested port is
  available, with proper conflict resolution and fallback to dynamic-range allocation.

  ### Design

  **Key distinction**: There are two separate concepts for SSL port usage:
  1. **Port ownership** (`assigned_ssl_port`) — A port exclusively owned by a binding, allocated from
     `AvailablePorts`. Used for server hostnames (`.local`, mDNS, etc.) and iptables forwards.
  2. **Domain SSL port** — The port used for domain-based vhost entries. A binding does NOT need to own
     a port to have a domain vhost on it. The VHostController already supports multiple hostnames on the
     same port via SNI. Any binding can create a domain vhost entry on any SSL port that the
     VHostController has a listener for, regardless of who "owns" that port.

  For example: the OS owns port 443 as its `assigned_ssl_port`. A service with
  `preferred_external_port: 443` won't get 443 as its `assigned_ssl_port` (it's taken), but it CAN
  still have domain vhost entries on port 443 — SNI routes by hostname.

  #### 1. Preferred Port Allocation for Ownership ✅ DONE

  `AvailablePorts::try_alloc(port) -> Option<u16>` added to `forward.rs`. `BindInfo::new()` and
  `BindInfo::update()` attempt the preferred port first, falling back to dynamic-range allocation.

  #### 2. Per-Address Enable/Disable ✅ DONE

  Gateway-level `private_disabled`/`public_enabled` on `NetInfo` replaced with per-address
  `DerivedAddressInfo` on `BindInfo`. `hostname_info` removed from `Host` — computed addresses now
  live in `BindInfo.addresses.possible`.

  **`DerivedAddressInfo` struct** (on `BindInfo`):

  ```rust
  pub struct DerivedAddressInfo {
      pub private_disabled: BTreeSet<HostnameInfo>,
      pub public_enabled: BTreeSet<HostnameInfo>,
      pub possible: BTreeSet<HostnameInfo>,  // COMPUTED by update()
  }
  ```

  `DerivedAddressInfo::enabled()` returns `possible` filtered by the two sets. `HostnameInfo` derives
  `Ord` for `BTreeSet` usage. `AddressFilter` (implementing `InterfaceFilter`) derives enabled
  gateway set from `DerivedAddressInfo` for vhost/forward filtering.

  **RPC endpoint**: `set-gateway-enabled` replaced with `set-address-enabled` (on both
  `server.host.binding` and `package.host.binding`).

  **How disabling works per address type** (enforcement deferred to Section 3):
  - **WAN/LAN IP:port**: Will be enforced via **source-IP gating** in the vhost layer (Section 3).
  - **Hostname-based addresses** (`.local`, domains): Disabled by **not creating the vhost/SNI
    entry** for that hostname.

  #### 3. Eliminate the Port 5443 Hack: Source-IP-Based WAN Blocking (`vhost.rs`, `net_controller.rs`)

  **Current problem**: The `if ssl.preferred_external_port == 443` branch (line 341 of
  `net_controller.rs`) creates a bespoke dual-vhost setup: port 5443 for private-only access and port
  443 for public (or public+private). This exists because both public and private traffic arrive on the
  same port 443 listener, and the current `InterfaceFilter`/`PublicFilter` model distinguishes
  public/private by which _network interface_ the connection arrived on — which doesn't work when both
  traffic types share a listener.

  **Solution**: Determine public vs private based on **source IP** at the vhost level. Traffic arriving
  from the gateway IP should be treated as public (the gateway may MASQUERADE/NAT internet traffic, so
  anything from the gateway is potentially public). Traffic from LAN IPs is private.

  This applies to **all** vhost targets, not just port 443:
  - **Add a `public` field to `ProxyTarget`** (or an enum: `Public`, `Private`, `Both`) indicating
    what traffic this target accepts, derived from the binding's user-controlled `public` field.
  - **Modify `VHostTarget::filter()`** (`vhost.rs:342`): Instead of (or in addition to) checking the
    network interface via `GatewayInfo`, check the source IP of the TCP connection against known gateway
    IPs. If the source IP matches a gateway or IP outside the subnet, the connection is public;
    otherwise it's private. Use this to gate against the target's `public` field.
  - **Eliminate the 5443 port entirely**: A single vhost entry on port 443 (or any shared SSL port) can
    serve both public and private traffic, with per-target source-IP gating determining which backend
    handles which connections.

  #### 4. Port Forward Mapping in Patch-DB

  When a binding is marked `public = true`, StartOS must record the required port forwards in patch-db
  so the frontend can display them to the user. The user then configures these on their router manually.

  For each public binding, store:
  - The external port the router should forward (the actual vhost port used for domains, or the
    `assigned_port` / `assigned_ssl_port` for non-domain access)
  - The protocol (TCP/UDP)
  - The StartOS LAN IP as the forward target
  - Which service/binding this forward is for (for display purposes)

  This mapping should be in the public database model so the frontend can read and display it.

  #### 5. Simplify `update()` Domain Vhost Logic (`net_controller.rs`)

  With source-IP gating in the vhost controller:
  - **Remove the `== 443` special case** and the 5443 secondary vhost.
  - For **server hostnames** (`.local`, mDNS, embassy, startos, localhost): use `assigned_ssl_port`
    (the port the binding owns).
  - For **domain-based vhost entries**: attempt to use `preferred_external_port` as the vhost port.
    This succeeds if the port is either unused or already has an SSL listener (SNI handles sharing).
    It fails only if the port is already in use by a non-SSL binding, or is a restricted port. On
    failure, fall back to `assigned_ssl_port`.
  - The binding's `public` field determines the `ProxyTarget`'s public/private gating.
  - Hostname info must exactly match the actual vhost port used: for server hostnames, report
    `ssl_port: assigned_ssl_port`. For domains, report `ssl_port: preferred_external_port` if it was
    successfully used for the domain vhost, otherwise report `ssl_port: assigned_ssl_port`.

  #### 6. Reachability Test Endpoint

  New RPC endpoint that tests whether an address is actually reachable, with diagnostic info on
  failure.

  **RPC endpoint** (`binding.rs` or new file):
  - **`test-address`** — Test reachability of a specific address.

    ```ts
    interface BindingTestAddressParams {
      internalPort: number;
      address: HostnameInfo;
    }
    ```

    The backend simply performs the raw checks and returns the results. The **frontend** owns all
    interpretation — it already knows the address type, expected IP, expected port, etc. from the
    `HostnameInfo` data, so it can compare against the backend results and construct fix messaging.

    ```ts
    interface TestAddressResult {
      dns: string[] | null; // resolved IPs, null if not a domain address or lookup failed
      portOpen: boolean | null; // TCP connect result, null if not applicable
    }
    ```

    This yields two RPC methods:
    - `server.host.binding.test-address`
    - `package.host.binding.test-address`

  The frontend already has the full `HostnameInfo` context (expected IP, domain, port, gateway,
  public/private). It compares the backend's raw results against the expected state and constructs
  localized fix instructions. For example:
  - `dns` returned but doesn't contain the expected WAN IP → "Update DNS A record for {domain}
    to {wanIp}"
  - `dns` is `null` for a domain address → "DNS lookup failed for {domain}"
  - `portOpen` is `false` → "Configure port forward on your router: external {port} TCP →
    {lanIp}:{port}"

  ### Key Files

  | File                                 | Role                                                                                                                                                    |
  | ------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------- |
  | `core/src/net/forward.rs`            | `AvailablePorts` — port pool allocation, `try_alloc()` for preferred ports                                                                              |
  | `core/src/net/host/binding.rs`       | `Bindings` (Map wrapper for patchdb), `BindInfo`/`NetInfo`/`DerivedAddressInfo`/`AddressFilter` — per-address enable/disable, `set-address-enabled` RPC |
  | `core/src/net/net_controller.rs:259` | `NetServiceData::update()` — computes `DerivedAddressInfo.possible`, vhost/forward/DNS reconciliation, 5443 hack removal                                |
  | `core/src/net/vhost.rs`              | `VHostController` / `ProxyTarget` — source-IP gating for public/private                                                                                 |
  | `core/src/net/gateway.rs`            | `InterfaceFilter` trait and filter types (`AddressFilter`, `PublicFilter`, etc.)                                                                        |
  | `core/src/net/service_interface.rs`  | `HostnameInfo` — derives `Ord` for `BTreeSet` usage                                                                                                     |
  | `core/src/net/host/address.rs`       | `HostAddress` (flattened struct), domain CRUD endpoints                                                                                                 |
  | `sdk/base/lib/interfaces/Host.ts`    | SDK `MultiHost.bindPort()` — no changes needed                                                                                                          |
  | `core/src/db/model/public.rs`        | Public DB model — port forward mapping                                                                                                                  |

- [ ] Extract TS-exported types into a lightweight sub-crate for fast binding generation

  **Problem**: `make ts-bindings` compiles the entire `start-os` crate (with all dependencies: tokio,
  axum, openssl, etc.) just to run test functions that serialize type definitions to `.ts` files.
  Even in debug mode, this takes minutes. The generated output is pure type info — no runtime code
  is needed.

  **Goal**: Generate TS bindings in seconds by isolating exported types in a small crate with minimal
  dependencies.

  **Approach**: Create a `core/bindings-types/` sub-crate containing (or re-exporting) all 168
  `#[ts(export)]` types. This crate depends only on `serde`, `ts-rs`, `exver`, and other type-only
  crates — not on tokio, axum, openssl, etc. Then `build-ts.sh` runs `cargo test -p bindings-types`
  instead of `cargo test -p start-os`.

  **Challenge**: The exported types are scattered across `core/src/` and reference each other and
  other crate types. Extracting them requires either moving the type definitions into the sub-crate
  (and importing them back into `start-os`) or restructuring to share a common types crate.

- [ ] Use auto-generated RPC types in the frontend instead of manual duplicates

  **Problem**: The web frontend manually defines ~755 lines of API request/response types in
  `web/projects/ui/src/app/services/api/api.types.ts` that can drift from the actual Rust types.

  **Current state**: The Rust backend already has `#[ts(export)]` on RPC param types (e.g.
  `AddTunnelParams`, `SetWifiEnabledParams`, `LoginParams`), and they are generated into
  `core/bindings/`. However, commit `71b83245b` ("Chore/unexport api ts #2585", April 2024)
  deliberately stopped building them into the SDK and had the frontend maintain its own types.

  **Goal**: Reverse that decision — pipe the generated RPC types through the SDK into the frontend
  so `api.types.ts` can import them instead of duplicating them. This eliminates drift between
  backend and frontend API contracts.

- [ ] Auto-configure port forwards via UPnP/NAT-PMP/PCP - @dr-bonez

  **Blocked by**: "Support preferred external ports besides 443" (must be implemented and tested
  end-to-end first).

  **Goal**: When a binding is marked public, automatically configure port forwards on the user's router
  using UPnP, NAT-PMP, or PCP, instead of requiring manual router configuration. Fall back to
  displaying manual instructions (the port forward mapping from patch-db) when auto-configuration is
  unavailable or fails.

- [ ] Decouple createTask from service running state - @dr-bonez

  **Problem**: `createTask` currently depends on the service being in a running state.

  **Goal**: The `input-not-matches` handler in StartOS should queue the task, check it once the
  service is ready, then clear it if it matches. This allows tasks to be created regardless of
  whether the service is currently running.
