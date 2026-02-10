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

  #### 1. Preferred Port Allocation for Ownership (`forward.rs`, `binding.rs`)

  Expand `AvailablePorts` to support trying a preferred port before falling back to the dynamic range:

  - Add `try_alloc(port) -> Option<u16>`: Attempts to exclusively allocate a specific port. Returns
    `None` if the port is already allocated or restricted.
  - Enforce the restricted port list (currently noted in `vhost.rs:89`: `<=1024, >=32768, 5355, 5432,
    9050, 6010, 9051, 5353`) — skip the preferred port if restricted, except for ports the OS itself
    uses (80, 443).
  - No SSL-vs-non-SSL distinction or refcounting needed at this layer — ownership is always exclusive.
    SSL port sharing for domains is handled entirely by the VHostController via SNI.

  Modify `BindInfo::new()` and `BindInfo::update()` to attempt the preferred port first:

  ```
  assigned_ssl_port = try_alloc(ssl.preferred_external_port)
                      .unwrap_or(dynamic_pool.alloc())
  assigned_port     = try_alloc(options.preferred_external_port)
                      .unwrap_or(dynamic_pool.alloc())
  ```

  After this change, `assigned_ssl_port` may match the preferred port if it was available, or fall back
  to the dynamic range as before.

  #### 2. Per-Address Enable/Disable (replaces gateway overrides)

  **Current model being removed**: `NetInfo` has `private_disabled: OrdSet<GatewayId>` and
  `public_enabled: OrdSet<GatewayId>` — gateway-level toggles where private gateways are enabled by
  default and public gateways are disabled by default. The `set-gateway-enabled` RPC endpoint and the
  `InterfaceFilter` impl on `NetInfo` use these sets. This model is unintuitive because users think in
  terms of individual addresses, not gateways.

  **New model**: Per-address enable/disable using `DerivedAddressInfo` on `BindInfo`. Instead of
  gateway-level toggles, users toggle individual addresses. The `hostnameInfo` field moves from `Host`
  to `BindInfo.addresses` (as the computed `possible` set).

  **`DerivedAddressInfo` struct** (added to `BindInfo`):

  ```rust
  pub struct DerivedAddressInfo {
      /// User-controlled: private-gateway addresses the user has disabled
      pub private_disabled: BTreeSet<HostnameInfo>,
      /// User-controlled: public-gateway addresses the user has enabled
      pub public_enabled: BTreeSet<HostnameInfo>,
      /// COMPUTED by update(): all possible addresses for this binding
      pub possible: BTreeSet<HostnameInfo>,
  }
  ```

  `DerivedAddressInfo::enabled()` returns `possible` filtered by the two sets: private addresses are
  enabled by default (disabled if in `private_disabled`), public addresses are disabled by default
  (enabled if in `public_enabled`). Requires `HostnameInfo` to derive `Ord` for `BTreeSet` usage.

  **How disabling works per address type**:

  The enforcement mechanism varies by address type because different addresses are reached through
  different network paths:

  - **WAN IP:port** (public gateway IP addresses): Disabled via **source-IP gating** in the vhost
    layer (Section 3). Public and private traffic share the same port listener, so we can't just
    remove the vhost entry — that would also block private traffic. Instead, the vhost target is
    tagged with which source-IP classes it accepts. When a WAN IP address is disabled, the vhost
    target rejects connections whose source IP matches the gateway (i.e., NAT'd internet traffic)
    or falls outside the gateway's LAN subnets. LAN traffic to the same port is unaffected.
  - **LAN IP:port** (private gateway IP addresses): Also enforced via **source-IP gating**. When
    disabled, the vhost target rejects connections from LAN subnets on that gateway. This is the
    inverse of the WAN case — same mechanism, different source-IP class.
  - **Hostname-based addresses** (`.local`, domains): Disabled by **not creating the vhost/SNI
    entry** for that hostname. Since hostname-based routing uses SNI (SSL) or Host header (HTTP),
    removing the entry means the hostname simply doesn't resolve to a backend. No traffic reaches
    the service for that hostname.

  **Backend changes**:

  - **Remove from `NetInfo`**: Delete the `private_disabled` and `public_enabled` fields entirely.
    `NetInfo` becomes just `{ assigned_port: Option<u16>, assigned_ssl_port: Option<u16> }`.
  - **Add `addresses: DerivedAddressInfo` to `BindInfo`**: User-controlled sets (`private_disabled`,
    `public_enabled`) are preserved across updates; `possible` is recomputed by `update()`.
  - **Remove `hostname_info` from `Host`**: Computed addresses now live in `BindInfo.addresses.possible`
    instead of being a top-level field on `Host` that was never persisted to the DB.
  - **Default behavior preserved**: Private-gateway addresses default to enabled, public-gateway
    addresses default to disabled, via the `enabled()` method on `DerivedAddressInfo`.
  - **Remove `set-gateway-enabled`** RPC endpoint from `binding.rs`.
  - **Remove `InterfaceFilter` impl for `NetInfo`**: The per-gateway filter logic is replaced by
    per-address filtering derived from `DerivedAddressInfo`.

  **New RPC endpoint** (`binding.rs`):

  Following the existing `HostApiKind` pattern, replace `set-gateway-enabled` with:

  - **`set-address-enabled`** — Toggle an individual address on or off.

    ```ts
    interface BindingSetAddressEnabledParams {
      internalPort: number
      address: HostnameInfo   // identifies the address directly (no separate AddressId type needed)
      enabled: boolean | null // null = reset to default
    }
    ```

    Mutates `BindInfo.addresses.private_disabled` / `.public_enabled` based on the address's `public`
    field. If `public == true` and enabled, add to `public_enabled`; if disabled, remove. If
    `public == false` and enabled, remove from `private_disabled`; if disabled, add. Uses `sync_db`
    metadata.

    This yields two RPC methods:
    - `server.host.binding.set-address-enabled`
    - `package.host.binding.set-address-enabled`

  #### 3. Eliminate the Port 5443 Hack: Source-IP-Based WAN Blocking (`vhost.rs`, `net_controller.rs`)

  **Current problem**: The `if ssl.preferred_external_port == 443` branch (line 341 of
  `net_controller.rs`) creates a bespoke dual-vhost setup: port 5443 for private-only access and port
  443 for public (or public+private). This exists because both public and private traffic arrive on the
  same port 443 listener, and the current `InterfaceFilter`/`PublicFilter` model distinguishes
  public/private by which *network interface* the connection arrived on — which doesn't work when both
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

  #### 6. Frontend: Interfaces Page Overhaul (View/Manage Split)

  The current interfaces page is a single page showing gateways (with toggle), addresses, public
  domains, and private domains. It gets split into two pages: **View** and **Manage**.

  **SDK**: `preferredExternalPort` is already exposed. No additional SDK changes needed.

  ##### View Page

  Displays all computed addresses for the interface (from `hostname_info`) as a flat list. For each
  address, show: URL, type (IPv4, IPv6, .local, domain), access level (public/private),
  gateway name, SSL indicator, enable/disable state, port forward info for public addresses, and a test button
  for reachability (see Section 7).

  No gateway-level toggles. The old `gateways.component.ts` toggle UI is removed.

  **Note**: Exact UI element placement (where toggles, buttons, info badges go) is sensitive.
  Prompt the user for specific placement decisions during implementation.

  ##### Manage Page

  Simple CRUD interface for configuring which addresses exist. Two sections:

  - **Public domains**: Add/remove. Uses existing RPC endpoints:
    - `{server,package}.host.address.domain.public.add`
    - `{server,package}.host.address.domain.public.remove`
  - **Private domains**: Add/remove. Uses existing RPC endpoints:
    - `{server,package}.host.address.domain.private.add`
    - `{server,package}.host.address.domain.private.remove`

  ##### Key Frontend Files to Modify

  | File | Change |
  |------|--------|
  | `web/projects/ui/src/app/routes/portal/components/interfaces/` | Overhaul: split into view/manage |
  | `web/projects/ui/src/app/routes/portal/components/interfaces/gateways.component.ts` | Remove (replaced by per-address toggles on View page) |
  | `web/projects/ui/src/app/routes/portal/components/interfaces/interface.service.ts` | Update `MappedServiceInterface` to compute enabled addresses from `DerivedAddressInfo` |
  | `web/projects/ui/src/app/routes/portal/components/interfaces/addresses/` | Refactor for View page with overflow menu (enable/disable) and test buttons |
  | `web/projects/ui/src/app/routes/portal/routes/services/services.routes.ts` | Add routes for view/manage sub-pages |
  | `web/projects/ui/src/app/routes/portal/routes/system/system.routes.ts` | Add routes for view/manage sub-pages |

  #### 7. Reachability Test Endpoint

  New RPC endpoint that tests whether an address is actually reachable, with diagnostic info on
  failure.

  **RPC endpoint** (`binding.rs` or new file):

  - **`test-address`** — Test reachability of a specific address.

    ```ts
    interface BindingTestAddressParams {
      internalPort: number
      address: HostnameInfo
    }
    ```

    The backend simply performs the raw checks and returns the results. The **frontend** owns all
    interpretation — it already knows the address type, expected IP, expected port, etc. from the
    `HostnameInfo` data, so it can compare against the backend results and construct fix messaging.

    ```ts
    interface TestAddressResult {
      dns: string[] | null      // resolved IPs, null if not a domain address or lookup failed
      portOpen: boolean | null  // TCP connect result, null if not applicable
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

  | File | Role |
  |------|------|
  | `core/src/net/forward.rs` | `AvailablePorts` — port pool allocation |
  | `core/src/net/host/binding.rs` | `BindInfo`/`NetInfo`/`DerivedAddressInfo` — remove gateway overrides, add per-address enable/disable sets, new RPC endpoints |
  | `core/src/net/net_controller.rs:259` | `NetServiceData::update()` — compute `enabled` on `HostnameInfo`, vhost/forward/DNS reconciliation, 5443 hack removal |
  | `core/src/net/vhost.rs` | `VHostController` / `ProxyTarget` — source-IP gating for public/private |
  | `core/src/net/gateway.rs` | `InterfaceFilter` — remove `NetInfo` impl, simplify |
  | `core/src/net/service_interface.rs` | `HostnameInfo` — add `Ord` derives for use in `BTreeSet` |
  | `core/src/net/host/address.rs` | Existing domain/onion CRUD endpoints (no changes needed) |
  | `sdk/base/lib/interfaces/Host.ts` | SDK `MultiHost.bindPort()` — no changes needed |
  | `core/src/db/model/public.rs` | Public DB model — port forward mapping |

- [ ] Auto-configure port forwards via UPnP/NAT-PMP/PCP - @dr-bonez

  **Blocked by**: "Support preferred external ports besides 443" (must be implemented and tested
  end-to-end first).

  **Goal**: When a binding is marked public, automatically configure port forwards on the user's router
  using UPnP, NAT-PMP, or PCP, instead of requiring manual router configuration. Fall back to
  displaying manual instructions (the port forward mapping from patch-db) when auto-configuration is
  unavailable or fails.
