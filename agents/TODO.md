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

  #### 2. User-Controlled Public/Private on Bindings

  There are three distinct concepts for public/private in the system:

  1. **Gateway public/private** (descriptive): A property of the gateway/network interface — whether
     its ports are publicly reachable from the internet without port forwards. This is
     `NetworkInterfaceInfo::public()`.
  2. **Binding public/private** (user intent): Whether the user wants this binding to be reachable from
     the internet. This is a **new** user-controlled field on the binding.
  3. **`public_enabled` / `private_disabled`** (per-gateway overrides): Override the default behavior
     where private gateways are enabled and public gateways are disabled. These sets are per-gateway
     exceptions to that default.

  Add a `public` field to `BindInfo` (or `NetInfo`) representing user intent:

  - **When `public = false`** (default): Block WAN traffic to this binding using source-IP gating
    (see Section 3). The binding is only accessible on the LAN.
  - **When `public = true`**: Allow WAN traffic. Additionally, maintain a port forward mapping in
    patch-db (see Section 4) so the user knows what to configure on their router.

  **New RPC endpoints** (`binding.rs`):

  Following the existing `HostApiKind` pattern, add a new subcommand to the `binding` parent handler:

  - **`set-public`** — Set whether a binding should be publicly accessible.

    ```ts
    interface BindingSetPublicParams {
      internalPort: number
      public: boolean
    }
    ```

    Mutates `BindInfo` to set the `public` field, syncs the host. Uses `sync_db` metadata.

    This yields two RPC methods:
    - `server.host.binding.set-public`
    - `package.host.binding.set-public`

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

  #### 6. SDK and Frontend Changes

  - **SDK**: `preferredExternalPort` is already exposed. No additional SDK changes needed.
  - **Frontend**: Needs to display the port forward mapping from patch-db, showing the user what
    router configuration is required for their public bindings. Also needs UI for toggling the
    `public` field on a binding. Bindings are always private by default.

  ### Key Files

  | File | Role |
  |------|------|
  | `core/src/net/forward.rs` | `AvailablePorts` — port pool allocation |
  | `core/src/net/host/binding.rs` | `BindInfo::new()` / `update()` — port assignment at bind time |
  | `core/src/net/net_controller.rs:259` | `NetServiceData::update()` — vhost/forward/DNS reconciliation, 5443 hack removal |
  | `core/src/net/vhost.rs` | `VHostController` / `ProxyTarget` — source-IP gating for public/private |
  | `core/src/net/gateway.rs` | `PublicFilter`, `InterfaceFilter` — may need refactoring |
  | `sdk/base/lib/interfaces/Host.ts` | SDK `MultiHost.bindPort()` — no changes needed |
  | `core/src/db/model/public.rs` | Public DB model — port forward mapping |

- [ ] Auto-configure port forwards via UPnP/NAT-PMP/PCP - @dr-bonez

  **Blocked by**: "Support preferred external ports besides 443" (must be implemented and tested
  end-to-end first).

  **Goal**: When a binding is marked public, automatically configure port forwards on the user's router
  using UPnP, NAT-PMP, or PCP, instead of requiring manual router configuration. Fall back to
  displaying manual instructions (the port forward mapping from patch-db) when auto-configuration is
  unavailable or fails.
