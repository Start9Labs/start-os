# StartWRT API Contract

Complete RPC API contract for the StartWRT backend. All endpoints use **JSON-RPC 2.0** over a single HTTP POST endpoint. Request and response types are defined in Rust.

**Goal:** The frontend should never touch UCI files, run shell commands, or read raw files. Every operation goes through a purpose-built RPC method. The backend handles all UCI manipulation, service restarts, and system queries internally.

---

## Shared Types

```rust
/// Identifies a security profile. Used across WiFi, VPN, Ethernet, and Profiles.
#[derive(Serialize, Deserialize)]
struct ProfileId {
    fullname: String,
    interface: String,
    vlan_tag: u16,
}

/// Partial profile identifier for lookups where not all fields are known.
#[derive(Serialize, Deserialize)]
struct ProfileIdOpt {
    fullname: Option<String>,
    interface: Option<String>,
    vlan_tag: Option<u16>,
}

/// A single DNS server entry with protocol info.
#[derive(Serialize, Deserialize)]
struct DnsServer {
    /// IPv4 address of the DNS server
    address: String,
    /// false = plain UDP (port 53), true = DNS-over-HTTPS via SmartDNS
    ssl: bool,
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
enum Protocol {
    Tcp,
    Udp,
    #[serde(rename = "tcp+udp")]
    TcpUdp,
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
enum Theme {
    Dark,
    Light,
    System,
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
enum RemoteAccess {
    Default,
    Never,
    Always,
}
```

---

## 1. Auth

### `auth.login`

```rust
#[derive(Deserialize)]
struct LoginRequest {
    password: String,
}
// Response: null
```

### `auth.logout`

```rust
// Request: {} (empty object)
// Response: null
```

### `auth.set-password`

```rust
#[derive(Deserialize)]
struct SetPasswordRequest {
    old_password: String,
    new_password: String,
}
// Response: null
```

---

## 2. System

### `system.info`

```rust
// Request: {}

#[derive(Serialize)]
struct SystemInfoResponse {
    version: String,
    language: String,
    date: String,  // ISO 8601
    theme: Theme,
}
```

### `system.newer-versions`

```rust
// Request: {}

#[derive(Serialize)]
struct VersionInfo {
    version: String,
    release_notes: String,  // Markdown
}
// Response: Vec<VersionInfo>
```

### `system.restart`

```rust
// Request: {}
// Response: null
// Backend: runs `reboot` internally
```

### `system.set-preferences`

```rust
#[derive(Deserialize)]
struct SetPreferencesRequest {
    language: Option<String>,
    theme: Option<Theme>,
    remote_access: Option<RemoteAccess>,
}
// Response: null
```

### `system.logs`

Non-streaming endpoint for CLI usage. Returns all current log entries.

```rust
// Request: {}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct LogEntry {
    timestamp: String,
    message: String,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct LogsResponse {
    entries: Vec<LogEntry>,
}
// Response: LogsResponse
// Backend: runs `logread`, parses syslog lines
```

### `/api/logs` (WebSocket)

Live-streaming endpoint for the web UI.

1. Client opens WebSocket to `/api/logs`
2. Server spawns `logread -f` (dumps historical entries then follows new ones)
3. Each line is parsed into `LogEntry` and sent as a JSON text frame
4. Connection closes when either side disconnects; child process is killed on drop
5. **Session auth required** — the session cookie is validated before the WebSocket upgrade (returns 401 if invalid)
6. Each message is a single `LogEntry` JSON object (not wrapped in `LogsResponse`)
7. Unparseable lines are silently dropped (same as the RPC endpoint)

---

## 3. WAN

### `wan.ipv4-get`

Returns WAN IPv4 configuration and the actual assigned IP.

```rust
// Request: {}

#[derive(Serialize)]
#[serde(rename_all = "lowercase")]
enum WanIpv4Mode {
    Dhcp,
    Static,
    Pppoe,
}

#[derive(Serialize)]
struct WanIpv4Response {
    mode: WanIpv4Mode,
    /// Actual assigned IP (from ubus/runtime), regardless of mode
    assigned_ip: Option<String>,
    /// Static/PPPoE config fields (populated when relevant)
    address: Option<String>,
    netmask: Option<String>,
    gateway: Option<String>,
    /// PPPoE-specific
    username: Option<String>,
    password: Option<String>,
    device: Option<String>,
}
```

### `wan.ipv4-set`

```rust
#[derive(Deserialize)]
struct WanIpv4SetRequest {
    mode: WanIpv4Mode,
    /// Required when mode = Static
    address: Option<String>,
    netmask: Option<String>,
    gateway: Option<String>,
    /// Required when mode = Pppoe
    username: Option<String>,
    password: Option<String>,
    device: Option<String>,
}
// Response: null
// Backend: updates UCI network.wan, restarts network
```

### `wan.ipv6-get`

```rust
// Request: {}

#[derive(Serialize)]
#[serde(rename_all = "lowercase")]
enum WanIpv6Mode {
    Disabled,
    Slaac,
    Dhcpv6,
    Static,
    #[serde(rename = "6rd")]
    SixRd,
}

#[derive(Serialize)]
struct WanIpv6Response {
    mode: WanIpv6Mode,
    /// Static mode
    address: Option<String>,
    prefix: Option<String>,    // e.g. "/64"
    gateway: Option<String>,
    /// 6RD mode
    peer_ipv4: Option<String>,
    mask: Option<String>,      // e.g. "/32"
    border_relay: Option<String>,
}
```

### `wan.ipv6-set`

```rust
#[derive(Deserialize)]
struct WanIpv6SetRequest {
    mode: WanIpv6Mode,
    address: Option<String>,
    prefix: Option<String>,
    gateway: Option<String>,
    peer_ipv4: Option<String>,
    mask: Option<String>,
    border_relay: Option<String>,
}
// Response: null
// Backend: updates UCI network.wan6, restarts network
```

### `wan.mac-get`

```rust
// Request: {}

#[derive(Serialize)]
#[serde(rename_all = "lowercase")]
enum MacStrategy {
    Router,
    Custom,
}

#[derive(Serialize)]
struct WanMacResponse {
    strategy: MacStrategy,
    /// Current effective MAC address
    mac: String,
    /// The router's default MAC (shown as read-only reference)
    default_mac: String,
}
```

### `wan.mac-set`

```rust
#[derive(Deserialize)]
struct WanMacSetRequest {
    strategy: MacStrategy,
    /// Required when strategy = Custom
    mac: Option<String>,
}
// Response: null
// Backend: updates UCI network.wan macaddr, restarts network
```

### `wan.dns-get`

```rust
// Request: {}

#[derive(Serialize)]
#[serde(rename_all = "lowercase")]
enum DnsMode {
    Isp,
    Custom,
}

#[derive(Serialize)]
struct WanDnsResponse {
    mode: DnsMode,
    /// Populated when mode = Custom
    servers: Vec<DnsServer>,
}
```

### `wan.dns-set`

```rust
#[derive(Deserialize)]
struct WanDnsSetRequest {
    mode: DnsMode,
    /// Required when mode = Custom. Each entry specifies address and protocol.
    servers: Option<Vec<DnsServer>>,
}
// Response: null
// Backend: updates UCI network wan+wan6 DNS settings, restarts network
```

### `wan.ddns-get`

```rust
// Request: {}

#[derive(Serialize)]
#[serde(rename_all = "lowercase")]
enum DdnsProvider {
    Start9,
    Dyndns,
    Noip,
    Cloudflare,
    Duckdns,
    Freedns,
}

#[derive(Serialize)]
struct WanDdnsResponse {
    enabled: bool,
    provider: DdnsProvider,
    /// The resolved/active hostname (auto-detected for Start9, user-configured for others)
    hostname: Option<String>,
    /// Provider-specific fields
    username: Option<String>,
    password: Option<String>,
    token: Option<String>,
    zone: Option<String>,
}
```

### `wan.ddns-set`

```rust
#[derive(Deserialize)]
struct WanDdnsSetRequest {
    enabled: bool,
    provider: DdnsProvider,
    /// Provider-specific fields (which are required depends on provider)
    hostname: Option<String>,
    username: Option<String>,
    password: Option<String>,
    token: Option<String>,
    zone: Option<String>,
}
// Response: null
// Backend: updates UCI ddns config, restarts/stops ddns service
```

---

## 4. LAN

### `lan.ipv4-get`

```rust
// Request: {}

#[derive(Serialize)]
struct LanIpv4Response {
    /// Full router IP, e.g. "192.168.0.1"
    address: String,
    /// Always /16, but include for completeness
    netmask: String,
}
```

### `lan.ipv4-set`

```rust
#[derive(Deserialize)]
struct LanIpv4SetRequest {
    address: String,
}
// Response: null
// Backend: updates UCI network.lan ipaddr, sets netmask to /16, restarts network
```

### `lan.ipv6-get`

```rust
// Request: {}

#[derive(Serialize)]
struct LanIpv6Response {
    slaac: bool,
    dhcpv6: bool,
    /// Prefix delegation length, e.g. 64
    prefix: u8,
    /// Current IPv6 address (if assigned)
    ip6addr: Option<String>,
    /// WAN prefix length (read-only context for the UI)
    wan_prefix: u8,
}
```

### `lan.ipv6-set`

```rust
#[derive(Deserialize)]
struct LanIpv6SetRequest {
    slaac: bool,
    dhcpv6: bool,
    prefix: u8,
}
// Response: null
// Backend: updates UCI dhcp.lan + network.lan, restarts network + odhcpd
```

---

## 5. Ethernet

### `ethernet.get`

```rust
// Request: {}

#[derive(Serialize)]
struct EthernetPort {
    /// Physical port name (e.g. "eth0")
    name: String,
    /// Security profile assigned to this port
    profile: String,
    /// Whether this port is currently the WAN uplink
    wan: bool,
}
// Response: Vec<EthernetPort>
```

### `ethernet.set`

```rust
#[derive(Deserialize)]
struct EthernetSetRequest {
    ports: Vec<EthernetPort>,
}
// Response: null
// Backend: updates UCI network interfaces/devices, restarts network
```

---

## 6. Devices

### `devices.list`

```rust
// Request: {}

#[derive(Serialize)]
#[serde(rename_all = "lowercase")]
enum DeviceStatus {
    Online,
    Offline,
    Blocked,
}

#[derive(Serialize)]
struct Device {
    mac: String,
    /// User-assigned name
    name: Option<String>,
    /// Device-reported hostname
    hostname: Option<String>,
    status: DeviceStatus,
    /// "Ethernet", "Wi-Fi 2.4GHz", "Wi-Fi 5GHz", etc.
    connection: Option<String>,
    ipv4: Option<String>,
    ipv6: Option<String>,
    ipv4_static: bool,
    ipv6_static: bool,
    security_profile: Option<String>,
}
// Response: Vec<Device>
// Backend: reads DHCP hosts, firewall rules, ARP table, DHCP leases
```

### `devices.update`

```rust
#[derive(Deserialize)]
struct DeviceUpdateRequest {
    mac: String,
    name: String,
    ipv4_static: bool,
    ipv4: String,
    ipv6_static: bool,
    ipv6: String,
}
// Response: null
// Backend: creates/updates DHCP host section, restarts dnsmasq
```

### `devices.block`

```rust
#[derive(Deserialize)]
struct DeviceBlockRequest {
    mac: String,
}
// Response: null
// Backend: creates firewall REJECT rule for src_mac, removes static IP, restarts firewall + dnsmasq
```

### `devices.unblock`

```rust
#[derive(Deserialize)]
struct DeviceUnblockRequest {
    mac: String,
}
// Response: null
// Backend: removes firewall block rules for mac, restarts firewall
```

### `devices.forget`

```rust
#[derive(Deserialize)]
struct DeviceForgetRequest {
    mac: String,
}
// Response: null
// Backend: removes DHCP host + firewall rules for mac, restarts firewall + dnsmasq
```

### `devices.data-usage`

```rust
#[derive(Deserialize)]
#[serde(rename_all = "lowercase")]
enum DataUsagePeriod {
    Day,
    Week,
    Month,
    #[serde(rename = "3months")]
    ThreeMonths,
}

#[derive(Deserialize)]
struct DeviceDataUsageRequest {
    mac: String,
    period: DataUsagePeriod,
}

#[derive(Serialize)]
struct DataUsagePoint {
    /// Unix timestamp (seconds)
    timestamp: u64,
    /// Bytes uploaded
    upload: u64,
    /// Bytes downloaded
    download: u64,
}
// Response: Vec<DataUsagePoint>
// Backend: queries nlbwmon
```

---

## 7. Published Ports

### `published-ports.list`

Returns all port forwarding rules with enriched device info and status.

```rust
// Request: {}

#[derive(Serialize)]
#[serde(rename_all = "lowercase")]
enum PublishedPortStatus {
    Active,
    /// IPv4 unavailable (e.g. CGNAT)
    Partial,
    /// Device offline or identity mismatch
    Paused,
    /// Failed to apply rule
    Error,
    Disabled,
}

#[derive(Serialize)]
struct PublishedPort {
    id: String,
    enabled: bool,
    label: String,
    device_mac: String,
    /// Internal port or range, e.g. "8123" or "27015-27030"
    ports: String,
    protocol: Protocol,
    ipv4: bool,
    ipv6: bool,
    /// External port for IPv4 (if different from internal)
    ipv4_public_port: Option<String>,
    /// "any" or CIDR like "203.0.113.0/24"
    source: String,
    // --- Enriched by backend ---
    status: PublishedPortStatus,
    status_reason: Option<String>,
    device_name: Option<String>,
    device_ipv4: Option<String>,
    device_ipv6: Option<String>,
}
// Response: Vec<PublishedPort>
// Backend: reads firewall redirects + rules, resolves device info from DHCP/ARP
```

### `published-ports.set`

Replaces the full list of published ports.

```rust
#[derive(Deserialize)]
struct PublishedPortInput {
    id: String,
    enabled: bool,
    label: String,
    device_mac: String,
    ports: String,
    protocol: Protocol,
    ipv4: bool,
    ipv6: bool,
    ipv4_public_port: Option<String>,
    source: String,
}

#[derive(Deserialize)]
struct PublishedPortsSetRequest {
    ports: Vec<PublishedPortInput>,
}
// Response: null
// Backend: rebuilds firewall redirect+rule sections, resolves device IPs, restarts firewall
```

---

## 8. Outbound VPN (WireGuard Clients)

### `vpn-client.list`

```rust
// Request: {}

#[derive(Serialize)]
struct OutboundVpn {
    /// WireGuard interface name (e.g. "wg_proton")
    id: String,
    label: String,
    /// "Internet" or another VPN's label (for chaining)
    target: String,
    enabled: bool,
    /// Which security profiles route through this VPN
    used_by: Vec<String>,
}
// Response: Vec<OutboundVpn>
```

### `vpn-client.create`

```rust
#[derive(Deserialize)]
struct OutboundVpnCreateRequest {
    label: String,
    target: String,
    /// Raw WireGuard .conf file contents
    config: String,
}

#[derive(Serialize)]
struct OutboundVpnCreateResponse {
    /// The assigned interface ID
    id: String,
}
// Backend: parses WireGuard config, creates UCI interface+peer, restarts network
```

### `vpn-client.update`

```rust
#[derive(Deserialize)]
struct OutboundVpnUpdateRequest {
    id: String,
    label: String,
    target: String,
}
// Response: null
// Backend: updates UCI interface label+target options, restarts network
```

### `vpn-client.delete`

```rust
#[derive(Deserialize)]
struct OutboundVpnDeleteRequest {
    id: String,
}
// Response: null
// Backend: removes UCI interface+peer sections, restarts network
```

### `vpn-client.set-enabled`

```rust
#[derive(Deserialize)]
struct OutboundVpnSetEnabledRequest {
    id: String,
    enabled: bool,
}
// Response: null
// Backend: sets/clears UCI disabled flag, restarts network
```

---

## 9. Inbound VPN (WireGuard Servers)

> These endpoints already exist. Included for completeness.

### `vpn-server.list`

```rust
// Request: {}

#[derive(Serialize)]
struct VpnServer {
    profile: String,
    label: String,
    enabled: bool,
    listen_port: u16,
    endpoint: String,
    public_key: String,
    server_address: String,
    peers: Vec<VpnServerPeer>,
}

#[derive(Serialize)]
struct VpnServerPeer {
    name: String,
    ip: Option<String>,
    public_key: Option<String>,
    preshared_key: Option<String>,
}

#[derive(Serialize)]
struct VpnServerListResponse {
    servers: Vec<VpnServer>,
}
```

### `vpn-server.set`

```rust
#[derive(Deserialize)]
struct VpnServerSetRequest {
    profile: String,
    config: VpnServerConfig,
}

#[derive(Deserialize)]
struct VpnServerConfig {
    label: String,
    enabled: bool,
    listen_port: u16,
    endpoint: String,
    private_key: Option<String>,
}
// Response: null
```

### `vpn-server.delete`

```rust
#[derive(Deserialize)]
struct VpnServerDeleteRequest {
    profile: String,
}
// Response: null
```

### `vpn-server.peer-add`

```rust
#[derive(Deserialize)]
struct VpnServerPeerAddRequest {
    profile: String,
    peer: VpnServerPeerInput,
}

#[derive(Deserialize)]
struct VpnServerPeerInput {
    name: String,
    ip: Option<String>,
    public_key: Option<String>,
    preshared_key: Option<String>,
}

#[derive(Serialize)]
struct VpnServerPeerAddResponse {
    /// WireGuard client config (only when server generates the keypair)
    client_config: Option<String>,
    public_key: String,
    ip: String,
}
```

### `vpn-server.peer-delete`

```rust
#[derive(Deserialize)]
struct VpnServerPeerDeleteRequest {
    profile: String,
    public_key: String,
}
// Response: null
```

---

## 10. WiFi

> These endpoints already exist. Included for completeness.

### `wifi.get`

```rust
// Request: {}

#[derive(Serialize)]
struct WifiRadio {
    band: String,       // "2g", "5g"
    channel: String,    // "auto", "1", "36", etc.
    enabled: bool,
    broadcast: bool,
}

#[derive(Serialize)]
struct WifiPassword {
    label: String,
    profile: Option<ProfileId>,
    password: String,
}

#[derive(Serialize)]
struct WifiConfig {
    ssid: String,
    radios: HashMap<String, WifiRadio>,
    passwords: Vec<WifiPassword>,
}
// Response: WifiConfig
```

### `wifi.set`

```rust
// Request: WifiConfig (same structure as response)
// Response: null
```

### `wifi.blackout-get`

```rust
// Request: {}

#[derive(Serialize)]
struct BlackoutWindow {
    start_time: String,  // "HH:MM"
    end_time: String,    // "HH:MM"
    /// [Sun, Mon, Tue, Wed, Thu, Fri, Sat]
    days: [bool; 7],
}
// Response: Vec<BlackoutWindow>
```

### `wifi.blackout-set`

```rust
#[derive(Deserialize)]
struct BlackoutSetRequest {
    windows: Vec<BlackoutWindow>,
}
// Response: null
```

---

## 11. Security Profiles

> These endpoints already exist. Included for completeness.

### `profiles.list`

```rust
// Request: {}
// Response: Vec<ProfileId>
```

### `profiles.get`

```rust
// Request: ProfileIdOpt

#[derive(Serialize, Deserialize)]
#[serde(untagged)]
enum LanAccess {
    /// "ALL" or "SAME_PROFILE"
    Preset(String),
    /// Explicit list of allowed profiles
    Whitelist { other_profiles: Vec<ProfileIdOpt> },
}

#[derive(Serialize, Deserialize)]
#[serde(untagged)]
enum WanAccess {
    /// "ALL" or "NONE"
    Preset(String),
    Whitelist { whitelist: Vec<String> },
    Blacklist { blacklist: Vec<String> },
}

#[derive(Serialize)]
struct SecurityProfile {
    fullname: String,
    interface: String,
    vlan_tag: u16,
    gateway_ip: String,
    /// "wan" for default WAN, or outbound VPN interface name
    outbound: String,
    lan_access: LanAccess,
    wan_access: WanAccess,
    access_to_new_profiles: bool,
    owns_lan: bool,
    dns_override: Option<Vec<DnsServer>>,
    /// "system", "custom", or "vpn"
    dns_source: String,
}
// Response: SecurityProfile
```

### `profiles.create`

```rust
#[derive(Deserialize)]
struct ProfileCreateRequest {
    fullname: Option<String>,
    interface: Option<String>,
    vlan_tag: Option<u16>,
    gateway_ip: String,
    outbound: String,
    lan_access: LanAccess,
    wan_access: WanAccess,
    access_to_new_profiles: bool,
    owns_lan: bool,
    dns_override: Option<Vec<DnsServer>>,
}
// Response: ProfileId
```

### `profiles.set`

```rust
#[derive(Deserialize)]
struct ProfileUpdateRequest {
    fullname: Option<String>,
    /// Required — identifies the profile to update
    interface: String,
    /// Required — identifies the profile to update
    vlan_tag: u16,
    gateway_ip: String,
    outbound: String,
    lan_access: LanAccess,
    wan_access: WanAccess,
    access_to_new_profiles: bool,
    owns_lan: bool,
    dns_override: Option<Vec<DnsServer>>,
}
// Response: ProfileId
```

### `profiles.delete`

```rust
// Request: ProfileIdOpt
// Response: null
```

---

## 12. SSH Keys

### `ssh-keys.list`

```rust
// Request: {}

#[derive(Serialize)]
struct SshKey {
    /// Full raw line from authorized_keys
    raw: String,
    /// e.g. "ssh-ed25519", "ssh-rsa"
    algorithm: String,
    /// Base64-encoded public key
    public_key: String,
    /// Comment/hostname portion
    hostname: String,
}
// Response: Vec<SshKey>
// Backend: reads /root/.ssh/authorized_keys, parses lines
```

### `ssh-keys.add`

```rust
#[derive(Deserialize)]
struct SshKeyAddRequest {
    /// Full SSH public key line (e.g. "ssh-ed25519 AAAA... user@host")
    key: String,
}
// Response: null
// Backend: appends to /root/.ssh/authorized_keys
```

### `ssh-keys.delete`

```rust
#[derive(Deserialize)]
struct SshKeyDeleteRequest {
    /// The full raw line to remove
    raw: String,
}
// Response: null
// Backend: removes matching line from /root/.ssh/authorized_keys
```

---

## Endpoint Summary

| RPC Method | Status | Category |
|------------|--------|----------|
| `auth.login` | Exists | Auth |
| `auth.logout` | Exists | Auth |
| `auth.set-password` | Exists | Auth |
| `system.info` | Exists | System |
| `system.newer-versions` | Exists | System |
| `system.restart` | Exists | System |
| `system.set-preferences` | Exists | System |
| `system.logs` | Exists | System |
| `/api/logs` (WebSocket) | Exists | System |
| `wan.ipv4-get` | **New** | WAN |
| `wan.ipv4-set` | **New** | WAN |
| `wan.ipv6-get` | **New** | WAN |
| `wan.ipv6-set` | **New** | WAN |
| `wan.mac-get` | **New** | WAN |
| `wan.mac-set` | **New** | WAN |
| `wan.dns-get` | **New** | WAN |
| `wan.dns-set` | **New** | WAN |
| `wan.ddns-get` | **New** | WAN |
| `wan.ddns-set` | **New** | WAN |
| `lan.ipv4-get` | **New** | LAN |
| `lan.ipv4-set` | **New** | LAN |
| `lan.ipv6-get` | **New** | LAN |
| `lan.ipv6-set` | **New** | LAN |
| `ethernet.get` | **New** | Ethernet |
| `ethernet.set` | **New** | Ethernet |
| `devices.list` | **New** | Devices |
| `devices.update` | **New** | Devices |
| `devices.block` | **New** | Devices |
| `devices.unblock` | **New** | Devices |
| `devices.forget` | **New** | Devices |
| `devices.data-usage` | **New** | Devices |
| `published-ports.list` | **New** | Published Ports |
| `published-ports.set` | **New** | Published Ports |
| `vpn-client.list` | **New** | Outbound VPN |
| `vpn-client.create` | **New** | Outbound VPN |
| `vpn-client.update` | **New** | Outbound VPN |
| `vpn-client.delete` | **New** | Outbound VPN |
| `vpn-client.set-enabled` | **New** | Outbound VPN |
| `vpn-server.list` | Exists | Inbound VPN |
| `vpn-server.set` | Exists | Inbound VPN |
| `vpn-server.delete` | Exists | Inbound VPN |
| `vpn-server.peer-add` | Exists | Inbound VPN |
| `vpn-server.peer-delete` | Exists | Inbound VPN |
| `wifi.get` | Exists | WiFi |
| `wifi.set` | Exists | WiFi |
| `wifi.blackout-get` | Exists | WiFi |
| `wifi.blackout-set` | Exists | WiFi |
| `profiles.list` | Exists | Profiles |
| `profiles.get` | Exists | Profiles |
| `profiles.create` | Exists | Profiles |
| `profiles.set` | Exists | Profiles |
| `profiles.delete` | Exists | Profiles |
| `ssh-keys.list` | **New** | SSH Keys |
| `ssh-keys.add` | **New** | SSH Keys |
| `ssh-keys.delete` | **New** | SSH Keys |

**Totals:** 53 endpoints (22 existing, 31 new)

---

## Deprecated Endpoints

The following generic endpoints should be removed from the frontend once all smart endpoints are implemented:

| Endpoint | Replaced by |
|----------|-------------|
| `uci.get` | All `*.get` endpoints above |
| `uci.set` | All `*.set` endpoints above |
| `exec` | Absorbed into smart endpoints internally |
| `file.get` | `ssh-keys.list` |
| `file.set` | `ssh-keys.add`, `ssh-keys.delete` |
