import { z } from 'zod'
const wire = <A extends z.ZodType, B extends z.ZodType>(input: A, output: B) =>
  z.codec(input, output, { decode: v => v as never, encode: v => v as never })
export const ActivityDeleteParams = z.object({ id: z.int() }).meta({
  id: 'ActivityDeleteParams',
})
export const ActivityEntry = z
  .object({
    id: z.int(),
    timestamp: z.string(),
    category: z.string(),
    action: z.string(),
    success: z.boolean(),
    summary: z.string(),
    error: z.string().nullable().optional(),
  })
  .meta({
    id: 'ActivityEntry',
  })
export const ActivityListParams = z
  .object({
    offset: wire(z.int().nullable().optional(), z.int().nullable()),
    limit: wire(z.int().nullable().optional(), z.int().nullable()),
  })
  .meta({
    id: 'ActivityListParams',
  })

/**
 * A published port that will be removed because the device it forwards to can
 * no longer be reached at its current address — its security profile is changing
 * (ethernet-port or WiFi-password reassignment) or its WiFi password is being
 * deleted (disconnecting it) — so its DNAT rule, bound to the device's old
 * subnet, would otherwise break. Surfaced to the UI to confirm the deletion.
 */
export const AffectedPublishedPort = z
  .object({
    id: z.string(),
    label: z.string(),
    device_mac: z.string(),
    device_name: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'AffectedPublishedPort',
    description:
      "A published port that will be removed because the device it forwards to can\nno longer be reached at its current address — its security profile is changing\n(ethernet-port or WiFi-password reassignment) or its WiFi password is being\ndeleted (disconnecting it) — so its DNAT rule, bound to the device's old\nsubnet, would otherwise break. Surfaced to the UI to confirm the deletion.",
  })
export const ApChannel = z
  .object({
    channel: z.int(),
    /**
     * Widths in MHz whose every bonded channel is usable.
     */
    widths: z.array(z.int()).meta({
      description: 'Widths in MHz whose every bonded channel is usable.',
    }),
  })
  .meta({
    id: 'ApChannel',
  })

/**
 * One automatic port use for the published-ports UI.
 */
export const AutomaticPortUse = z
  .object({
    /**
     * UCI section name (`apf_<mac>_<extport>`).
     */
    id: z.string().meta({
      description: 'UCI section name (`apf_<mac>_<extport>`).',
    }),
    /**
     * Which mechanism created it ("PCP", "UPnP", or "SNI").
     */
    kind: z.string().meta({
      description: 'Which mechanism created it ("PCP", "UPnP", or "SNI").',
    }),
    device_mac: z.string(),
    device_name: wire(z.string().nullable().optional(), z.string().nullable()),
    /**
     * Forward target address on the LAN.
     */
    internal_ip: wire(
      z.string().nullable().optional(),
      z.string().nullable(),
    ).meta({
      description: 'Forward target address on the LAN.',
    }),
    /**
     * Internal port (or range).
     */
    ports: z.string().meta({
      description: 'Internal port (or range).',
    }),
    /**
     * External port (or range) on the WAN.
     */
    public_ports: z.string().meta({
      description: 'External port (or range) on the WAN.',
    }),
    /**
     * Seconds until the lease expires if not renewed (None when the daemon
     * isn't tracking it yet, e.g. right after boot).
     */
    expires_secs: wire(z.int().nullable().optional(), z.int().nullable()).meta({
      description:
        "Seconds until the lease expires if not renewed (None when the daemon\nisn't tracking it yet, e.g. right after boot).",
    }),
    /**
     * Hostname for an SNI route; absent for plain forwards.
     */
    hostname: wire(
      z.string().nullable().optional(),
      z.string().nullable(),
    ).meta({
      description: 'Hostname for an SNI route; absent for plain forwards.',
    }),
  })
  .meta({
    id: 'AutomaticPortUse',
    description: 'One automatic port use for the published-ports UI.',
  })
export const BlackoutWindow = z
  .object({
    startTime: z.string(),
    endTime: z.string(),
    days: z.tuple([
      z.boolean(),
      z.boolean(),
      z.boolean(),
      z.boolean(),
      z.boolean(),
      z.boolean(),
      z.boolean(),
    ]),
  })
  .meta({
    id: 'BlackoutWindow',
  })
export const CheckInitializedRes = z.object({ initialized: z.boolean() }).meta({
  id: 'CheckInitializedRes',
})
export const DataUsagePeriod = z.enum(['week', 'month', '3months']).meta({
  id: 'DataUsagePeriod',
})
export const DataUsagePoint = z
  .object({ timestamp: z.int(), upload: z.int(), download: z.int() })
  .meta({
    id: 'DataUsagePoint',
  })
export const DdnsProvider = z
  .enum(['dyndns', 'noip', 'cloudflare', 'duckdns', 'freedns'])
  .meta({
    id: 'DdnsProvider',
  })
export const DeleteArgs = z
  .object({
    /**
     * Profile interface name
     */
    profile: z.string().meta({
      description: 'Profile interface name',
    }),
  })
  .meta({
    id: 'DeleteArgs',
  })
export const DeviceMacReq = z.object({ mac: z.string() }).meta({
  id: 'DeviceMacReq',
})
export const DeviceStatus = z.enum(['online', 'offline']).meta({
  id: 'DeviceStatus',
})
export const DeviceUpdateReq = z
  .object({
    mac: z.string(),
    /**
     * Absent leaves the assigned name untouched; empty clears it. Otherwise
     * it must pass [`validate_device_name`].
     */
    name: wire(z.string().nullable().optional(), z.string().nullable()).meta({
      description:
        'Absent leaves the assigned name untouched; empty clears it. Otherwise\nit must pass [`validate_device_name`].',
    }),
    ipv4_static: z.boolean(),
    ipv4: z.string(),
  })
  .meta({
    id: 'DeviceUpdateReq',
  })
export const DirGetArgs = z.object({ path: z.string() }).meta({
  id: 'DirGetArgs',
})
export const DiskState = z
  .object({
    /**
     * An eMMC device was found.
     */
    emmcFound: z.boolean().meta({
      description: 'An eMMC device was found.',
    }),
    /**
     * The eMMC has existing firmware (rootfs partition present).
     */
    hasFirmware: z.boolean().meta({
      description: 'The eMMC has existing firmware (rootfs partition present).',
    }),
  })
  .meta({
    id: 'DiskState',
  })
export const DnsMode = z.enum(['isp', 'custom']).meta({
  id: 'DnsMode',
})

/**
 * A single DNS server entry with protocol info.
 * `ssl: false` = plain UDP on port 53.
 * `ssl: true`  = DNS-over-HTTPS via SmartDNS (`server-https`).
 */
export const DnsServer = z
  .object({ address: z.string(), ssl: z.boolean() })
  .meta({
    id: 'DnsServer',
    description:
      'A single DNS server entry with protocol info.\n`ssl: false` = plain UDP on port 53.\n`ssl: true`  = DNS-over-HTTPS via SmartDNS (`server-https`).',
  })
export const ExecReq = z
  .object({
    command: z.string(),
    args: z.array(z.string()),
    /**
     * Timeout in milliseconds
     */
    timeout: z.int().meta({
      description: 'Timeout in milliseconds',
    }),
  })
  .meta({
    id: 'ExecReq',
  })
export const ExecRes = z
  .object({ stdout: z.string(), stderr: z.string(), exitCode: z.int() })
  .meta({
    id: 'ExecRes',
  })
export const FileContents = z
  .object({ contents: z.string(), modified: z.string() })
  .meta({
    id: 'FileContents',
  })
export const FileType = z
  .enum([
    'regular-file',
    'directory',
    'symlink',
    'block-device',
    'char-device',
    'fifo',
    'socket',
  ])
  .meta({
    id: 'FileType',
  })
export const GetArgs = z.object({ names: z.array(z.string()) }).meta({
  id: 'GetArgs',
})
export const GetFileArgs = z.object({ path: z.string() }).meta({
  id: 'GetFileArgs',
})

/**
 * Unguessable random token, used as a one-time REST endpoint path segment.
 * Wraps `startos::util::new_guid()` (160-bit, base32-encoded, 32 chars).
 */
export const Guid = z.string().meta({
  id: 'Guid',
  description:
    'Unguessable random token, used as a one-time REST endpoint path segment.\nWraps `startos::util::new_guid()` (160-bit, base32-encoded, 32 chars).',
})
export const InjectedDnsRecord = z
  .object({
    name: z.string(),
    rtype: z.string(),
    value: z.string(),
    ttl: z.int(),
    /**
     * The injecting device's address, when known.
     */
    source: wire(z.string().nullable().optional(), z.string().nullable()).meta({
      description: "The injecting device's address, when known.",
    }),
    /**
     * Owning LAN device MAC (uppercase); absent for a WireGuard peer.
     */
    owner_mac: wire(
      z.string().nullable().optional(),
      z.string().nullable(),
    ).meta({
      description:
        'Owning LAN device MAC (uppercase); absent for a WireGuard peer.',
    }),
    /**
     * Owning inbound-VPN peer public key; absent for a LAN device.
     */
    owner_peer: wire(
      z.string().nullable().optional(),
      z.string().nullable(),
    ).meta({
      description:
        'Owning inbound-VPN peer public key; absent for a LAN device.',
    }),
    /**
     * Display name of the owning LAN device, when one is known.
     */
    device_name: wire(
      z.string().nullable().optional(),
      z.string().nullable(),
    ).meta({
      description: 'Display name of the owning LAN device, when one is known.',
    }),
    /**
     * Profile interface whose subnet the record was injected from.
     */
    profile: wire(z.string().nullable().optional(), z.string().nullable()).meta(
      {
        description:
          'Profile interface whose subnet the record was injected from.',
      },
    ),
  })
  .meta({
    id: 'InjectedDnsRecord',
  })
export const IoFormat = z
  .enum(['json', 'json-pretty', 'yaml', 'cbor', 'toml', 'toml-pretty'])
  .meta({
    id: 'IoFormat',
  })
export const LanIpv4Response = z
  .object({ address: z.string(), netmask: z.string() })
  .meta({
    id: 'LanIpv4Response',
  })
export const LanIpv4SetRequest = z
  .object({
    address: z.string(),
    /**
     * When true, forcibly delete VPN peers that would break due to block change.
     */
    force: wire(z.boolean().optional(), z.boolean()).meta({
      description:
        'When true, forcibly delete VPN peers that would break due to block change.',
    }),
  })
  .meta({
    id: 'LanIpv4SetRequest',
  })
export const LanIpv6Response = z
  .object({
    slaac: z.boolean(),
    dhcpv6: z.boolean(),
    /**
     * Prefix delegation length, e.g. 64
     */
    prefix: z.int().meta({
      description: 'Prefix delegation length, e.g. 64',
    }),
    /**
     * Current IPv6 address (if assigned)
     */
    ip6addr: wire(z.string().nullable().optional(), z.string().nullable()).meta(
      {
        description: 'Current IPv6 address (if assigned)',
      },
    ),
    /**
     * WAN prefix length (read-only context for the UI)
     */
    wan_prefix: z.int().meta({
      description: 'WAN prefix length (read-only context for the UI)',
    }),
  })
  .meta({
    id: 'LanIpv6Response',
  })
export const LanIpv6SetRequest = z
  .object({ slaac: z.boolean(), dhcpv6: z.boolean(), prefix: z.int() })
  .meta({
    id: 'LanIpv6SetRequest',
  })
export const LogEntry = z
  .object({ timestamp: z.string(), message: z.string() })
  .meta({
    id: 'LogEntry',
  })
export const LoginParams = z
  .object({
    password: z.string(),
    userAgent: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'LoginParams',
  })
export const LoginRes = z.object({ session: z.string() }).meta({
  id: 'LoginRes',
})
export const LogoutParams = z
  .object({
    /**
     * Session hash injected by auth middleware
     */
    sessionHash: wire(
      z.string().nullable().optional(),
      z.string().nullable(),
    ).meta({
      description: 'Session hash injected by auth middleware',
    }),
  })
  .meta({
    id: 'LogoutParams',
  })
export const MacStrategy = z.enum(['router', 'custom']).meta({
  id: 'MacStrategy',
})
export const OutboundVpn = z
  .object({
    id: z.string(),
    label: z.string(),
    target: z.string(),
    enabled: z.boolean(),
    used_by: z.array(z.string()),
    /**
     * True when the VPN's WireGuard interface has at least one IPv6 address;
     * false means profiles routed through this VPN will have IPv6 disabled
     * (per `outbound_supports_ipv6` in profiles.rs).
     */
    supports_ipv6: z.boolean().meta({
      description:
        "True when the VPN's WireGuard interface has at least one IPv6 address;\nfalse means profiles routed through this VPN will have IPv6 disabled\n(per `outbound_supports_ipv6` in profiles.rs).",
    }),
    /**
     * Interface MTU, if explicitly set. `None` means the kernel default
     * (1420 on a clean path) is in effect. Editable via `update`.
     */
    mtu: wire(z.int().nullable().optional(), z.int().nullable()).meta({
      description:
        'Interface MTU, if explicitly set. `None` means the kernel default\n(1420 on a clean path) is in effect. Editable via `update`.',
    }),
    /**
     * The server is named by hostname; only `Internet` is a valid target.
     */
    hostname_endpoint: z.boolean().meta({
      description:
        'The server is named by hostname; only `Internet` is a valid target.',
    }),
  })
  .meta({
    id: 'OutboundVpn',
  })
export const OutboundVpnCreateRequest = z
  .object({
    label: z.string(),
    target: z.string(),
    /**
     * Raw WireGuard .conf file contents
     */
    config: z.string().meta({
      description: 'Raw WireGuard .conf file contents',
    }),
  })
  .meta({
    id: 'OutboundVpnCreateRequest',
  })
export const OutboundVpnCreateResponse = z.object({ id: z.string() }).meta({
  id: 'OutboundVpnCreateResponse',
})
export const OutboundVpnDeleteRequest = z.object({ id: z.string() }).meta({
  id: 'OutboundVpnDeleteRequest',
})
export const OutboundVpnSetEnabledRequest = z
  .object({ id: z.string(), enabled: z.boolean() })
  .meta({
    id: 'OutboundVpnSetEnabledRequest',
  })
export const OutboundVpnUpdateRequest = z
  .object({
    id: z.string(),
    label: z.string(),
    target: z.string(),
    /**
     * Desired interface MTU. `None` (or absent) restores the default: the
     * chain MTU for a VPN connecting through another, else the kernel's.
     */
    mtu: wire(z.int().nullable().optional(), z.int().nullable()).meta({
      description:
        "Desired interface MTU. `None` (or absent) restores the default: the\nchain MTU for a VPN connecting through another, else the kernel's.",
    }),
  })
  .meta({
    id: 'OutboundVpnUpdateRequest',
  })

/**
 * Response from adding a peer, contains client config if keys were generated
 */
export const PeerAddResponse = z
  .object({
    /**
     * WireGuard client configuration (only present if keys were generated server-side)
     */
    client_config: z.string().nullable().optional().meta({
      description:
        'WireGuard client configuration (only present if keys were generated server-side)',
    }),
    /**
     * The public key of the peer
     */
    public_key: z.string().meta({
      description: 'The public key of the peer',
    }),
    /**
     * The assigned IP address
     */
    ip: z.string().meta({
      description: 'The assigned IP address',
    }),
  })
  .meta({
    id: 'PeerAddResponse',
    description:
      'Response from adding a peer, contains client config if keys were generated',
  })
export const PeerDeleteArgs = z
  .object({
    /**
     * Profile interface name
     */
    profile: z.string().meta({
      description: 'Profile interface name',
    }),
    /**
     * Public key of the peer to delete
     */
    public_key: z.string().meta({
      description: 'Public key of the peer to delete',
    }),
  })
  .meta({
    id: 'PeerDeleteArgs',
  })
export const ProfileId = z
  .object({ fullname: z.string(), interface: z.string(), vlan_tag: z.int() })
  .meta({
    id: 'ProfileId',
  })
export const ProfileIdOpt = z
  .object({
    fullname: wire(z.string().nullable().optional(), z.string().nullable()),
    interface: wire(z.string().nullable().optional(), z.string().nullable()),
    vlan_tag: wire(z.int().nullable().optional(), z.int().nullable()),
  })
  .meta({
    id: 'ProfileIdOpt',
  })
export const Protocol = z.enum(['tcp', 'udp', 'tcp+udp']).meta({
  id: 'Protocol',
})
export const PublishedPortStatus = z
  .enum(['active', 'partial', 'paused', 'error', 'disabled'])
  .meta({
    id: 'PublishedPortStatus',
  })
export const ResetPasswordParams = z
  .object({ oldPassword: z.string(), newPassword: z.string() })
  .meta({
    id: 'ResetPasswordParams',
  })
export const ScheduleGetParams = z.object({ interface: z.string() }).meta({
  id: 'ScheduleGetParams',
})
export const ScheduleWindow = z
  .object({
    startTime: z.string(),
    endTime: z.string(),
    days: z.tuple([
      z.boolean(),
      z.boolean(),
      z.boolean(),
      z.boolean(),
      z.boolean(),
      z.boolean(),
      z.boolean(),
    ]),
  })
  .meta({
    id: 'ScheduleWindow',
  })
export const Section = z
  .object({
    ty: z.string(),
    name: wire(z.string().nullable().optional(), z.string().nullable()),
    options: z.record(z.string(), z.string()),
    lists: z.record(z.string(), z.array(z.string())),
  })
  .meta({
    id: 'Section',
  })
export const SetAutoForwardRequest = z
  .object({ mac: z.string(), allow: z.boolean() })
  .meta({
    id: 'SetAutoForwardRequest',
  })
export const SetDnsInjectionReq = z
  .object({ mac: z.string(), allow: z.boolean() })
  .meta({
    id: 'SetDnsInjectionReq',
  })
export const SetFileArgs = z
  .object({
    path: z.string(),
    contents: z.string(),
    modified: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'SetFileArgs',
  })
export const SetInitialPasswordParams = z
  .object({ password: z.string() })
  .meta({
    id: 'SetInitialPasswordParams',
  })
export const SetPreferencesReq = z
  .object({
    language: wire(z.string().nullable().optional(), z.string().nullable()),
    theme: wire(z.string().nullable().optional(), z.string().nullable()),
    remoteAccess: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'SetPreferencesReq',
  })
export const SetTimezoneParams = z
  .object({
    /**
     * IANA timezone name, e.g. "America/New_York"
     */
    timezone: z.string().meta({
      description: 'IANA timezone name, e.g. "America/New_York"',
    }),
  })
  .meta({
    id: 'SetTimezoneParams',
  })
export const SniPortUse = z
  .object({
    ports: z.string(),
    hostnames: z.array(z.string()),
    devices: z.array(z.string()),
  })
  .meta({
    id: 'SniPortUse',
  })
export const SpeedData = z
  .object({
    up: wire(z.number(), z.number().nullable()),
    down: wire(z.number(), z.number().nullable()),
  })
  .meta({
    id: 'SpeedData',
  })
export const SshKeyAddParams = z.object({ key: z.string() }).meta({
  id: 'SshKeyAddParams',
})
export const SshKeyDeleteParams = z.object({ fingerprint: z.string() }).meta({
  id: 'SshKeyDeleteParams',
})
export const SshKeyResponse = z
  .object({
    algorithm: z.string(),
    fingerprint: z.string(),
    hostname: z.string(),
  })
  .meta({
    id: 'SshKeyResponse',
  })
export const SystemInfoResponse = z
  .object({
    version: z.string(),
    gitHash: z.string(),
    language: z.string(),
    date: z.string(),
    theme: z.string(),
    remoteAccess: z.string(),
    timezone: z.string(),
  })
  .meta({
    id: 'SystemInfoResponse',
  })
export const UpdateSystemParams = z
  .object({
    registry: wire(z.string().nullable().optional(), z.string().nullable()),
    targetVersion: wire(
      z.string().nullable().optional(),
      z.string().nullable(),
    ),
  })
  .meta({
    id: 'UpdateSystemParams',
  })
export const UpdateSystemRes = z
  .object({
    target: wire(z.string().nullable().optional(), z.string().nullable()),
    progress: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'UpdateSystemRes',
  })
export const VerifyPasswordParams = z.object({ password: z.string() }).meta({
  id: 'VerifyPasswordParams',
})
export const VersionInfo = z
  .object({ version: z.string(), releaseNotes: z.string() })
  .meta({
    id: 'VersionInfo',
  })

/**
 * VPN server configuration for set/create requests
 */
export const VpnServerConfig = z
  .object({
    /**
     * Human-readable label for the VPN server
     */
    label: z.string().meta({
      description: 'Human-readable label for the VPN server',
    }),
    /**
     * Whether the VPN server is enabled (listening)
     */
    enabled: z.boolean().meta({
      description: 'Whether the VPN server is enabled (listening)',
    }),
    /**
     * Listen port for WireGuard connections
     */
    listen_port: z.int().meta({
      description: 'Listen port for WireGuard connections',
    }),
    /**
     * Public endpoint address for inbound client connections (hostname or IP)
     */
    endpoint: z.string().meta({
      description:
        'Public endpoint address for inbound client connections (hostname or IP)',
    }),
    /**
     * Private key. If not provided, a new key will be generated.
     */
    private_key: z.string().nullable().optional().meta({
      description: 'Private key. If not provided, a new key will be generated.',
    }),
  })
  .meta({
    id: 'VpnServerConfig',
    description: 'VPN server configuration for set/create requests',
  })

/**
 * Peer configuration for VPN server
 */
export const VpnServerPeer = z
  .object({
    /**
     * Human-readable name for the peer
     */
    name: z.string().meta({
      description: 'Human-readable name for the peer',
    }),
    /**
     * Assigned IP address within the profile's subnet (auto-allocated if not specified)
     */
    ip: z.string().nullable().optional().meta({
      description:
        "Assigned IP address within the profile's subnet (auto-allocated if not specified)",
    }),
    /**
     * Public key. If not provided when adding a peer, a key pair will be generated.
     */
    public_key: z.string().nullable().optional().meta({
      description:
        'Public key. If not provided when adding a peer, a key pair will be generated.',
    }),
    /**
     * Pre-shared key (auto-generated if not provided)
     */
    preshared_key: z.string().nullable().optional().meta({
      description: 'Pre-shared key (auto-generated if not provided)',
    }),
    /**
     * Route all traffic (LAN + WAN) through the tunnel. Default (false/absent) = split tunnel (LAN only).
     */
    route_all: z.boolean().nullable().optional().meta({
      description:
        'Route all traffic (LAN + WAN) through the tunnel. Default (false/absent) = split tunnel (LAN only).',
    }),
  })
  .meta({
    id: 'VpnServerPeer',
    description: 'Peer configuration for VPN server',
  })
export const WanAccess = z
  .union([
    z.literal('ALL'),
    z.literal('NONE'),
    z.object({ whitelist: z.array(z.string()) }),
    z.object({ blacklist: z.array(z.string()) }),
  ])
  .meta({
    id: 'WanAccess',
  })
export const WanIpv4Mode = z.enum(['dhcp', 'static', 'pppoe']).meta({
  id: 'WanIpv4Mode',
})
export const WanIpv6Mode = z
  .enum(['disabled', 'slaac', 'dhcpv6', 'static', '6rd'])
  .meta({
    id: 'WanIpv6Mode',
  })
export const WifiRadio = z
  .object({
    band: z.string(),
    channel: z.string(),
    /**
     * Channel width in MHz. Null when the radio's mode sets none; null on
     * `set` keeps the current width.
     */
    width: wire(z.int().nullable().optional(), z.int().nullable()).meta({
      description:
        "Channel width in MHz. Null when the radio's mode sets none; null on\n`set` keeps the current width.",
    }),
    enabled: z.boolean(),
    broadcast: z.boolean(),
  })
  .meta({
    id: 'WifiRadio',
  })
export const ActivityListResponse = z
  .object({ entries: z.array(ActivityEntry), total: z.int() })
  .meta({
    id: 'ActivityListResponse',
  })
export const BackupCreateRes = z
  .object({ guid: Guid, filename: z.string() })
  .meta({
    id: 'BackupCreateRes',
  })
export const BackupRestoreRes = z.object({ upload: Guid }).meta({
  id: 'BackupRestoreRes',
})
export const BandChannels = z
  .object({
    channels: z.array(ApChannel),
    /**
     * Widths automatic channel selection can serve without radar-detection channels.
     */
    autoWidths: z.array(z.int()).meta({
      description:
        'Widths automatic channel selection can serve without radar-detection channels.',
    }),
  })
  .meta({
    id: 'BandChannels',
  })
export const BlackoutWindows = z
  .object({ windows: z.array(BlackoutWindow) })
  .meta({
    id: 'BlackoutWindows',
  })
export const DataUsageReq = z
  .object({ mac: z.string(), period: DataUsagePeriod })
  .meta({
    id: 'DataUsageReq',
  })
export const Device = z
  .object({
    mac: wire(z.string().nullable().optional(), z.string().nullable()),
    /**
     * Fully-resolved display name: UCI static name → live DHCP hostname →
     * remembered hostname (cache) → `device-<mac>` placeholder.
     */
    name: z.string().meta({
      description:
        'Fully-resolved display name: UCI static name → live DHCP hostname →\nremembered hostname (cache) → `device-<mac>` placeholder.',
    }),
    /**
     * The UCI static name; `None` when `name` is resolved from elsewhere.
     * What the rename form edits.
     */
    custom_name: wire(
      z.string().nullable().optional(),
      z.string().nullable(),
    ).meta({
      description:
        'The UCI static name; `None` when `name` is resolved from elsewhere.\nWhat the rename form edits.',
    }),
    /**
     * Raw DHCP lease hostname (may be "*").
     */
    hostname: wire(
      z.string().nullable().optional(),
      z.string().nullable(),
    ).meta({
      description: 'Raw DHCP lease hostname (may be "*").',
    }),
    status: DeviceStatus,
    connection: wire(z.string().nullable().optional(), z.string().nullable()),
    ipv4: wire(z.string().nullable().optional(), z.string().nullable()),
    ipv6: wire(z.string().nullable().optional(), z.string().nullable()),
    ipv4_static: z.boolean(),
    /**
     * Whether this device may auto-create port forwards via PCP/UPnP
     * (default off; set via `devices set-auto-forward`).
     */
    allow_auto_port_forward: z.boolean().meta({
      description:
        'Whether this device may auto-create port forwards via PCP/UPnP\n(default off; set via `devices set-auto-forward`).',
    }),
    /**
     * Whether this device may publish DNS records into the router's resolver
     * (`devices set-dns-injection`).
     */
    allow_dns_injection: z.boolean().meta({
      description:
        "Whether this device may publish DNS records into the router's resolver\n(`devices set-dns-injection`).",
    }),
    security_profile: wire(
      z.string().nullable().optional(),
      z.string().nullable(),
    ),
    speed: wire(SpeedData.nullable().optional(), SpeedData.nullable()),
    data_usage: wire(
      wire(z.number(), z.number().nullable()).nullable().optional(),
      wire(z.number(), z.number().nullable()).nullable(),
    ),
  })
  .meta({
    id: 'Device',
  })
export const DiagnosticsCreateRes = z
  .object({ guid: Guid, filename: z.string() })
  .meta({
    id: 'DiagnosticsCreateRes',
  })
export const DirEntry = z
  .object({
    name: z.string(),
    size: z.int(),
    blocks: z.int(),
    io_block: z.int(),
    file_type: FileType,
    device: z.int(),
    inode: z.int(),
    links: z.int(),
    mode: z.int(),
    uid: z.int(),
    gid: z.int(),
    access: z.string(),
    modify: z.string(),
    change: z.string(),
  })
  .meta({
    id: 'DirEntry',
  })
export const EditArgs = z
  .object({ get: ProfileIdOpt, create: z.boolean() })
  .meta({
    id: 'EditArgs',
  })

/**
 * `ethernet.set` request: the desired port layout plus a confirmation flag.
 * Reassigning a port to a different profile moves its devices to a new subnet,
 * which breaks any published ports forwarding to them. When the flag is false
 * and such ports exist, `set` applies nothing and returns them in
 * `EthernetSetResult` for a confirmation dialog; with the flag true it deletes
 * those published ports as part of the reassignment.
 */
export const EthernetSetRequest = z
  .intersection(
    z.object({
      confirm_published_port_deletion: wire(
        z.boolean().optional(),
        z.boolean(),
      ),
    }),
    z.object({
      wan_ipv6: z.boolean(),
      wan_port: wire(z.string().nullable().optional(), z.string().nullable()),
      ports: z.record(
        z.string(),
        z.object({
          profile: wire(
            ProfileIdOpt.nullable().optional(),
            ProfileIdOpt.nullable(),
          ),
        }),
      ),
    }),
  )
  .meta({
    id: 'EthernetSetRequest',
    description:
      '`ethernet.set` request: the desired port layout plus a confirmation flag.\nReassigning a port to a different profile moves its devices to a new subnet,\nwhich breaks any published ports forwarding to them. When the flag is false\nand such ports exist, `set` applies nothing and returns them in\n`EthernetSetResult` for a confirmation dialog; with the flag true it deletes\nthose published ports as part of the reassignment.',
  })
export const EthernetSetResult = z
  .object({
    /**
     * Non-empty (and nothing applied) when published ports would be deleted and
     * the caller hasn't confirmed yet. Empty once the change is applied.
     */
    pending_published_port_deletions: z.array(AffectedPublishedPort).meta({
      description:
        "Non-empty (and nothing applied) when published ports would be deleted and\nthe caller hasn't confirmed yet. Empty once the change is applied.",
    }),
  })
  .meta({
    id: 'EthernetSetResult',
  })
export const LogsResponse = z.object({ entries: z.array(LogEntry) }).meta({
  id: 'LogsResponse',
})
export const PeerAddArgs = z
  .object({
    /**
     * Profile interface name (e.g., "lan", "guest")
     */
    profile: z.string().meta({
      description: 'Profile interface name (e.g., "lan", "guest")',
    }),
    /**
     * Peer configuration
     */
    peer: VpnServerPeer.meta({
      description: 'Peer configuration',
    }),
  })
  .meta({
    id: 'PeerAddArgs',
  })
export const ProfileSetRequest = z
  .intersection(
    z.object({ force: wire(z.boolean().optional(), z.boolean()) }),
    z.intersection(
      z.object({
        gateway_ip: z.string(),
        outbound: z.string(),
        lan_access: z.union([
          z.literal('ALL'),
          z.literal('SAME_PROFILE'),
          z.object({ other_profiles: z.array(ProfileIdOpt) }),
        ]),
        wan_access: WanAccess,
        dns_override: wire(z.array(DnsServer).optional(), z.array(DnsServer)),
        dns_source: wire(z.string().optional(), z.string()),
        access_to_new_profiles: z.boolean(),
        owns_lan: z.boolean(),
      }),
      ProfileIdOpt,
    ),
  )
  .meta({
    id: 'ProfileSetRequest',
  })
export const PublishedPort = z
  .object({
    id: z.string(),
    enabled: z.boolean(),
    label: z.string(),
    device_mac: z.string(),
    ports: z.string(),
    protocol: Protocol,
    ipv4: z.boolean(),
    ipv6: z.boolean(),
    ipv4_public_port: wire(
      z.string().nullable().optional(),
      z.string().nullable(),
    ),
    source: z.string(),
    /**
     * Whether WAN collision confirmation remains active.
     */
    override_wan_ports: z.boolean().meta({
      description: 'Whether WAN collision confirmation remains active.',
    }),
    status: PublishedPortStatus,
    status_reason: wire(
      z.string().nullable().optional(),
      z.string().nullable(),
    ),
    device_name: wire(z.string().nullable().optional(), z.string().nullable()),
    device_ipv4: wire(z.string().nullable().optional(), z.string().nullable()),
    device_ipv6: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'PublishedPort',
  })
export const PublishedPortInput = z
  .object({
    id: z.string(),
    enabled: z.boolean(),
    label: z.string(),
    device_mac: z.string(),
    ports: z.string(),
    protocol: Protocol,
    ipv4: z.boolean(),
    ipv6: z.boolean(),
    ipv4_public_port: wire(
      z.string().nullable().optional(),
      z.string().nullable(),
    ),
    source: z.string(),
    /**
     * Confirms an enabled IPv4 WAN collision.
     */
    override_wan_ports: wire(z.boolean().optional(), z.boolean()).meta({
      description: 'Confirms an enabled IPv4 WAN collision.',
    }),
  })
  .meta({
    id: 'PublishedPortInput',
  })
export const ScheduleWindows = z
  .object({ interface: z.string(), windows: z.array(ScheduleWindow) })
  .meta({
    id: 'ScheduleWindows',
  })
export const SetArgs = z
  .object({
    /**
     * Profile interface name (e.g., "lan", "guest")
     */
    profile: z.string().meta({
      description: 'Profile interface name (e.g., "lan", "guest")',
    }),
    /**
     * VPN server configuration
     */
    config: VpnServerConfig.meta({
      description: 'VPN server configuration',
    }),
  })
  .meta({
    id: 'SetArgs',
  })
export const SetupStatusRes = z
  .object({ setupMode: z.boolean(), disk: DiskState })
  .meta({
    id: 'SetupStatusRes',
  })
export const UciFile = z
  .object({
    sections: z.array(Section),
    modified: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'UciFile',
  })

/**
 * VPN server configuration returned by list (excludes sensitive data)
 */
export const VpnServer = z
  .object({
    /**
     * Profile interface name this VPN server is associated with (e.g., "lan", "guest")
     */
    profile: z.string().meta({
      description:
        'Profile interface name this VPN server is associated with (e.g., "lan", "guest")',
    }),
    /**
     * Human-readable label for the VPN server
     */
    label: z.string().meta({
      description: 'Human-readable label for the VPN server',
    }),
    /**
     * Whether the VPN server is enabled (listening)
     */
    enabled: z.boolean().meta({
      description: 'Whether the VPN server is enabled (listening)',
    }),
    /**
     * Listen port for WireGuard connections
     */
    listen_port: z.int().meta({
      description: 'Listen port for WireGuard connections',
    }),
    /**
     * Public endpoint address for inbound client connections (hostname or IP)
     */
    endpoint: z.string().meta({
      description:
        'Public endpoint address for inbound client connections (hostname or IP)',
    }),
    /**
     * Server's public key
     */
    public_key: z.string().meta({
      description: "Server's public key",
    }),
    /**
     * Server's VPN address (derived from profile's gateway)
     */
    server_address: z.string().meta({
      description: "Server's VPN address (derived from profile's gateway)",
    }),
    /**
     * Connected peers
     */
    peers: z.array(VpnServerPeer).meta({
      description: 'Connected peers',
    }),
  })
  .meta({
    id: 'VpnServer',
    description:
      'VPN server configuration returned by list (excludes sensitive data)',
  })
export const WanDdnsResponse = z
  .object({
    enabled: z.boolean(),
    provider: DdnsProvider,
    hostname: wire(z.string().nullable().optional(), z.string().nullable()),
    username: wire(z.string().nullable().optional(), z.string().nullable()),
    password: wire(z.string().nullable().optional(), z.string().nullable()),
    token: wire(z.string().nullable().optional(), z.string().nullable()),
    zone: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'WanDdnsResponse',
  })
export const WanDdnsSetRequest = z
  .object({
    enabled: z.boolean(),
    provider: DdnsProvider,
    hostname: wire(z.string().nullable().optional(), z.string().nullable()),
    username: wire(z.string().nullable().optional(), z.string().nullable()),
    password: wire(z.string().nullable().optional(), z.string().nullable()),
    token: wire(z.string().nullable().optional(), z.string().nullable()),
    zone: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'WanDdnsSetRequest',
  })
export const WanDnsResponse = z
  .object({ mode: DnsMode, servers: z.array(DnsServer) })
  .meta({
    id: 'WanDnsResponse',
  })
export const WanDnsSetRequest = z
  .object({
    mode: DnsMode,
    servers: wire(
      z.array(DnsServer).nullable().optional(),
      z.array(DnsServer).nullable(),
    ),
  })
  .meta({
    id: 'WanDnsSetRequest',
  })
export const WanIpv4Response = z
  .object({
    mode: WanIpv4Mode,
    assigned_ip: wire(z.string().nullable().optional(), z.string().nullable()),
    address: wire(z.string().nullable().optional(), z.string().nullable()),
    netmask: wire(z.string().nullable().optional(), z.string().nullable()),
    gateway: wire(z.string().nullable().optional(), z.string().nullable()),
    username: wire(z.string().nullable().optional(), z.string().nullable()),
    password: wire(z.string().nullable().optional(), z.string().nullable()),
    device: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'WanIpv4Response',
  })
export const WanIpv4SetRequest = z
  .object({
    mode: WanIpv4Mode,
    address: wire(z.string().nullable().optional(), z.string().nullable()),
    netmask: wire(z.string().nullable().optional(), z.string().nullable()),
    gateway: wire(z.string().nullable().optional(), z.string().nullable()),
    username: wire(z.string().nullable().optional(), z.string().nullable()),
    password: wire(z.string().nullable().optional(), z.string().nullable()),
    device: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'WanIpv4SetRequest',
  })
export const WanIpv6Response = z
  .object({
    mode: WanIpv6Mode,
    address: wire(z.string().nullable().optional(), z.string().nullable()),
    prefix: wire(z.string().nullable().optional(), z.string().nullable()),
    gateway: wire(z.string().nullable().optional(), z.string().nullable()),
    ip6prefix: wire(z.string().nullable().optional(), z.string().nullable()),
    ip6prefixlen: wire(z.string().nullable().optional(), z.string().nullable()),
    ip4prefixlen: wire(z.string().nullable().optional(), z.string().nullable()),
    border_relay: wire(z.string().nullable().optional(), z.string().nullable()),
    assigned_ipv6: wire(
      z.string().nullable().optional(),
      z.string().nullable(),
    ),
    /**
     * Static mode: LAN prefix pool for sub-delegation, e.g. "2001:db8:abcd::/48"
     */
    lan_prefix: wire(
      z.string().nullable().optional(),
      z.string().nullable(),
    ).meta({
      description:
        'Static mode: LAN prefix pool for sub-delegation, e.g. "2001:db8:abcd::/48"',
    }),
  })
  .meta({
    id: 'WanIpv6Response',
  })
export const WanIpv6SetRequest = z
  .object({
    mode: WanIpv6Mode,
    address: wire(z.string().nullable().optional(), z.string().nullable()),
    prefix: wire(z.string().nullable().optional(), z.string().nullable()),
    gateway: wire(z.string().nullable().optional(), z.string().nullable()),
    ip6prefix: wire(z.string().nullable().optional(), z.string().nullable()),
    ip6prefixlen: wire(z.string().nullable().optional(), z.string().nullable()),
    ip4prefixlen: wire(z.string().nullable().optional(), z.string().nullable()),
    border_relay: wire(z.string().nullable().optional(), z.string().nullable()),
    /**
     * Static mode: LAN prefix pool for sub-delegation, e.g. "2001:db8:abcd::/48"
     */
    lan_prefix: wire(
      z.string().nullable().optional(),
      z.string().nullable(),
    ).meta({
      description:
        'Static mode: LAN prefix pool for sub-delegation, e.g. "2001:db8:abcd::/48"',
    }),
  })
  .meta({
    id: 'WanIpv6SetRequest',
  })
export const WanMacResponse = z
  .object({ strategy: MacStrategy, mac: z.string(), default_mac: z.string() })
  .meta({
    id: 'WanMacResponse',
  })
export const WanMacSetRequest = z
  .object({
    strategy: MacStrategy,
    mac: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'WanMacSetRequest',
  })

/**
 * An enabled IPv4 forward overlapping a router- or SNI-owned WAN port.
 */
export const WanPortCollision = z
  .object({
    id: z.string(),
    label: z.string(),
    router_service_ports: z.array(z.string()),
    hostname_route_ports: z.array(SniPortUse),
  })
  .meta({
    id: 'WanPortCollision',
    description:
      'An enabled IPv4 forward overlapping a router- or SNI-owned WAN port.',
  })

/**
 * `wifi.set` request: the desired WiFi config plus a confirmation flag.
 * When a profile loses its last WiFi password its devices can no longer reach
 * that subnet — disconnected if the password was removed, or moved to another
 * profile if it was reassigned — which breaks any published ports forwarding to
 * them. When the flag is false and such ports exist, `set` applies nothing and
 * returns them in `WifiSetResult` for a confirmation dialog; with the flag true
 * it deletes those published ports as part of the change.
 */
export const WifiSetRequest = z
  .intersection(
    z.object({
      confirmPublishedPortDeletion: wire(z.boolean().optional(), z.boolean()),
    }),
    z.object({
      ssid: z.string(),
      broadcastSeparately: z.boolean(),
      /**
       * Unset runs the world regulatory domain.
       */
      country: wire(
        z.string().nullable().optional(),
        z.string().nullable(),
      ).meta({
        description: 'Unset runs the world regulatory domain.',
      }),
      radios: z.record(z.string(), WifiRadio),
      passwords: z.array(
        z.object({
          label: z.string(),
          profile: wire(
            ProfileIdOpt.nullable().optional(),
            ProfileIdOpt.nullable(),
          ),
          password: z.string(),
        }),
      ),
    }),
  )
  .meta({
    id: 'WifiSetRequest',
    description:
      '`wifi.set` request: the desired WiFi config plus a confirmation flag.\nWhen a profile loses its last WiFi password its devices can no longer reach\nthat subnet — disconnected if the password was removed, or moved to another\nprofile if it was reassigned — which breaks any published ports forwarding to\nthem. When the flag is false and such ports exist, `set` applies nothing and\nreturns them in `WifiSetResult` for a confirmation dialog; with the flag true\nit deletes those published ports as part of the change.',
  })
export const WifiSetResult = z
  .object({
    /**
     * Non-empty (and nothing applied) when published ports would be deleted and
     * the caller hasn't confirmed yet. Empty once the change is applied.
     */
    pendingPublishedPortDeletions: z.array(AffectedPublishedPort).meta({
      description:
        "Non-empty (and nothing applied) when published ports would be deleted and\nthe caller hasn't confirmed yet. Empty once the change is applied.",
    }),
  })
  .meta({
    id: 'WifiSetResult',
  })
export const PublishedPortsSetRequest = z
  .object({ ports: z.array(PublishedPortInput) })
  .meta({
    id: 'PublishedPortsSetRequest',
  })

/**
 * [`set`] response. A non-empty collision list means nothing was applied —
 * the caller confirms and re-saves; empty means the request was applied.
 */
export const PublishedPortsSetResult = z
  .object({ pending_wan_port_collisions: z.array(WanPortCollision) })
  .meta({
    id: 'PublishedPortsSetResult',
    description:
      '[`set`] response. A non-empty collision list means nothing was applied —\nthe caller confirms and re-saves; empty means the request was applied.',
  })

/**
 * Response containing all VPN servers
 */
export const VpnServers = z.object({ servers: z.array(VpnServer) }).meta({
  id: 'VpnServers',
  description: 'Response containing all VPN servers',
})
export const WifiRegulatory = z
  .object({
    /**
     * ISO 3166-1 alpha-2 codes the regulatory database defines.
     */
    countries: z.array(z.string()).meta({
      description: 'ISO 3166-1 alpha-2 codes the regulatory database defines.',
    }),
    /**
     * What an access point may use under the current country, by band.
     */
    bands: z.record(z.string(), BandChannels).meta({
      description:
        'What an access point may use under the current country, by band.',
    }),
  })
  .meta({
    id: 'WifiRegulatory',
  })
export const Api = {
  _PARAMS: z.object({}) as z.ZodType<{}, {}>,
  _CHILDREN: {
    activity: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        clear: {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _RETURN: z.null(),
        },
        delete: { _PARAMS: ActivityDeleteParams, _RETURN: z.null() },
        list: {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            ActivityListParams,
          ),
          _RETURN: ActivityListResponse,
        },
      },
    },
    auth: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        'check-initialized': {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _RETURN: CheckInitializedRes,
        },
        login: { _PARAMS: LoginParams, _RETURN: LoginRes },
        logout: { _PARAMS: LogoutParams, _RETURN: z.null() },
        'set-initial-password': {
          _PARAMS: SetInitialPasswordParams,
          _RETURN: LoginRes,
        },
        'set-password': { _PARAMS: ResetPasswordParams, _RETURN: z.null() },
        'verify-password': { _PARAMS: VerifyPasswordParams, _RETURN: z.null() },
      },
    },
    backup: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        create: {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _RETURN: BackupCreateRes,
        },
        restore: {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _RETURN: BackupRestoreRes,
        },
      },
    },
    devices: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        'data-usage': {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            DataUsageReq.meta({
              description:
                'A clap-compatible wrapper that reads the inner value as JSON from stdin\ninstead of from CLI args. Used for commands that take complex structured\ninput (entire profile objects, etc.).\n\nThis is a thin wrapper around `startos::util::serde::StdinDeserializable`\nthat adds a `CommandFactory` impl so it can be used directly as a handler\n`Params` type (rather than flattened inside a `#[derive(Parser)]` struct,\nwhich is how startos uses its version).',
            }),
          ),
          _RETURN: z.array(DataUsagePoint),
        },
        forget: {
          _PARAMS: DeviceMacReq.meta({
            description:
              'A clap-compatible wrapper that reads the inner value as JSON from stdin\ninstead of from CLI args. Used for commands that take complex structured\ninput (entire profile objects, etc.).\n\nThis is a thin wrapper around `startos::util::serde::StdinDeserializable`\nthat adds a `CommandFactory` impl so it can be used directly as a handler\n`Params` type (rather than flattened inside a `#[derive(Parser)]` struct,\nwhich is how startos uses its version).',
          }),
          _RETURN: z.null(),
        },
        list: {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            z.object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: z.array(Device),
        },
        'set-auto-forward': {
          _PARAMS: SetAutoForwardRequest.meta({
            description:
              'A clap-compatible wrapper that reads the inner value as JSON from stdin\ninstead of from CLI args. Used for commands that take complex structured\ninput (entire profile objects, etc.).\n\nThis is a thin wrapper around `startos::util::serde::StdinDeserializable`\nthat adds a `CommandFactory` impl so it can be used directly as a handler\n`Params` type (rather than flattened inside a `#[derive(Parser)]` struct,\nwhich is how startos uses its version).',
          }),
          _RETURN: z.null(),
        },
        'set-dns-injection': {
          _PARAMS: SetDnsInjectionReq.meta({
            description:
              'A clap-compatible wrapper that reads the inner value as JSON from stdin\ninstead of from CLI args. Used for commands that take complex structured\ninput (entire profile objects, etc.).\n\nThis is a thin wrapper around `startos::util::serde::StdinDeserializable`\nthat adds a `CommandFactory` impl so it can be used directly as a handler\n`Params` type (rather than flattened inside a `#[derive(Parser)]` struct,\nwhich is how startos uses its version).',
          }),
          _RETURN: z.null(),
        },
        update: {
          _PARAMS: DeviceUpdateReq.meta({
            description:
              'A clap-compatible wrapper that reads the inner value as JSON from stdin\ninstead of from CLI args. Used for commands that take complex structured\ninput (entire profile objects, etc.).\n\nThis is a thin wrapper around `startos::util::serde::StdinDeserializable`\nthat adds a `CommandFactory` impl so it can be used directly as a handler\n`Params` type (rather than flattened inside a `#[derive(Parser)]` struct,\nwhich is how startos uses its version).',
          }),
          _RETURN: z.null(),
        },
      },
    },
    diagnostics: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        create: {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _RETURN: DiagnosticsCreateRes,
        },
      },
    },
    dir: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        get: {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            DirGetArgs,
          ),
          _RETURN: z.array(DirEntry),
        },
      },
    },
    dns: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        'injected-list': {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            z.object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: z.array(InjectedDnsRecord),
        },
      },
    },
    ethernet: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        edit: {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            z.object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: z.null(),
        },
        get: {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            z.object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: z.object({
            wan_ipv6: z.boolean(),
            wan_port: wire(
              z.string().nullable().optional(),
              z.string().nullable(),
            ),
            ports: z.record(
              z.string(),
              z.object({
                profile: wire(
                  ProfileId.nullable().optional(),
                  ProfileId.nullable(),
                ),
              }),
            ),
          }),
        },
        set: {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            EthernetSetRequest.meta({
              description:
                'A clap-compatible wrapper that reads the inner value as JSON from stdin\ninstead of from CLI args. Used for commands that take complex structured\ninput (entire profile objects, etc.).\n\nThis is a thin wrapper around `startos::util::serde::StdinDeserializable`\nthat adds a `CommandFactory` impl so it can be used directly as a handler\n`Params` type (rather than flattened inside a `#[derive(Parser)]` struct,\nwhich is how startos uses its version).',
            }),
          ),
          _RETURN: EthernetSetResult,
        },
      },
    },
    exec: {
      _PARAMS: z.intersection(
        z.intersection(
          z.object({
            format: wire(IoFormat.nullable().optional(), IoFormat.nullable()),
          }),
          z.object({}) as z.ZodType<{}, {}>,
        ),
        ExecReq,
      ),
      _RETURN: ExecRes,
    },
    file: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        get: {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            GetFileArgs,
          ),
          _RETURN: FileContents,
        },
        set: {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            SetFileArgs,
          ),
          _RETURN: z.null(),
        },
      },
    },
    lan: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        'ipv4-get': {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            z.object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: LanIpv4Response,
        },
        'ipv4-set': {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            LanIpv4SetRequest.meta({
              description:
                'A clap-compatible wrapper that reads the inner value as JSON from stdin\ninstead of from CLI args. Used for commands that take complex structured\ninput (entire profile objects, etc.).\n\nThis is a thin wrapper around `startos::util::serde::StdinDeserializable`\nthat adds a `CommandFactory` impl so it can be used directly as a handler\n`Params` type (rather than flattened inside a `#[derive(Parser)]` struct,\nwhich is how startos uses its version).',
            }),
          ),
          _RETURN: z.null(),
        },
        'ipv6-get': {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            z.object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: LanIpv6Response,
        },
        'ipv6-set': {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            LanIpv6SetRequest.meta({
              description:
                'A clap-compatible wrapper that reads the inner value as JSON from stdin\ninstead of from CLI args. Used for commands that take complex structured\ninput (entire profile objects, etc.).\n\nThis is a thin wrapper around `startos::util::serde::StdinDeserializable`\nthat adds a `CommandFactory` impl so it can be used directly as a handler\n`Params` type (rather than flattened inside a `#[derive(Parser)]` struct,\nwhich is how startos uses its version).',
            }),
          ),
          _RETURN: z.null(),
        },
      },
    },
    profiles: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        create: {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            z
              .intersection(
                z.object({
                  gateway_ip: z.string(),
                  outbound: z.string(),
                  lan_access: z.union([
                    z.literal('ALL'),
                    z.literal('SAME_PROFILE'),
                    z.object({ other_profiles: z.array(ProfileIdOpt) }),
                  ]),
                  wan_access: WanAccess,
                  dns_override: wire(
                    z.array(DnsServer).optional(),
                    z.array(DnsServer),
                  ),
                  dns_source: wire(z.string().optional(), z.string()),
                  access_to_new_profiles: z.boolean(),
                  owns_lan: z.boolean(),
                }),
                ProfileIdOpt,
              )
              .meta({
                description:
                  'A clap-compatible wrapper that reads the inner value as JSON from stdin\ninstead of from CLI args. Used for commands that take complex structured\ninput (entire profile objects, etc.).\n\nThis is a thin wrapper around `startos::util::serde::StdinDeserializable`\nthat adds a `CommandFactory` impl so it can be used directly as a handler\n`Params` type (rather than flattened inside a `#[derive(Parser)]` struct,\nwhich is how startos uses its version).',
              }),
          ),
          _RETURN: ProfileId,
        },
        delete: { _PARAMS: ProfileIdOpt, _RETURN: z.null() },
        edit: {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            EditArgs,
          ),
          _RETURN: ProfileId,
        },
        get: {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            ProfileIdOpt,
          ),
          _RETURN: z.intersection(
            z.object({
              gateway_ip: z.string(),
              outbound: z.string(),
              lan_access: z.union([
                z.literal('ALL'),
                z.literal('SAME_PROFILE'),
                z.object({ other_profiles: z.array(ProfileId) }),
              ]),
              wan_access: WanAccess,
              dns_override: wire(
                z.array(DnsServer).optional(),
                z.array(DnsServer),
              ),
              dns_source: wire(z.string().optional(), z.string()),
              access_to_new_profiles: z.boolean(),
              owns_lan: z.boolean(),
            }),
            ProfileId,
          ),
        },
        list: {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            z.object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: z.array(ProfileId),
        },
        'schedule-get': {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            ScheduleGetParams.meta({
              description:
                'A clap-compatible wrapper that reads the inner value as JSON from stdin\ninstead of from CLI args. Used for commands that take complex structured\ninput (entire profile objects, etc.).\n\nThis is a thin wrapper around `startos::util::serde::StdinDeserializable`\nthat adds a `CommandFactory` impl so it can be used directly as a handler\n`Params` type (rather than flattened inside a `#[derive(Parser)]` struct,\nwhich is how startos uses its version).',
            }),
          ),
          _RETURN: z.array(ScheduleWindow),
        },
        'schedule-set': {
          _PARAMS: ScheduleWindows.meta({
            description:
              'A clap-compatible wrapper that reads the inner value as JSON from stdin\ninstead of from CLI args. Used for commands that take complex structured\ninput (entire profile objects, etc.).\n\nThis is a thin wrapper around `startos::util::serde::StdinDeserializable`\nthat adds a `CommandFactory` impl so it can be used directly as a handler\n`Params` type (rather than flattened inside a `#[derive(Parser)]` struct,\nwhich is how startos uses its version).',
          }),
          _RETURN: z.null(),
        },
        set: {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            ProfileSetRequest.meta({
              description:
                'A clap-compatible wrapper that reads the inner value as JSON from stdin\ninstead of from CLI args. Used for commands that take complex structured\ninput (entire profile objects, etc.).\n\nThis is a thin wrapper around `startos::util::serde::StdinDeserializable`\nthat adds a `CommandFactory` impl so it can be used directly as a handler\n`Params` type (rather than flattened inside a `#[derive(Parser)]` struct,\nwhich is how startos uses its version).',
            }),
          ),
          _RETURN: ProfileId,
        },
      },
    },
    'published-ports': {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        'auto-list': {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            z.object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: z.array(AutomaticPortUse),
        },
        list: {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            z.object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: z.array(PublishedPort),
        },
        reconcile: {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _RETURN: z.unknown(),
        },
        set: {
          _PARAMS: PublishedPortsSetRequest.meta({
            description:
              'A clap-compatible wrapper that reads the inner value as JSON from stdin\ninstead of from CLI args. Used for commands that take complex structured\ninput (entire profile objects, etc.).\n\nThis is a thin wrapper around `startos::util::serde::StdinDeserializable`\nthat adds a `CommandFactory` impl so it can be used directly as a handler\n`Params` type (rather than flattened inside a `#[derive(Parser)]` struct,\nwhich is how startos uses its version).',
          }),
          _RETURN: PublishedPortsSetResult,
        },
        'sync-hairpin': {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _RETURN: z.unknown(),
        },
        'wan-changed': {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _RETURN: z.unknown(),
        },
      },
    },
    setup: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        status: {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _RETURN: SetupStatusRes,
        },
      },
    },
    'ssh-keys': {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        add: {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            SshKeyAddParams,
          ),
          _RETURN: SshKeyResponse,
        },
        delete: { _PARAMS: SshKeyDeleteParams, _RETURN: z.null() },
        list: {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            z.object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: z.array(SshKeyResponse),
        },
      },
    },
    system: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        'apply-remote-access': {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _RETURN: z.unknown(),
        },
        'factory-reset': {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _RETURN: z.null(),
        },
        'get-timezones': {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            z.object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: z.array(z.string()),
        },
        info: {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            z.object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: SystemInfoResponse,
        },
        logs: {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            z.object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: LogsResponse,
        },
        'newer-versions': {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            z.object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: z.array(VersionInfo),
        },
        restart: {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _RETURN: z.null(),
        },
        'set-preferences': {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            SetPreferencesReq.meta({
              description:
                'A clap-compatible wrapper that reads the inner value as JSON from stdin\ninstead of from CLI args. Used for commands that take complex structured\ninput (entire profile objects, etc.).\n\nThis is a thin wrapper around `startos::util::serde::StdinDeserializable`\nthat adds a `CommandFactory` impl so it can be used directly as a handler\n`Params` type (rather than flattened inside a `#[derive(Parser)]` struct,\nwhich is how startos uses its version).',
            }),
          ),
          _RETURN: z.unknown(),
        },
        'set-timezone': {
          _PARAMS: SetTimezoneParams.meta({
            description:
              'A clap-compatible wrapper that reads the inner value as JSON from stdin\ninstead of from CLI args. Used for commands that take complex structured\ninput (entire profile objects, etc.).\n\nThis is a thin wrapper around `startos::util::serde::StdinDeserializable`\nthat adds a `CommandFactory` impl so it can be used directly as a handler\n`Params` type (rather than flattened inside a `#[derive(Parser)]` struct,\nwhich is how startos uses its version).',
          }),
          _RETURN: z.null(),
        },
        update: {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            UpdateSystemParams,
          ),
          _RETURN: UpdateSystemRes,
        },
      },
    },
    uci: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        edit: {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            GetArgs,
          ),
          _RETURN: z.record(z.string(), z.string()),
        },
        get: {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            GetArgs,
          ),
          _RETURN: z.record(z.string(), UciFile),
        },
        set: {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            z.record(z.string(), UciFile).meta({
              description:
                'A clap-compatible wrapper that reads the inner value as JSON from stdin\ninstead of from CLI args. Used for commands that take complex structured\ninput (entire profile objects, etc.).\n\nThis is a thin wrapper around `startos::util::serde::StdinDeserializable`\nthat adds a `CommandFactory` impl so it can be used directly as a handler\n`Params` type (rather than flattened inside a `#[derive(Parser)]` struct,\nwhich is how startos uses its version).',
            }),
          ),
          _RETURN: z.record(z.string(), z.string()),
        },
      },
    },
    'vpn-client': {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        create: {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            OutboundVpnCreateRequest.meta({
              description:
                'A clap-compatible wrapper that reads the inner value as JSON from stdin\ninstead of from CLI args. Used for commands that take complex structured\ninput (entire profile objects, etc.).\n\nThis is a thin wrapper around `startos::util::serde::StdinDeserializable`\nthat adds a `CommandFactory` impl so it can be used directly as a handler\n`Params` type (rather than flattened inside a `#[derive(Parser)]` struct,\nwhich is how startos uses its version).',
            }),
          ),
          _RETURN: OutboundVpnCreateResponse,
        },
        delete: {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            OutboundVpnDeleteRequest,
          ),
          _RETURN: z.null(),
        },
        list: {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            z.object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: z.array(OutboundVpn),
        },
        'set-enabled': {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            OutboundVpnSetEnabledRequest.meta({
              description:
                'A clap-compatible wrapper that reads the inner value as JSON from stdin\ninstead of from CLI args. Used for commands that take complex structured\ninput (entire profile objects, etc.).\n\nThis is a thin wrapper around `startos::util::serde::StdinDeserializable`\nthat adds a `CommandFactory` impl so it can be used directly as a handler\n`Params` type (rather than flattened inside a `#[derive(Parser)]` struct,\nwhich is how startos uses its version).',
            }),
          ),
          _RETURN: z.null(),
        },
        update: {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            OutboundVpnUpdateRequest.meta({
              description:
                'A clap-compatible wrapper that reads the inner value as JSON from stdin\ninstead of from CLI args. Used for commands that take complex structured\ninput (entire profile objects, etc.).\n\nThis is a thin wrapper around `startos::util::serde::StdinDeserializable`\nthat adds a `CommandFactory` impl so it can be used directly as a handler\n`Params` type (rather than flattened inside a `#[derive(Parser)]` struct,\nwhich is how startos uses its version).',
            }),
          ),
          _RETURN: z.null(),
        },
      },
    },
    'vpn-server': {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        delete: {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            DeleteArgs,
          ),
          _RETURN: z.null(),
        },
        list: {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            z.object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: VpnServers,
        },
        'peer-add': {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            PeerAddArgs.meta({
              description:
                'A clap-compatible wrapper that reads the inner value as JSON from stdin\ninstead of from CLI args. Used for commands that take complex structured\ninput (entire profile objects, etc.).\n\nThis is a thin wrapper around `startos::util::serde::StdinDeserializable`\nthat adds a `CommandFactory` impl so it can be used directly as a handler\n`Params` type (rather than flattened inside a `#[derive(Parser)]` struct,\nwhich is how startos uses its version).',
            }),
          ),
          _RETURN: PeerAddResponse,
        },
        'peer-delete': {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            PeerDeleteArgs,
          ),
          _RETURN: z.null(),
        },
        set: {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            SetArgs.meta({
              description:
                'A clap-compatible wrapper that reads the inner value as JSON from stdin\ninstead of from CLI args. Used for commands that take complex structured\ninput (entire profile objects, etc.).\n\nThis is a thin wrapper around `startos::util::serde::StdinDeserializable`\nthat adds a `CommandFactory` impl so it can be used directly as a handler\n`Params` type (rather than flattened inside a `#[derive(Parser)]` struct,\nwhich is how startos uses its version).',
            }),
          ),
          _RETURN: z.null(),
        },
      },
    },
    wan: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        'ddns-get': {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            z.object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: WanDdnsResponse,
        },
        'ddns-set': {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            WanDdnsSetRequest.meta({
              description:
                'A clap-compatible wrapper that reads the inner value as JSON from stdin\ninstead of from CLI args. Used for commands that take complex structured\ninput (entire profile objects, etc.).\n\nThis is a thin wrapper around `startos::util::serde::StdinDeserializable`\nthat adds a `CommandFactory` impl so it can be used directly as a handler\n`Params` type (rather than flattened inside a `#[derive(Parser)]` struct,\nwhich is how startos uses its version).',
            }),
          ),
          _RETURN: z.null(),
        },
        'dns-get': {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            z.object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: WanDnsResponse,
        },
        'dns-set': {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            WanDnsSetRequest.meta({
              description:
                'A clap-compatible wrapper that reads the inner value as JSON from stdin\ninstead of from CLI args. Used for commands that take complex structured\ninput (entire profile objects, etc.).\n\nThis is a thin wrapper around `startos::util::serde::StdinDeserializable`\nthat adds a `CommandFactory` impl so it can be used directly as a handler\n`Params` type (rather than flattened inside a `#[derive(Parser)]` struct,\nwhich is how startos uses its version).',
            }),
          ),
          _RETURN: z.null(),
        },
        'ipv4-get': {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            z.object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: WanIpv4Response,
        },
        'ipv4-set': {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            WanIpv4SetRequest.meta({
              description:
                'A clap-compatible wrapper that reads the inner value as JSON from stdin\ninstead of from CLI args. Used for commands that take complex structured\ninput (entire profile objects, etc.).\n\nThis is a thin wrapper around `startos::util::serde::StdinDeserializable`\nthat adds a `CommandFactory` impl so it can be used directly as a handler\n`Params` type (rather than flattened inside a `#[derive(Parser)]` struct,\nwhich is how startos uses its version).',
            }),
          ),
          _RETURN: z.null(),
        },
        'ipv6-get': {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            z.object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: WanIpv6Response,
        },
        'ipv6-set': {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            WanIpv6SetRequest.meta({
              description:
                'A clap-compatible wrapper that reads the inner value as JSON from stdin\ninstead of from CLI args. Used for commands that take complex structured\ninput (entire profile objects, etc.).\n\nThis is a thin wrapper around `startos::util::serde::StdinDeserializable`\nthat adds a `CommandFactory` impl so it can be used directly as a handler\n`Params` type (rather than flattened inside a `#[derive(Parser)]` struct,\nwhich is how startos uses its version).',
            }),
          ),
          _RETURN: z.null(),
        },
        'mac-get': {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            z.object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: WanMacResponse,
        },
        'mac-set': {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            WanMacSetRequest.meta({
              description:
                'A clap-compatible wrapper that reads the inner value as JSON from stdin\ninstead of from CLI args. Used for commands that take complex structured\ninput (entire profile objects, etc.).\n\nThis is a thin wrapper around `startos::util::serde::StdinDeserializable`\nthat adds a `CommandFactory` impl so it can be used directly as a handler\n`Params` type (rather than flattened inside a `#[derive(Parser)]` struct,\nwhich is how startos uses its version).',
            }),
          ),
          _RETURN: z.null(),
        },
      },
    },
    wifi: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        'blackout-get': {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            z.object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: z.array(BlackoutWindow),
        },
        'blackout-set': {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            BlackoutWindows.meta({
              description:
                'A clap-compatible wrapper that reads the inner value as JSON from stdin\ninstead of from CLI args. Used for commands that take complex structured\ninput (entire profile objects, etc.).\n\nThis is a thin wrapper around `startos::util::serde::StdinDeserializable`\nthat adds a `CommandFactory` impl so it can be used directly as a handler\n`Params` type (rather than flattened inside a `#[derive(Parser)]` struct,\nwhich is how startos uses its version).',
            }),
          ),
          _RETURN: z.null(),
        },
        edit: {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            z.object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: z.null(),
        },
        'generate-password': {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            z.object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: z.string(),
        },
        get: {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            z.object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: z.object({
            ssid: z.string(),
            broadcastSeparately: z.boolean(),
            /**
             * Unset runs the world regulatory domain.
             */
            country: wire(
              z.string().nullable().optional(),
              z.string().nullable(),
            ).meta({
              description: 'Unset runs the world regulatory domain.',
            }),
            radios: z.record(z.string(), WifiRadio),
            passwords: z.array(
              z.object({
                label: z.string(),
                profile: wire(
                  ProfileId.nullable().optional(),
                  ProfileId.nullable(),
                ),
                password: z.string(),
              }),
            ),
          }),
        },
        regulatory: {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            z.object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: WifiRegulatory,
        },
        set: {
          _PARAMS: z.intersection(
            z.intersection(
              z.object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              z.object({}) as z.ZodType<{}, {}>,
            ),
            WifiSetRequest.meta({
              description:
                'A clap-compatible wrapper that reads the inner value as JSON from stdin\ninstead of from CLI args. Used for commands that take complex structured\ninput (entire profile objects, etc.).\n\nThis is a thin wrapper around `startos::util::serde::StdinDeserializable`\nthat adds a `CommandFactory` impl so it can be used directly as a handler\n`Params` type (rather than flattened inside a `#[derive(Parser)]` struct,\nwhich is how startos uses its version).',
            }),
          ),
          _RETURN: WifiSetResult,
        },
      },
    },
  },
}
