import { z } from 'zod'
const wire = <A extends z.ZodType, B extends z.ZodType>(input: A, output: B) =>
  z.codec(input, output, { decode: v => v as never, encode: v => v as never })
export const AddDnsRecordParams = z
  .object({
    name: z.string(),
    type: z.string(),
    value: z.string(),
    ttl: wire(z.int().nullable().optional(), z.int().nullable()),
  })
  .meta({
    id: 'AddDnsRecordParams',
  })
export const AddPinholeParams = z
  .object({
    /**
     * The client's global IPv6 (GUA) to expose. Must be an address this tunnel
     * delegates to a client — its subnet needs an IPv6 prefix.
     */
    gua: z.string().meta({
      description:
        "The client's global IPv6 (GUA) to expose. Must be an address this tunnel\ndelegates to a client — its subnet needs an IPv6 prefix.",
    }),
    /**
     * External port opened on the GUA.
     */
    externalPort: z.int().meta({
      description: 'External port opened on the GUA.',
    }),
    /**
     * Destination port on the GUA. Omit for a pure pinhole (no NAT, internal ==
     * external); set a different value for a port remap (e.g. 80 -> 443).
     */
    internalPort: wire(z.int().nullable().optional(), z.int().nullable()).meta({
      description:
        'Destination port on the GUA. Omit for a pure pinhole (no NAT, internal ==\nexternal); set a different value for a port remap (e.g. 80 -> 443).',
    }),
    label: wire(z.string().nullable().optional(), z.string().nullable()),
    /**
     * Number of contiguous ports, counting up from external/internal. Default 1.
     */
    count: wire(z.int().nullable().optional(), z.int().nullable()).meta({
      description:
        'Number of contiguous ports, counting up from external/internal. Default 1.',
    }),
  })
  .meta({
    id: 'AddPinholeParams',
  })
export const AddPortForwardParams = z
  .object({
    /**
     * External (WAN) port to forward. The external IP is fixed to the target's
     * WAN so return traffic stays symmetric.
     */
    externalPort: z.int().meta({
      description:
        "External (WAN) port to forward. The external IP is fixed to the target's\nWAN so return traffic stays symmetric.",
    }),
    target: z.string(),
    label: wire(z.string().nullable().optional(), z.string().nullable()),
    /**
     * Hostnames to SNI-demux on the shared external port. Empty = normal DNAT.
     */
    sni: wire(z.array(z.string()).optional(), z.array(z.string())).meta({
      description:
        'Hostnames to SNI-demux on the shared external port. Empty = normal DNAT.',
    }),
    /**
     * Number of contiguous ports to forward (a PCP PORT_SET range), counting up
     * from both `external_port` and the target port. Defaults to 1. Not valid
     * together with SNI demux.
     */
    count: wire(z.int().nullable().optional(), z.int().nullable()).meta({
      description:
        'Number of contiguous ports to forward (a PCP PORT_SET range), counting up\nfrom both `external_port` and the target port. Defaults to 1. Not valid\ntogether with SNI demux.',
    }),
  })
  .meta({
    id: 'AddPortForwardParams',
  })
export const AddSubnetParams = z.object({ name: z.string() }).meta({
  id: 'AddSubnetParams',
})
export const AnyVerifyingKey = z.string().meta({
  id: 'AnyVerifyingKey',
})
export const ApplyParams = z.object({ expr: z.string() }).meta({
  id: 'ApplyParams',
})

/**
 * Per-subnet DNS proxy configuration. WireGuard clients on a subnet are pointed
 * at the subnet's in-tunnel server address (`.1`) for DNS; the forward-only proxy
 * listening there resolves every query against the upstream(s) selected here.
 */
export const DnsConfig = z
  .union([
    z.object({
      /**
       * Forward to the VPS's own system resolvers.
       */
      type: z.literal('default').meta({
        description: "Forward to the VPS's own system resolvers.",
      }),
    }),
    z.intersection(
      z.object({
        /**
         * Forward to a device on this subnet (its WireGuard IP) on port 53.
         */
        type: z.literal('device').meta({
          description:
            'Forward to a device on this subnet (its WireGuard IP) on port 53.',
        }),
      }),
      z.object({ ip: z.string() }),
    ),
    z.intersection(
      z.object({
        /**
         * Forward to operator-specified upstream servers (1-3, optional `:port`).
         */
        type: z.literal('custom').meta({
          description:
            'Forward to operator-specified upstream servers (1-3, optional `:port`).',
        }),
      }),
      z.object({ servers: z.array(z.string()) }),
    ),
  ])
  .meta({
    id: 'DnsConfig',
    description:
      "Per-subnet DNS proxy configuration. WireGuard clients on a subnet are pointed\nat the subnet's in-tunnel server address (`.1`) for DNS; the forward-only proxy\nlistening there resolves every query against the upstream(s) selected here.",
  })

/**
 * Which upstream a subnet's DNS proxy forwards to. `Device`/`Custom` draw
 * their data from companion fields on [`SetSubnetDnsParams`].
 */
export const DnsMode = z.enum(['default', 'device', 'custom']).meta({
  id: 'DnsMode',
  description:
    "Which upstream a subnet's DNS proxy forwards to. `Device`/`Custom` draw\ntheir data from companion fields on [`SetSubnetDnsParams`].",
})

/**
 * A DNS record served by the tunnel (injected via RFC 2136 or added manually).
 * `value` is the rdata as text: an IP for A/AAAA, a name for CNAME, etc.
 */
export const DnsRecordEntry = z
  .object({
    name: z.string(),
    type: z.string(),
    value: z.string(),
    ttl: z.int(),
    /**
     * The device IP that injected this, or `null` for a manual record.
     */
    source: wire(z.string().nullable().optional(), z.string().nullable()).meta({
      description:
        'The device IP that injected this, or `null` for a manual record.',
    }),
  })
  .meta({
    id: 'DnsRecordEntry',
    description:
      'A DNS record served by the tunnel (injected via RFC 2136 or added manually).\n`value` is the rdata as text: an IP for A/AAAA, a name for CNAME, etc.',
  })
export const Dump = z.object({ id: z.int(), value: z.unknown() }).meta({
  id: 'Dump',
})
export const DumpParams = z
  .object({
    pointer: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'DumpParams',
  })
export const GenerateCertParams = z
  .object({ subject: z.array(z.string()) })
  .meta({
    id: 'GenerateCertParams',
  })
export const Guid = z.string().meta({
  id: 'Guid',
})

/**
 * Proof that a set of auth keys was unenrolled — removed from the persisted
 * and ephemeral signer stores with any continuations they opened killed.
 * Obtained via [`SignatureAuthContext::unenroll`], or [`Self::unenroll`]
 * from inside a db transaction.
 */
export const HasUnenrolledKeys = z.null().meta({
  id: 'HasUnenrolledKeys',
  description:
    'Proof that a set of auth keys was unenrolled — removed from the persisted\nand ephemeral signer stores with any continuations they opened killed.\nObtained via [`SignatureAuthContext::unenroll`], or [`Self::unenroll`]\nfrom inside a db transaction.',
})
export const HttpRedirectStatus = z
  .object({
    ip: z.string(),
    /**
     * Whether the redirect is on for this IP (default true).
     */
    enabled: z.boolean().meta({
      description: 'Whether the redirect is on for this IP (default true).',
    }),
    /**
     * Whether a port-forward already occupies port 80 on this IP, in which case
     * the redirect yields and does not bind.
     */
    forwarded: z.boolean().meta({
      description:
        'Whether a port-forward already occupies port 80 on this IP, in which case\nthe redirect yields and does not bind.',
    }),
  })
  .meta({
    id: 'HttpRedirectStatus',
  })
export const IoFormat = z
  .enum(['json', 'json-pretty', 'yaml', 'cbor', 'toml', 'toml-pretty'])
  .meta({
    id: 'IoFormat',
  })
export const KillParams = z.object({ ids: z.array(z.string()) }).meta({
  id: 'KillParams',
})
export const ListDevicesParams = z.object({ subnet: z.string() }).meta({
  id: 'ListDevicesParams',
})
export const ListParams = (z.object({}) as z.ZodType<{}, {}>).meta({
  id: 'ListParams',
})
export const LogoutParams = (z.object({}) as z.ZodType<{}, {}>).meta({
  id: 'LogoutParams',
})
export const RemoveDeviceParams = z
  .object({ subnet: z.string(), ip: z.string() })
  .meta({
    id: 'RemoveDeviceParams',
  })
export const RemoveDnsRecordParams = z
  .object({
    name: z.string(),
    type: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'RemoveDnsRecordParams',
  })
export const RemovePinholeParams = z
  .object({ gua: z.string(), externalPort: z.int() })
  .meta({
    id: 'RemovePinholeParams',
  })
export const RemovePortForwardParams = z
  .object({
    source: z.string(),
    /**
     * Remove a single SNI route on `source`; omit to remove the whole forward.
     */
    hostname: wire(
      z.string().nullable().optional(),
      z.string().nullable(),
    ).meta({
      description:
        'Remove a single SNI route on `source`; omit to remove the whole forward.',
    }),
  })
  .meta({
    id: 'RemovePortForwardParams',
  })
export const Session = z
  .object({
    /**
     * A friendly name for the key, if one was assigned at enrollment (e.g.
     * tunnel device keys). UI-enrolled keys are unnamed.
     */
    name: wire(z.string().nullable().optional(), z.string().nullable()).meta({
      description:
        'A friendly name for the key, if one was assigned at enrollment (e.g.\ntunnel device keys). UI-enrolled keys are unnamed.',
    }),
    loggedIn: wire(z.string().optional(), z.string()),
    lastActive: wire(z.string().optional(), z.string()),
    userAgent: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'Session',
  })
export const SetAutoPortForwardParams = z
  .object({ subnet: z.string(), ip: z.string(), enabled: z.boolean() })
  .meta({
    id: 'SetAutoPortForwardParams',
  })
export const SetDeviceWanParams = z
  .object({
    subnet: z.string(),
    ip: z.string(),
    wanIp: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'SetDeviceWanParams',
  })
export const SetDnsInjectionParams = z
  .object({ subnet: z.string(), ip: z.string(), enabled: z.boolean() })
  .meta({
    id: 'SetDnsInjectionParams',
  })
export const SetHttpRedirectEnabledParams = z
  .object({
    /**
     * The public IPv4 whose default-on port-80 HTTP→HTTPS redirect to toggle.
     */
    ip: z.string().meta({
      description:
        'The public IPv4 whose default-on port-80 HTTP→HTTPS redirect to toggle.',
    }),
    enabled: z.boolean(),
  })
  .meta({
    id: 'SetHttpRedirectEnabledParams',
  })
export const SetListenParams = z.object({ listen: z.string() }).meta({
  id: 'SetListenParams',
})
export const SetPasswordParams = z.object({ password: z.string() }).meta({
  id: 'SetPasswordParams',
})
export const SetPinholeEnabledParams = z
  .object({ gua: z.string(), externalPort: z.int(), enabled: z.boolean() })
  .meta({
    id: 'SetPinholeEnabledParams',
  })
export const SetPortForwardEnabledParams = z
  .object({
    source: z.string(),
    enabled: z.boolean(),
    /**
     * Toggle a single SNI route on `source`; omit for a DNAT forward.
     */
    hostname: wire(
      z.string().nullable().optional(),
      z.string().nullable(),
    ).meta({
      description:
        'Toggle a single SNI route on `source`; omit for a DNAT forward.',
    }),
  })
  .meta({
    id: 'SetPortForwardEnabledParams',
  })
export const SetSubnetIpv6Params = z
  .object({
    /**
     * The routed IPv6 prefix delegated to this subnet (e.g. a /64 from Hetzner,
     * a /56 from Linode). `null` disables IPv6 on the subnet.
     */
    prefix: wire(z.string().nullable().optional(), z.string().nullable()).meta({
      description:
        'The routed IPv6 prefix delegated to this subnet (e.g. a /64 from Hetzner,\na /56 from Linode). `null` disables IPv6 on the subnet.',
    }),
  })
  .meta({
    id: 'SetSubnetIpv6Params',
  })
export const SetSubnetWanParams = z
  .object({
    wanIp: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'SetSubnetWanParams',
  })
export const ShowConfigParams = z
  .object({ subnet: z.string(), ip: z.string() })
  .meta({
    id: 'ShowConfigParams',
  })
export const SubnetParams = z.object({ subnet: z.string() }).meta({
  id: 'SubnetParams',
})
export const SubscribeParams = z
  .object({
    pointer: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'SubscribeParams',
  })
export const TunnelCertData = z
  .object({ key: z.string(), cert: z.string() })
  .meta({
    id: 'TunnelCertData',
  })
export const TunnelUpdateResult = z
  .object({
    /**
     * "up-to-date", "update-available", or "updating"
     */
    status: z.string().meta({
      description: '"up-to-date", "update-available", or "updating"',
    }),
    /**
     * Currently installed version
     */
    installed: z.string().meta({
      description: 'Currently installed version',
    }),
    /**
     * Available candidate version
     */
    candidate: z.string().meta({
      description: 'Available candidate version',
    }),
  })
  .meta({
    id: 'TunnelUpdateResult',
  })
export const UpdatePinholeLabelParams = z
  .object({
    gua: z.string(),
    externalPort: z.int(),
    label: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'UpdatePinholeLabelParams',
  })
export const UpdatePortForwardLabelParams = z
  .object({
    source: z.string(),
    label: wire(z.string().nullable().optional(), z.string().nullable()),
    /**
     * Label a single SNI route on `source`; omit to label the DNAT forward.
     */
    hostname: wire(
      z.string().nullable().optional(),
      z.string().nullable(),
    ).meta({
      description:
        'Label a single SNI route on `source`; omit to label the DNAT forward.',
    }),
  })
  .meta({
    id: 'UpdatePortForwardLabelParams',
  })

/**
 * A WireGuard client's role. A `Server` is a StartOS box that may configure the
 * gateway (DNS injection / auto port-forward); a `Client` is a plain peer with
 * no autoconfig. Stored and sticky — toggling the capability flags never changes
 * it; the migration backfills it from those flags.
 */
export const WgClientKind = z.enum(['client', 'server']).meta({
  id: 'WgClientKind',
  description:
    "A WireGuard client's role. A `Server` is a StartOS box that may configure the\ngateway (DNS injection / auto port-forward); a `Client` is a plain peer with\nno autoconfig. Stored and sticky — toggling the capability flags never changes\nit; the migration backfills it from those flags.",
})
export const AddDeviceParams = z
  .object({
    subnet: z.string(),
    name: z.string(),
    ip: wire(z.string().nullable().optional(), z.string().nullable()),
    /**
     * Client (no autoconfig) or Server (gateway-autoconfig on by default).
     */
    kind: wire(WgClientKind.optional(), WgClientKind).meta({
      description:
        'Client (no autoconfig) or Server (gateway-autoconfig on by default).',
    }),
  })
  .meta({
    id: 'AddDeviceParams',
  })
export const AddKeyParams = z
  .object({ name: z.string(), key: AnyVerifyingKey })
  .meta({
    id: 'AddKeyParams',
  })
export const ApplyWithPathParams = z
  .intersection(
    z.object({
      path: wire(z.string().nullable().optional(), z.string().nullable()),
    }),
    ApplyParams,
  )
  .meta({
    id: 'ApplyWithPathParams',
  })

/**
 * The server's enrolled auth keys, keyed by their PEM encoding. Each enrolled
 * key is a sign-in: it carries the same metadata a session used to (when it
 * was created, when it was last used, and the user agent that enrolled it).
 */
export const AuthKeys = z.record(z.string(), Session).meta({
  id: 'AuthKeys',
  description:
    "The server's enrolled auth keys, keyed by their PEM encoding. Each enrolled\nkey is a sign-in: it carries the same metadata a session used to (when it\nwas created, when it was last used, and the user agent that enrolled it).",
})
export const LoginParams = z
  .object({
    password: z.string(),
    /**
     * The PEM-encoded public key to enroll on a successful login. The login
     * request itself is signed with the matching secret key, so enrollment
     * proves possession.
     */
    pubkey: AnyVerifyingKey.meta({
      description:
        'The PEM-encoded public key to enroll on a successful login. The login\nrequest itself is signed with the matching secret key, so enrollment\nproves possession.',
    }),
    /**
     * Enroll in memory only, never persisted (kiosk mode, which re-enrolls
     * on every browser restart and would otherwise accumulate keys).
     */
    ephemeral: wire(z.boolean().optional(), z.boolean()).meta({
      description:
        'Enroll in memory only, never persisted (kiosk mode, which re-enrolls\non every browser restart and would otherwise accumulate keys).',
    }),
  })
  .meta({
    id: 'LoginParams',
  })
export const RemoveKeyParams = z.object({ key: AnyVerifyingKey }).meta({
  id: 'RemoveKeyParams',
})
export const SetDeviceKindParams = z
  .object({ subnet: z.string(), ip: z.string(), kind: WgClientKind })
  .meta({
    id: 'SetDeviceKindParams',
  })
export const SetSubnetDnsParams = z
  .object({
    mode: DnsMode,
    /**
     * The selected device's WireGuard IP; required when `mode` is `device`.
     */
    deviceIp: wire(
      z.string().nullable().optional(),
      z.string().nullable(),
    ).meta({
      description:
        "The selected device's WireGuard IP; required when `mode` is `device`.",
    }),
    /**
     * Up to 3 upstream servers (bare IP or `ip:port`); used when `mode` is `custom`.
     */
    servers: z.array(z.string()).meta({
      description:
        'Up to 3 upstream servers (bare IP or `ip:port`); used when `mode` is `custom`.',
    }),
  })
  .meta({
    id: 'SetSubnetDnsParams',
  })
export const SubscribeRes = z.object({ dump: Dump, guid: Guid }).meta({
  id: 'SubscribeRes',
})
export const WgConfig = z
  .object({
    name: z.string(),
    key: z.string(),
    psk: z.string(),
    /**
     * Client (no autoconfig) vs Server (a StartOS box). Sticky; defaulted by the
     * migration from the capability flags.
     */
    kind: wire(WgClientKind.optional(), WgClientKind).meta({
      description:
        'Client (no autoconfig) vs Server (a StartOS box). Sticky; defaulted by the\nmigration from the capability flags.',
    }),
    /**
     * Whether this device may inject DNS records via RFC 2136. Off by default
     * — only enable for devices you trust (it lets the device add records to
     * the tunnel's DNS for every peer to resolve).
     */
    allowDnsInjection: wire(z.boolean().optional(), z.boolean()).meta({
      description:
        "Whether this device may inject DNS records via RFC 2136. Off by default\n— only enable for devices you trust (it lets the device add records to\nthe tunnel's DNS for every peer to resolve).",
    }),
    /**
     * Whether this device may auto-create port forwards via PCP/IGD. Off by
     * default — paired with `allow_dns_injection` under one "Gateway
     * Autoconfiguration" toggle, but tracked separately so each capability is
     * gated on its own.
     */
    allowAutoPortForward: wire(z.boolean().optional(), z.boolean()).meta({
      description:
        'Whether this device may auto-create port forwards via PCP/IGD. Off by\ndefault — paired with `allow_dns_injection` under one "Gateway\nAutoconfiguration" toggle, but tracked separately so each capability is\ngated on its own.',
    }),
    /**
     * SNAT this device's egress to this WAN IP, overriding the subnet's
     * `wan_ip` / the default masquerade. `None` falls back to the subnet rule.
     */
    wanIp: wire(z.string().nullable().optional(), z.string().nullable()).meta({
      description:
        "SNAT this device's egress to this WAN IP, overriding the subnet's\n`wan_ip` / the default masquerade. `None` falls back to the subnet rule.",
    }),
  })
  .meta({
    id: 'WgConfig',
  })
export const SessionList = z
  .object({
    current: wire(z.string().nullable().optional(), z.string().nullable()),
    sessions: AuthKeys,
  })
  .meta({
    id: 'SessionList',
  })
export const WgSubnetClients = z.record(z.string(), WgConfig).meta({
  id: 'WgSubnetClients',
})
export const WgSubnetConfig = z
  .object({
    name: z.string(),
    clients: WgSubnetClients,
    dns: wire(DnsConfig.optional(), DnsConfig),
    /**
     * SNAT this subnet's egress to this WAN IP instead of `masquerade`. `None`
     * keeps the default masquerade; a per-device `wan_ip` overrides this.
     */
    wanIp: wire(z.string().nullable().optional(), z.string().nullable()).meta({
      description:
        "SNAT this subnet's egress to this WAN IP instead of `masquerade`. `None`\nkeeps the default masquerade; a per-device `wan_ip` overrides this.",
    }),
    /**
     * Routed IPv6 prefix delegated to this subnet, if any. Each host on the
     * subnet (the server and every client) gets one `/128` out of it with its
     * tunnel IPv4 embedded — see [`crate::tunnel::wg6`].
     */
    ipv6: wire(z.string().nullable().optional(), z.string().nullable()).meta({
      description:
        'Routed IPv6 prefix delegated to this subnet, if any. Each host on the\nsubnet (the server and every client) gets one `/128` out of it with its\ntunnel IPv4 embedded — see [`crate::tunnel::wg6`].',
    }),
  })
  .meta({
    id: 'WgSubnetConfig',
  })
export const Api = {
  _PARAMS: z.object({}) as z.ZodType<{}, {}>,
  _CHILDREN: {
    auth: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        key: {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _CHILDREN: {
            add: { _PARAMS: AddKeyParams, _RETURN: z.null() },
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
              _RETURN: AuthKeys,
            },
            remove: { _PARAMS: RemoveKeyParams, _RETURN: z.null() },
          },
        },
        login: { _PARAMS: LoginParams, _RETURN: z.null() },
        logout: {
          _PARAMS: LogoutParams,
          _RETURN: HasUnenrolledKeys.nullable(),
        },
        session: {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _CHILDREN: {
            kill: { _PARAMS: KillParams, _RETURN: z.null() },
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
                ListParams,
              ),
              _RETURN: SessionList,
            },
          },
        },
        'set-password': { _PARAMS: SetPasswordParams, _RETURN: z.null() },
      },
    },
    db: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        apply: { _PARAMS: ApplyWithPathParams, _RETURN: z.null() },
        dump: { _PARAMS: DumpParams, _RETURN: Dump },
        subscribe: { _PARAMS: SubscribeParams, _RETURN: SubscribeRes },
      },
    },
    device: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        add: { _PARAMS: AddDeviceParams, _RETURN: z.null() },
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
            ListDevicesParams,
          ),
          _RETURN: WgSubnetConfig,
        },
        remove: { _PARAMS: RemoveDeviceParams, _RETURN: z.null() },
        'set-auto-port-forward': {
          _PARAMS: SetAutoPortForwardParams,
          _RETURN: z.null(),
        },
        'set-dns-injection': {
          _PARAMS: SetDnsInjectionParams,
          _RETURN: z.null(),
        },
        'set-kind': { _PARAMS: SetDeviceKindParams, _RETURN: z.null() },
        'set-wan': { _PARAMS: SetDeviceWanParams, _RETURN: z.null() },
        'show-config': { _PARAMS: ShowConfigParams, _RETURN: z.string() },
      },
    },
    dns: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        add: { _PARAMS: AddDnsRecordParams, _RETURN: z.null() },
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
          _RETURN: z.array(DnsRecordEntry),
        },
        remove: { _PARAMS: RemoveDnsRecordParams, _RETURN: z.null() },
      },
    },
    'http-redirect': {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
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
          _RETURN: z.array(HttpRedirectStatus),
        },
        'set-enabled': {
          _PARAMS: SetHttpRedirectEnabledParams,
          _RETURN: z.null(),
        },
      },
    },
    pinhole: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        add: { _PARAMS: AddPinholeParams, _RETURN: z.null() },
        remove: { _PARAMS: RemovePinholeParams, _RETURN: z.null() },
        'set-enabled': { _PARAMS: SetPinholeEnabledParams, _RETURN: z.null() },
        'update-label': {
          _PARAMS: UpdatePinholeLabelParams,
          _RETURN: z.null(),
        },
      },
    },
    'port-forward': {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        add: { _PARAMS: AddPortForwardParams, _RETURN: z.null() },
        remove: { _PARAMS: RemovePortForwardParams, _RETURN: z.null() },
        'set-enabled': {
          _PARAMS: SetPortForwardEnabledParams,
          _RETURN: z.null(),
        },
        'update-label': {
          _PARAMS: UpdatePortForwardLabelParams,
          _RETURN: z.null(),
        },
      },
    },
    restart: { _PARAMS: z.object({}) as z.ZodType<{}, {}>, _RETURN: z.null() },
    subnet: {
      _PARAMS: SubnetParams,
      _CHILDREN: {
        add: { _PARAMS: AddSubnetParams, _RETURN: z.null() },
        remove: {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _RETURN: z.null(),
        },
        'set-dns': { _PARAMS: SetSubnetDnsParams, _RETURN: z.null() },
        'set-ipv6': { _PARAMS: SetSubnetIpv6Params, _RETURN: z.null() },
        'set-wan': { _PARAMS: SetSubnetWanParams, _RETURN: z.null() },
      },
    },
    update: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        apply: {
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
          _RETURN: TunnelUpdateResult,
        },
        check: {
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
          _RETURN: TunnelUpdateResult,
        },
      },
    },
    web: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        disable: {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _RETURN: z.null(),
        },
        enable: {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _RETURN: z.null(),
        },
        'generate-certificate': {
          _PARAMS: GenerateCertParams,
          _RETURN: z.string(),
        },
        'get-available-ips': {
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
        'get-certificate': {
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
          _RETURN: z.string().nullable(),
        },
        'get-listen': {
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
          _RETURN: z.string().nullable(),
        },
        'import-certificate': { _PARAMS: TunnelCertData, _RETURN: z.null() },
        'set-listen': { _PARAMS: SetListenParams, _RETURN: z.null() },
        uninit: {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _RETURN: z.null(),
        },
      },
    },
  },
}
