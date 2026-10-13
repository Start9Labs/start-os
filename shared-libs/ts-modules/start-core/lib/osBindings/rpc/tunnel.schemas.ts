import { z } from 'zod'
const wire = <A extends z.ZodType, B extends z.ZodType>(input: A, output: B) =>
  z.codec(input, output, { decode: v => v as never, encode: v => v as never })
const object = <Shape extends z.core.$ZodLooseShape>(shape: Shape) =>
  z.object(shape).meta({ additionalProperties: true })
const literal_0 = z.string()
const literal_1 = z.string().nullable()
const literal_2 = z.array(z.string())
export const AddDnsRecordParams: z.ZodObject<{
  name: z.ZodString
  type: z.ZodString
  value: z.ZodString
  ttl: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<z.ZodInt>>,
    z.ZodNullable<z.ZodInt>
  >
}> = object({
  name: z.string(),
  type: z.string(),
  value: z.string(),
  ttl: wire(z.int().nullable().optional(), z.int().nullable()),
}).meta({
  id: 'AddDnsRecordParams',
})
export const AddPinholeParams: z.ZodObject<{
  /**
   * The client's global IPv6 (GUA) to expose. Must be an address this tunnel
   * delegates to a client — its subnet needs an IPv6 prefix.
   */
  gua: z.ZodString
  /**
   * External port opened on the GUA.
   */
  externalPort: z.ZodInt
  /**
   * Destination port on the GUA. Omit for a pure pinhole (no NAT, internal ==
   * external); set a different value for a port remap (e.g. 80 -> 443).
   */
  internalPort: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<z.ZodInt>>,
    z.ZodNullable<z.ZodInt>
  >
  label: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<z.ZodString>>,
    z.ZodNullable<z.ZodString>
  >
  /**
   * Number of contiguous ports, counting up from external/internal. Default 1.
   */
  count: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<z.ZodInt>>,
    z.ZodNullable<z.ZodInt>
  >
}> = object({
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
}).meta({
  id: 'AddPinholeParams',
})
export const AddPortForwardParams: z.ZodObject<{
  /**
   * External (WAN) port to forward. The external IP is fixed to the target's
   * WAN so return traffic stays symmetric.
   */
  externalPort: z.ZodInt
  target: z.ZodString
  label: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<z.ZodString>>,
    z.ZodNullable<z.ZodString>
  >
  /**
   * Hostnames to SNI-demux on the shared external port. Empty = normal DNAT.
   */
  sni: z.ZodCodec<
    z.ZodOptional<z.ZodArray<z.ZodString>>,
    z.ZodArray<z.ZodString>
  >
  /**
   * Number of contiguous ports to forward (a PCP PORT_SET range), counting up
   * from both `external_port` and the target port. Defaults to 1. Not valid
   * together with SNI demux.
   */
  count: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<z.ZodInt>>,
    z.ZodNullable<z.ZodInt>
  >
}> = object({
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
}).meta({
  id: 'AddPortForwardParams',
})
export const AddSubnetParams: z.ZodObject<{ name: z.ZodString }> = object({
  name: z.string(),
}).meta({
  id: 'AddSubnetParams',
})
export const AnyVerifyingKey: typeof literal_0 = literal_0.meta({
  id: 'AnyVerifyingKey',
})
export const ApplyParams: z.ZodObject<{ expr: z.ZodString }> = object({
  expr: z.string(),
}).meta({
  id: 'ApplyParams',
})

/**
 * Per-subnet DNS proxy configuration. WireGuard clients on a subnet are pointed
 * at the subnet's in-tunnel server address (`.1`) for DNS; the forward-only proxy
 * listening there resolves every query against the upstream(s) selected here.
 */
export const DnsConfig: z.ZodUnion<
  readonly [
    z.ZodObject<{
      /**
       * Forward to the VPS's own system resolvers.
       */
      type: z.ZodLiteral<'default'>
    }>,
    z.ZodIntersection<
      z.ZodObject<{
        /**
         * Forward to a device on this subnet (its WireGuard IP) on port 53.
         */
        type: z.ZodLiteral<'device'>
      }>,
      z.ZodObject<{ ip: z.ZodString }>
    >,
    z.ZodIntersection<
      z.ZodObject<{
        /**
         * Forward to operator-specified upstream servers (1-3, optional `:port`).
         */
        type: z.ZodLiteral<'custom'>
      }>,
      z.ZodObject<{ servers: typeof literal_2 }>
    >,
  ]
> = z
  .union([
    object({
      /**
       * Forward to the VPS's own system resolvers.
       */
      type: z.literal('default').meta({
        description: "Forward to the VPS's own system resolvers.",
      }),
    }),
    z.intersection(
      object({
        /**
         * Forward to a device on this subnet (its WireGuard IP) on port 53.
         */
        type: z.literal('device').meta({
          description:
            'Forward to a device on this subnet (its WireGuard IP) on port 53.',
        }),
      }),
      object({ ip: z.string() }),
    ),
    z.intersection(
      object({
        /**
         * Forward to operator-specified upstream servers (1-3, optional `:port`).
         */
        type: z.literal('custom').meta({
          description:
            'Forward to operator-specified upstream servers (1-3, optional `:port`).',
        }),
      }),
      object({ servers: literal_2 }),
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
export const DnsMode: z.ZodEnum<{
  default: 'default'
  device: 'device'
  custom: 'custom'
}> = z.enum(['default', 'device', 'custom']).meta({
  id: 'DnsMode',
  description:
    "Which upstream a subnet's DNS proxy forwards to. `Device`/`Custom` draw\ntheir data from companion fields on [`SetSubnetDnsParams`].",
})

/**
 * A DNS record served by the tunnel (injected via RFC 2136 or added manually).
 * `value` is the rdata as text: an IP for A/AAAA, a name for CNAME, etc.
 */
export const DnsRecordEntry: z.ZodObject<{
  name: z.ZodString
  type: z.ZodString
  value: z.ZodString
  ttl: z.ZodInt
  /**
   * The device IP that injected this, or `null` for a manual record.
   */
  source: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<z.ZodString>>,
    z.ZodNullable<z.ZodString>
  >
}> = object({
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
}).meta({
  id: 'DnsRecordEntry',
  description:
    'A DNS record served by the tunnel (injected via RFC 2136 or added manually).\n`value` is the rdata as text: an IP for A/AAAA, a name for CNAME, etc.',
})
export const Dump: z.ZodObject<{ id: z.ZodInt; value: z.ZodUnknown }> = object({
  id: z.int(),
  value: z.unknown(),
}).meta({
  id: 'Dump',
})
export const DumpParams: z.ZodObject<{
  pointer: z.ZodCodec<z.ZodOptional<typeof literal_1>, typeof literal_1>
}> = object({ pointer: wire(literal_1.optional(), literal_1) }).meta({
  id: 'DumpParams',
})
export const GenerateCertParams: z.ZodObject<{
  subject: z.ZodArray<z.ZodString>
}> = object({ subject: z.array(z.string()) }).meta({
  id: 'GenerateCertParams',
})
export const Guid: typeof literal_0 = literal_0.meta({
  id: 'Guid',
})

/**
 * Proof that a set of auth keys was unenrolled — removed from the persisted
 * and ephemeral signer stores with any continuations they opened killed.
 * Obtained via [`SignatureAuthContext::unenroll`], or [`Self::unenroll`]
 * from inside a db transaction.
 */
export const HasUnenrolledKeys: z.ZodNull = z.null().meta({
  id: 'HasUnenrolledKeys',
  description:
    'Proof that a set of auth keys was unenrolled — removed from the persisted\nand ephemeral signer stores with any continuations they opened killed.\nObtained via [`SignatureAuthContext::unenroll`], or [`Self::unenroll`]\nfrom inside a db transaction.',
})
export const HttpRedirectStatus: z.ZodObject<{
  ip: z.ZodString
  /**
   * Whether the redirect is on for this IP (default true).
   */
  enabled: z.ZodBoolean
  /**
   * Whether a port-forward already occupies port 80 on this IP, in which case
   * the redirect yields and does not bind.
   */
  forwarded: z.ZodBoolean
}> = object({
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
}).meta({
  id: 'HttpRedirectStatus',
})
export const IoFormat: z.ZodEnum<{
  json: 'json'
  'json-pretty': 'json-pretty'
  yaml: 'yaml'
  cbor: 'cbor'
  toml: 'toml'
  'toml-pretty': 'toml-pretty'
}> = z
  .enum(['json', 'json-pretty', 'yaml', 'cbor', 'toml', 'toml-pretty'])
  .meta({
    id: 'IoFormat',
  })
export const KillParams: z.ZodObject<{ ids: z.ZodArray<z.ZodString> }> = object(
  { ids: z.array(z.string()) },
).meta({
  id: 'KillParams',
})
export const ListDevicesParams: z.ZodObject<{ subnet: typeof literal_0 }> =
  object({ subnet: literal_0 }).meta({
    id: 'ListDevicesParams',
  })
export const ListParams: z.ZodType<{}, {}> = (
  object({}) as z.ZodType<{}, {}>
).meta({
  id: 'ListParams',
})
export const LogoutParams: z.ZodType<{}, {}> = (
  object({}) as z.ZodType<{}, {}>
).meta({
  id: 'LogoutParams',
})
export const RemoveDeviceParams: z.ZodObject<{
  subnet: typeof literal_0
  ip: z.ZodString
}> = object({ subnet: literal_0, ip: z.string() }).meta({
  id: 'RemoveDeviceParams',
})
export const RemoveDnsRecordParams: z.ZodObject<{
  name: z.ZodString
  type: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<z.ZodString>>,
    z.ZodNullable<z.ZodString>
  >
}> = object({
  name: z.string(),
  type: wire(z.string().nullable().optional(), z.string().nullable()),
}).meta({
  id: 'RemoveDnsRecordParams',
})
export const RemovePinholeParams: z.ZodObject<{
  gua: z.ZodString
  externalPort: z.ZodInt
}> = object({ gua: z.string(), externalPort: z.int() }).meta({
  id: 'RemovePinholeParams',
})
export const RemovePortForwardParams: z.ZodObject<{
  source: z.ZodString
  /**
   * Remove a single SNI route on `source`; omit to remove the whole forward.
   */
  hostname: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<z.ZodString>>,
    z.ZodNullable<z.ZodString>
  >
}> = object({
  source: z.string(),
  /**
   * Remove a single SNI route on `source`; omit to remove the whole forward.
   */
  hostname: wire(z.string().nullable().optional(), z.string().nullable()).meta({
    description:
      'Remove a single SNI route on `source`; omit to remove the whole forward.',
  }),
}).meta({
  id: 'RemovePortForwardParams',
})
export const Session: z.ZodObject<{
  /**
   * A friendly name for the key, if one was assigned at enrollment (e.g.
   * tunnel device keys). UI-enrolled keys are unnamed.
   */
  name: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<z.ZodString>>,
    z.ZodNullable<z.ZodString>
  >
  loggedIn: z.ZodCodec<z.ZodOptional<typeof literal_0>, typeof literal_0>
  lastActive: z.ZodCodec<z.ZodOptional<typeof literal_0>, typeof literal_0>
  userAgent: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<z.ZodString>>,
    z.ZodNullable<z.ZodString>
  >
}> = object({
  /**
   * A friendly name for the key, if one was assigned at enrollment (e.g.
   * tunnel device keys). UI-enrolled keys are unnamed.
   */
  name: wire(z.string().nullable().optional(), z.string().nullable()).meta({
    description:
      'A friendly name for the key, if one was assigned at enrollment (e.g.\ntunnel device keys). UI-enrolled keys are unnamed.',
  }),
  loggedIn: wire(literal_0.optional(), literal_0),
  lastActive: wire(literal_0.optional(), literal_0),
  userAgent: wire(z.string().nullable().optional(), z.string().nullable()),
}).meta({
  id: 'Session',
})
export const SetAutoPortForwardParams: z.ZodObject<{
  subnet: typeof literal_0
  ip: z.ZodString
  enabled: z.ZodBoolean
}> = object({ subnet: literal_0, ip: z.string(), enabled: z.boolean() }).meta({
  id: 'SetAutoPortForwardParams',
})
export const SetDeviceWanParams: z.ZodObject<{
  subnet: typeof literal_0
  ip: z.ZodString
  wanIp: z.ZodCodec<z.ZodOptional<typeof literal_1>, typeof literal_1>
}> = object({
  subnet: literal_0,
  ip: z.string(),
  wanIp: wire(literal_1.optional(), literal_1),
}).meta({
  id: 'SetDeviceWanParams',
})
export const SetDnsInjectionParams: z.ZodObject<{
  subnet: typeof literal_0
  ip: z.ZodString
  enabled: z.ZodBoolean
}> = object({ subnet: literal_0, ip: z.string(), enabled: z.boolean() }).meta({
  id: 'SetDnsInjectionParams',
})
export const SetHttpRedirectEnabledParams: z.ZodObject<{
  /**
   * The public IPv4 whose default-on port-80 HTTP→HTTPS redirect to toggle.
   */
  ip: z.ZodString
  enabled: z.ZodBoolean
}> = object({
  /**
   * The public IPv4 whose default-on port-80 HTTP→HTTPS redirect to toggle.
   */
  ip: z.string().meta({
    description:
      'The public IPv4 whose default-on port-80 HTTP→HTTPS redirect to toggle.',
  }),
  enabled: z.boolean(),
}).meta({
  id: 'SetHttpRedirectEnabledParams',
})
export const SetListenParams: z.ZodObject<{ listen: z.ZodString }> = object({
  listen: z.string(),
}).meta({
  id: 'SetListenParams',
})
export const SetPasswordParams: z.ZodObject<{ password: z.ZodString }> = object(
  { password: z.string() },
).meta({
  id: 'SetPasswordParams',
})
export const SetPinholeEnabledParams: z.ZodObject<{
  gua: z.ZodString
  externalPort: z.ZodInt
  enabled: z.ZodBoolean
}> = object({
  gua: z.string(),
  externalPort: z.int(),
  enabled: z.boolean(),
}).meta({
  id: 'SetPinholeEnabledParams',
})
export const SetPortForwardEnabledParams: z.ZodObject<{
  source: z.ZodString
  enabled: z.ZodBoolean
  /**
   * Toggle a single SNI route on `source`; omit for a DNAT forward.
   */
  hostname: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<z.ZodString>>,
    z.ZodNullable<z.ZodString>
  >
}> = object({
  source: z.string(),
  enabled: z.boolean(),
  /**
   * Toggle a single SNI route on `source`; omit for a DNAT forward.
   */
  hostname: wire(z.string().nullable().optional(), z.string().nullable()).meta({
    description:
      'Toggle a single SNI route on `source`; omit for a DNAT forward.',
  }),
}).meta({
  id: 'SetPortForwardEnabledParams',
})
export const SetSubnetIpv6Params: z.ZodObject<{
  /**
   * The routed IPv6 prefix delegated to this subnet (e.g. a /64 from Hetzner,
   * a /56 from Linode). `null` disables IPv6 on the subnet.
   */
  prefix: z.ZodCodec<z.ZodOptional<typeof literal_1>, typeof literal_1>
}> = object({
  /**
   * The routed IPv6 prefix delegated to this subnet (e.g. a /64 from Hetzner,
   * a /56 from Linode). `null` disables IPv6 on the subnet.
   */
  prefix: wire(literal_1.optional(), literal_1).meta({
    description:
      'The routed IPv6 prefix delegated to this subnet (e.g. a /64 from Hetzner,\na /56 from Linode). `null` disables IPv6 on the subnet.',
  }),
}).meta({
  id: 'SetSubnetIpv6Params',
})
export const SetSubnetWanParams: z.ZodObject<{
  wanIp: z.ZodCodec<z.ZodOptional<typeof literal_1>, typeof literal_1>
}> = object({ wanIp: wire(literal_1.optional(), literal_1) }).meta({
  id: 'SetSubnetWanParams',
})
export const ShowConfigParams: z.ZodObject<{
  subnet: typeof literal_0
  ip: z.ZodString
}> = object({ subnet: literal_0, ip: z.string() }).meta({
  id: 'ShowConfigParams',
})
export const SubnetParams: z.ZodObject<{ subnet: typeof literal_0 }> = object({
  subnet: literal_0,
}).meta({
  id: 'SubnetParams',
})
export const SubscribeParams: z.ZodObject<{
  pointer: z.ZodCodec<z.ZodOptional<typeof literal_1>, typeof literal_1>
}> = object({ pointer: wire(literal_1.optional(), literal_1) }).meta({
  id: 'SubscribeParams',
})
export const TunnelCertData: z.ZodObject<{
  key: typeof literal_0
  cert: typeof literal_0
}> = object({ key: literal_0, cert: literal_0 }).meta({
  id: 'TunnelCertData',
})
export const TunnelUpdateResult: z.ZodObject<{
  /**
   * "up-to-date", "update-available", or "updating"
   */
  status: z.ZodString
  /**
   * Currently installed version
   */
  installed: z.ZodString
  /**
   * Available candidate version
   */
  candidate: z.ZodString
}> = object({
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
}).meta({
  id: 'TunnelUpdateResult',
})
export const UpdatePinholeLabelParams: z.ZodObject<{
  gua: z.ZodString
  externalPort: z.ZodInt
  label: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<z.ZodString>>,
    z.ZodNullable<z.ZodString>
  >
}> = object({
  gua: z.string(),
  externalPort: z.int(),
  label: wire(z.string().nullable().optional(), z.string().nullable()),
}).meta({
  id: 'UpdatePinholeLabelParams',
})
export const UpdatePortForwardLabelParams: z.ZodObject<{
  source: z.ZodString
  label: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<z.ZodString>>,
    z.ZodNullable<z.ZodString>
  >
  /**
   * Label a single SNI route on `source`; omit to label the DNAT forward.
   */
  hostname: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<z.ZodString>>,
    z.ZodNullable<z.ZodString>
  >
}> = object({
  source: z.string(),
  label: wire(z.string().nullable().optional(), z.string().nullable()),
  /**
   * Label a single SNI route on `source`; omit to label the DNAT forward.
   */
  hostname: wire(z.string().nullable().optional(), z.string().nullable()).meta({
    description:
      'Label a single SNI route on `source`; omit to label the DNAT forward.',
  }),
}).meta({
  id: 'UpdatePortForwardLabelParams',
})

/**
 * A WireGuard client's role. A `Server` is a StartOS box that may configure the
 * gateway (DNS injection / auto port-forward); a `Client` is a plain peer with
 * no autoconfig. Stored and sticky — toggling the capability flags never changes
 * it; the migration backfills it from those flags.
 */
export const WgClientKind: z.ZodEnum<{ client: 'client'; server: 'server' }> = z
  .enum(['client', 'server'])
  .meta({
    id: 'WgClientKind',
    description:
      "A WireGuard client's role. A `Server` is a StartOS box that may configure the\ngateway (DNS injection / auto port-forward); a `Client` is a plain peer with\nno autoconfig. Stored and sticky — toggling the capability flags never changes\nit; the migration backfills it from those flags.",
  })
export const AddDeviceParams: z.ZodObject<{
  subnet: typeof literal_0
  name: z.ZodString
  ip: z.ZodCodec<z.ZodOptional<typeof literal_1>, typeof literal_1>
  /**
   * Client (no autoconfig) or Server (gateway-autoconfig on by default).
   */
  kind: z.ZodCodec<z.ZodOptional<typeof WgClientKind>, typeof WgClientKind>
}> = object({
  subnet: literal_0,
  name: z.string(),
  ip: wire(literal_1.optional(), literal_1),
  /**
   * Client (no autoconfig) or Server (gateway-autoconfig on by default).
   */
  kind: wire(WgClientKind.optional(), WgClientKind).meta({
    description:
      'Client (no autoconfig) or Server (gateway-autoconfig on by default).',
  }),
}).meta({
  id: 'AddDeviceParams',
})
export const AddKeyParams: z.ZodObject<{
  name: z.ZodString
  key: typeof AnyVerifyingKey
}> = object({ name: z.string(), key: AnyVerifyingKey }).meta({
  id: 'AddKeyParams',
})
export const ApplyWithPathParams: z.ZodIntersection<
  z.ZodObject<{
    path: z.ZodCodec<
      z.ZodOptional<z.ZodNullable<z.ZodString>>,
      z.ZodNullable<z.ZodString>
    >
  }>,
  typeof ApplyParams
> = z
  .intersection(
    object({
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
export const AuthKeys: z.ZodRecord<z.ZodString, typeof Session> = z
  .record(z.string(), Session)
  .meta({
    id: 'AuthKeys',
    description:
      "The server's enrolled auth keys, keyed by their PEM encoding. Each enrolled\nkey is a sign-in: it carries the same metadata a session used to (when it\nwas created, when it was last used, and the user agent that enrolled it).",
  })
export const LoginParams: z.ZodObject<{
  password: z.ZodString
  /**
   * The PEM-encoded public key to enroll on a successful login. The login
   * request itself is signed with the matching secret key, so enrollment
   * proves possession.
   */
  pubkey: typeof AnyVerifyingKey
  /**
   * Enroll in memory only, never persisted (kiosk mode, which re-enrolls
   * on every browser restart and would otherwise accumulate keys).
   */
  ephemeral: z.ZodCodec<z.ZodOptional<z.ZodBoolean>, z.ZodBoolean>
}> = object({
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
}).meta({
  id: 'LoginParams',
})
export const RemoveKeyParams: z.ZodObject<{ key: typeof AnyVerifyingKey }> =
  object({ key: AnyVerifyingKey }).meta({
    id: 'RemoveKeyParams',
  })
export const SetDeviceKindParams: z.ZodObject<{
  subnet: typeof literal_0
  ip: z.ZodString
  kind: typeof WgClientKind
}> = object({ subnet: literal_0, ip: z.string(), kind: WgClientKind }).meta({
  id: 'SetDeviceKindParams',
})
export const SetSubnetDnsParams: z.ZodObject<{
  mode: typeof DnsMode
  /**
   * The selected device's WireGuard IP; required when `mode` is `device`.
   */
  deviceIp: z.ZodCodec<z.ZodOptional<typeof literal_1>, typeof literal_1>
  /**
   * Up to 3 upstream servers (bare IP or `ip:port`); used when `mode` is `custom`.
   */
  servers: typeof literal_2
}> = object({
  mode: DnsMode,
  /**
   * The selected device's WireGuard IP; required when `mode` is `device`.
   */
  deviceIp: wire(literal_1.optional(), literal_1).meta({
    description:
      "The selected device's WireGuard IP; required when `mode` is `device`.",
  }),
  /**
   * Up to 3 upstream servers (bare IP or `ip:port`); used when `mode` is `custom`.
   */
  servers: literal_2.meta({
    description:
      'Up to 3 upstream servers (bare IP or `ip:port`); used when `mode` is `custom`.',
  }),
}).meta({
  id: 'SetSubnetDnsParams',
})
export const SubscribeRes: z.ZodObject<{
  dump: typeof Dump
  guid: typeof Guid
}> = object({ dump: Dump, guid: Guid }).meta({
  id: 'SubscribeRes',
})
export const WgConfig: z.ZodObject<{
  name: z.ZodString
  key: typeof literal_0
  psk: typeof literal_0
  /**
   * Client (no autoconfig) vs Server (a StartOS box). Sticky; defaulted by the
   * migration from the capability flags.
   */
  kind: z.ZodCodec<z.ZodOptional<typeof WgClientKind>, typeof WgClientKind>
  /**
   * Whether this device may inject DNS records via RFC 2136. Off by default
   * — only enable for devices you trust (it lets the device add records to
   * the tunnel's DNS for every peer to resolve).
   */
  allowDnsInjection: z.ZodCodec<z.ZodOptional<z.ZodBoolean>, z.ZodBoolean>
  /**
   * Whether this device may auto-create port forwards via PCP/IGD. Off by
   * default — paired with `allow_dns_injection` under one "Gateway
   * Autoconfiguration" toggle, but tracked separately so each capability is
   * gated on its own.
   */
  allowAutoPortForward: z.ZodCodec<z.ZodOptional<z.ZodBoolean>, z.ZodBoolean>
  /**
   * SNAT this device's egress to this WAN IP, overriding the subnet's
   * `wan_ip` / the default masquerade. `None` falls back to the subnet rule.
   */
  wanIp: z.ZodCodec<z.ZodOptional<typeof literal_1>, typeof literal_1>
}> = object({
  name: z.string(),
  key: literal_0,
  psk: literal_0,
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
  wanIp: wire(literal_1.optional(), literal_1).meta({
    description:
      "SNAT this device's egress to this WAN IP, overriding the subnet's\n`wan_ip` / the default masquerade. `None` falls back to the subnet rule.",
  }),
}).meta({
  id: 'WgConfig',
})
export const SessionList: z.ZodObject<{
  current: z.ZodCodec<z.ZodOptional<typeof literal_1>, typeof literal_1>
  sessions: typeof AuthKeys
}> = object({
  current: wire(literal_1.optional(), literal_1),
  sessions: AuthKeys,
}).meta({
  id: 'SessionList',
})
export const WgSubnetClients: z.ZodRecord<z.ZodString, typeof WgConfig> = z
  .record(z.string(), WgConfig)
  .meta({
    id: 'WgSubnetClients',
  })
export const WgSubnetConfig: z.ZodObject<{
  name: z.ZodString
  clients: typeof WgSubnetClients
  dns: z.ZodCodec<z.ZodOptional<typeof DnsConfig>, typeof DnsConfig>
  /**
   * SNAT this subnet's egress to this WAN IP instead of `masquerade`. `None`
   * keeps the default masquerade; a per-device `wan_ip` overrides this.
   */
  wanIp: z.ZodCodec<z.ZodOptional<typeof literal_1>, typeof literal_1>
  /**
   * Routed IPv6 prefix delegated to this subnet, if any. Each host on the
   * subnet (the server and every client) gets one `/128` out of it with its
   * tunnel IPv4 embedded — see [`crate::tunnel::wg6`].
   */
  ipv6: z.ZodCodec<z.ZodOptional<typeof literal_1>, typeof literal_1>
}> = object({
  name: z.string(),
  clients: WgSubnetClients,
  dns: wire(DnsConfig.optional(), DnsConfig),
  /**
   * SNAT this subnet's egress to this WAN IP instead of `masquerade`. `None`
   * keeps the default masquerade; a per-device `wan_ip` overrides this.
   */
  wanIp: wire(literal_1.optional(), literal_1).meta({
    description:
      "SNAT this subnet's egress to this WAN IP instead of `masquerade`. `None`\nkeeps the default masquerade; a per-device `wan_ip` overrides this.",
  }),
  /**
   * Routed IPv6 prefix delegated to this subnet, if any. Each host on the
   * subnet (the server and every client) gets one `/128` out of it with its
   * tunnel IPv4 embedded — see [`crate::tunnel::wg6`].
   */
  ipv6: wire(literal_1.optional(), literal_1).meta({
    description:
      'Routed IPv6 prefix delegated to this subnet, if any. Each host on the\nsubnet (the server and every client) gets one `/128` out of it with its\ntunnel IPv4 embedded — see [`crate::tunnel::wg6`].',
  }),
}).meta({
  id: 'WgSubnetConfig',
})
export const Api = {
  _PARAMS: object({}) as z.ZodType<{}, {}>,
  _CHILDREN: {
    auth: {
      _PARAMS: object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        key: {
          _PARAMS: object({}) as z.ZodType<{}, {}>,
          _CHILDREN: {
            add: { _PARAMS: AddKeyParams, _RETURN: z.null() },
            list: {
              _PARAMS: z.intersection(
                z.intersection(
                  object({
                    format: wire(
                      IoFormat.nullable().optional(),
                      IoFormat.nullable(),
                    ),
                  }),
                  object({}) as z.ZodType<{}, {}>,
                ),
                object({}) as z.ZodType<{}, {}>,
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
          _PARAMS: object({}) as z.ZodType<{}, {}>,
          _CHILDREN: {
            kill: { _PARAMS: KillParams, _RETURN: z.null() },
            list: {
              _PARAMS: z.intersection(
                z.intersection(
                  object({
                    format: wire(
                      IoFormat.nullable().optional(),
                      IoFormat.nullable(),
                    ),
                  }),
                  object({}) as z.ZodType<{}, {}>,
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
      _PARAMS: object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        apply: { _PARAMS: ApplyWithPathParams, _RETURN: z.null() },
        dump: { _PARAMS: DumpParams, _RETURN: Dump },
        subscribe: { _PARAMS: SubscribeParams, _RETURN: SubscribeRes },
      },
    },
    device: {
      _PARAMS: object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        add: { _PARAMS: AddDeviceParams, _RETURN: z.null() },
        list: {
          _PARAMS: z.intersection(
            z.intersection(
              object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              object({}) as z.ZodType<{}, {}>,
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
      _PARAMS: object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        add: { _PARAMS: AddDnsRecordParams, _RETURN: z.null() },
        list: {
          _PARAMS: z.intersection(
            z.intersection(
              object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              object({}) as z.ZodType<{}, {}>,
            ),
            object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: z.array(DnsRecordEntry),
        },
        remove: { _PARAMS: RemoveDnsRecordParams, _RETURN: z.null() },
      },
    },
    'http-redirect': {
      _PARAMS: object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        list: {
          _PARAMS: z.intersection(
            z.intersection(
              object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              object({}) as z.ZodType<{}, {}>,
            ),
            object({}) as z.ZodType<{}, {}>,
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
      _PARAMS: object({}) as z.ZodType<{}, {}>,
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
      _PARAMS: object({}) as z.ZodType<{}, {}>,
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
    restart: { _PARAMS: object({}) as z.ZodType<{}, {}>, _RETURN: z.null() },
    subnet: {
      _PARAMS: SubnetParams,
      _CHILDREN: {
        add: { _PARAMS: AddSubnetParams, _RETURN: z.null() },
        remove: { _PARAMS: object({}) as z.ZodType<{}, {}>, _RETURN: z.null() },
        'set-dns': { _PARAMS: SetSubnetDnsParams, _RETURN: z.null() },
        'set-ipv6': { _PARAMS: SetSubnetIpv6Params, _RETURN: z.null() },
        'set-wan': { _PARAMS: SetSubnetWanParams, _RETURN: z.null() },
      },
    },
    update: {
      _PARAMS: object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        apply: {
          _PARAMS: z.intersection(
            z.intersection(
              object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              object({}) as z.ZodType<{}, {}>,
            ),
            object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: TunnelUpdateResult,
        },
        check: {
          _PARAMS: z.intersection(
            z.intersection(
              object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              object({}) as z.ZodType<{}, {}>,
            ),
            object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: TunnelUpdateResult,
        },
      },
    },
    web: {
      _PARAMS: object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        disable: {
          _PARAMS: object({}) as z.ZodType<{}, {}>,
          _RETURN: z.null(),
        },
        enable: { _PARAMS: object({}) as z.ZodType<{}, {}>, _RETURN: z.null() },
        'generate-certificate': {
          _PARAMS: GenerateCertParams,
          _RETURN: literal_0,
        },
        'get-available-ips': {
          _PARAMS: z.intersection(
            z.intersection(
              object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              object({}) as z.ZodType<{}, {}>,
            ),
            object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: z.array(z.string()),
        },
        'get-certificate': {
          _PARAMS: z.intersection(
            z.intersection(
              object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              object({}) as z.ZodType<{}, {}>,
            ),
            object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: literal_0.nullable(),
        },
        'get-listen': {
          _PARAMS: z.intersection(
            z.intersection(
              object({
                format: wire(
                  IoFormat.nullable().optional(),
                  IoFormat.nullable(),
                ),
              }),
              object({}) as z.ZodType<{}, {}>,
            ),
            object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: z.string().nullable(),
        },
        'import-certificate': { _PARAMS: TunnelCertData, _RETURN: z.null() },
        'set-listen': { _PARAMS: SetListenParams, _RETURN: z.null() },
        uninit: { _PARAMS: object({}) as z.ZodType<{}, {}>, _RETURN: z.null() },
      },
    },
  },
}
