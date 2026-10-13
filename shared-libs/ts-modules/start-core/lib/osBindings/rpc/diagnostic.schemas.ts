import { z } from 'zod'
const wire = <A extends z.ZodType, B extends z.ZodType>(input: A, output: B) =>
  z.codec(input, output, { decode: v => v as never, encode: v => v as never })
const object = <Shape extends z.core.$ZodLooseShape>(shape: Shape) =>
  z.object(shape).meta({ additionalProperties: true })
const literal_0 = z.union([z.number(), z.string()]).nullable()
const literal_1 = z.string()
const literal_2 = z.string().nullable()
export const ApiState: z.ZodEnum<{
  error: 'error'
  initializing: 'initializing'
  running: 'running'
}> = z.enum(['error', 'initializing', 'running']).meta({
  id: 'ApiState',
})
export const Guid: typeof literal_1 = literal_1.meta({
  id: 'Guid',
})
export const HostId: typeof literal_1 = literal_1.meta({
  id: 'HostId',
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
export const LogEntry: z.ZodObject<{
  timestamp: typeof literal_1
  message: z.ZodString
  bootId: z.ZodString
}> = object({
  timestamp: literal_1,
  message: z.string(),
  bootId: z.string(),
}).meta({
  id: 'LogEntry',
})
export const PackageId: typeof literal_1 = literal_1.meta({
  id: 'PackageId',
})
export const QueryDnsParams: z.ZodObject<{ fqdn: z.ZodString }> = object({
  fqdn: z.string(),
}).meta({
  id: 'QueryDnsParams',
})

/**
 * What a public domain currently resolves to, per address family. A public
 * domain is DualStack: the operator points an `A` record at the gateway's WAN
 * IPv4 and an `AAAA` at the box's IPv6 GUA. Either may be absent.
 */
export const QueryDnsRes: z.ZodObject<{
  ipv4: z.ZodCodec<z.ZodOptional<typeof literal_2>, typeof literal_2>
  ipv6: z.ZodCodec<z.ZodOptional<typeof literal_2>, typeof literal_2>
}> = object({
  ipv4: wire(literal_2.optional(), literal_2),
  ipv6: wire(literal_2.optional(), literal_2),
}).meta({
  id: 'QueryDnsRes',
  description:
    "What a public domain currently resolves to, per address family. A public\ndomain is DualStack: the operator points an `A` record at the gateway's WAN\nIPv4 and an `AAAA` at the box's IPv6 GUA. Either may be absent.",
})
export const UpdateSystemParams: z.ZodObject<{
  registry: z.ZodString
  targetVersion: z.ZodCodec<z.ZodOptional<typeof literal_2>, typeof literal_2>
  progress: z.ZodCodec<z.ZodOptional<z.ZodBoolean>, z.ZodBoolean>
}> = object({
  registry: z.string(),
  targetVersion: wire(literal_2.optional(), literal_2),
  progress: wire(z.boolean().optional(), z.boolean()),
}).meta({
  id: 'UpdateSystemParams',
})
export const UpdateSystemRes: z.ZodObject<{
  target: z.ZodCodec<z.ZodOptional<typeof literal_2>, typeof literal_2>
  progress: z.ZodCodec<z.ZodOptional<typeof literal_2>, typeof literal_2>
}> = object({
  target: wire(literal_2.optional(), literal_2),
  progress: wire(literal_2.optional(), literal_2),
}).meta({
  id: 'UpdateSystemRes',
})
export const LogFollowResponse: z.ZodObject<{
  startCursor: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<z.ZodString>>,
    z.ZodNullable<z.ZodString>
  >
  guid: typeof Guid
}> = object({
  startCursor: wire(z.string().nullable().optional(), z.string().nullable()),
  guid: Guid,
}).meta({
  id: 'LogFollowResponse',
})
export const LogResponse: z.ZodObject<{
  entries: z.ZodArray<typeof LogEntry>
  startCursor: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<z.ZodString>>,
    z.ZodNullable<z.ZodString>
  >
  endCursor: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<z.ZodString>>,
    z.ZodNullable<z.ZodString>
  >
}> = object({
  entries: z.array(LogEntry),
  startCursor: wire(z.string().nullable().optional(), z.string().nullable()),
  endCursor: wire(z.string().nullable().optional(), z.string().nullable()),
}).meta({
  id: 'LogResponse',
})
export const RequiresHostId: z.ZodObject<{ host: typeof HostId }> = object({
  host: HostId,
}).meta({
  id: 'RequiresHostId',
})
export const RequiresPackageId: z.ZodObject<{ package: typeof PackageId }> =
  object({ package: PackageId }).meta({
    id: 'RequiresPackageId',
  })
export const Api = {
  _PARAMS: object({}) as z.ZodType<{}, {}>,
  _CHILDREN: {
    auth: {
      _PARAMS: object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        session: { _PARAMS: object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
      },
    },
    backup: {
      _PARAMS: object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        target: {
          _PARAMS: object({}) as z.ZodType<{}, {}>,
          _CHILDREN: {
            cifs: { _PARAMS: object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
          },
        },
      },
    },
    db: {
      _PARAMS: object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        put: { _PARAMS: object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
      },
    },
    diagnostic: {
      _PARAMS: object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        disk: {
          _PARAMS: object({}) as z.ZodType<{}, {}>,
          _CHILDREN: {
            forget: {
              _PARAMS: object({}) as z.ZodType<{}, {}>,
              _RETURN: z.null(),
            },
            repair: {
              _PARAMS: object({}) as z.ZodType<{}, {}>,
              _RETURN: z.null(),
            },
          },
        },
        error: {
          _PARAMS: object({}) as z.ZodType<{}, {}>,
          _RETURN: object({
            code: z.number(),
            message: z.string(),
            data: z.unknown().optional(),
          }),
        },
        'kernel-logs': {
          _PARAMS: z.intersection(
            object({
              limit: wire(z.int().nullable().optional(), z.int().nullable()),
              cursor: wire(
                z.string().nullable().optional(),
                z.string().nullable(),
              ),
              boot: wire(literal_0.optional(), literal_0),
              before: wire(z.boolean().optional(), z.boolean()),
            }),
            object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: LogResponse,
          _CHILDREN: {
            follow: {
              _PARAMS: object({}) as z.ZodType<{}, {}>,
              _RETURN: LogFollowResponse,
            },
          },
        },
        logs: {
          _PARAMS: z.intersection(
            object({
              limit: wire(z.int().nullable().optional(), z.int().nullable()),
              cursor: wire(
                z.string().nullable().optional(),
                z.string().nullable(),
              ),
              boot: wire(literal_0.optional(), literal_0),
              before: wire(z.boolean().optional(), z.boolean()),
            }),
            object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: LogResponse,
          _CHILDREN: {
            follow: {
              _PARAMS: object({}) as z.ZodType<{}, {}>,
              _RETURN: LogFollowResponse,
            },
          },
        },
        rebuild: {
          _PARAMS: object({}) as z.ZodType<{}, {}>,
          _RETURN: z.null(),
        },
        restart: {
          _PARAMS: object({}) as z.ZodType<{}, {}>,
          _RETURN: z.null(),
        },
        update: { _PARAMS: UpdateSystemParams, _RETURN: UpdateSystemRes },
      },
    },
    disk: {
      _PARAMS: object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        repair: { _PARAMS: object({}) as z.ZodType<{}, {}>, _RETURN: z.null() },
      },
    },
    'git-info': {
      _PARAMS: object({}) as z.ZodType<{}, {}>,
      _RETURN: z.string(),
    },
    init: { _PARAMS: object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
    kiosk: { _PARAMS: object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
    net: {
      _PARAMS: object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        acme: { _PARAMS: object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
        dns: {
          _PARAMS: object({}) as z.ZodType<{}, {}>,
          _CHILDREN: {
            query: {
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
                QueryDnsParams,
              ),
              _RETURN: QueryDnsRes,
            },
          },
        },
        forward: { _PARAMS: object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
        gateway: { _PARAMS: object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
        ssl: { _PARAMS: object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
        tunnel: { _PARAMS: object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
        vhost: { _PARAMS: object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
      },
    },
    notification: { _PARAMS: object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
    package: {
      _PARAMS: object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        action: { _PARAMS: object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
        backup: { _PARAMS: object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
        host: {
          _PARAMS: RequiresPackageId,
          _CHILDREN: {
            address: {
              _PARAMS: RequiresHostId,
              _CHILDREN: {
                domain: {
                  _PARAMS: object({}) as z.ZodType<{}, {}>,
                  _CHILDREN: {
                    private: {
                      _PARAMS: object({}) as z.ZodType<{}, {}>,
                      _CHILDREN: {},
                    },
                    public: {
                      _PARAMS: object({}) as z.ZodType<{}, {}>,
                      _CHILDREN: {},
                    },
                  },
                },
              },
            },
            binding: { _PARAMS: RequiresHostId, _CHILDREN: {} },
          },
        },
      },
    },
    server: {
      _PARAMS: object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        'device-info': {
          _PARAMS: z.intersection(
            object({
              format: wire(IoFormat.nullable().optional(), IoFormat.nullable()),
            }),
            object({}) as z.ZodType<{}, {}>,
          ),
          _CHILDREN: {},
        },
        experimental: {
          _PARAMS: object({}) as z.ZodType<{}, {}>,
          _CHILDREN: {},
        },
        host: {
          _PARAMS: object({}) as z.ZodType<{}, {}>,
          _CHILDREN: {
            address: {
              _PARAMS: object({}) as z.ZodType<{}, {}>,
              _CHILDREN: {
                domain: {
                  _PARAMS: object({}) as z.ZodType<{}, {}>,
                  _CHILDREN: {
                    private: {
                      _PARAMS: object({}) as z.ZodType<{}, {}>,
                      _CHILDREN: {},
                    },
                    public: {
                      _PARAMS: object({}) as z.ZodType<{}, {}>,
                      _CHILDREN: {},
                    },
                  },
                },
              },
            },
            binding: {
              _PARAMS: object({}) as z.ZodType<{}, {}>,
              _CHILDREN: {},
            },
          },
        },
        metrics: {
          _PARAMS: z.intersection(
            object({
              format: wire(IoFormat.nullable().optional(), IoFormat.nullable()),
            }),
            object({}) as z.ZodType<{}, {}>,
          ),
          _CHILDREN: {},
        },
      },
    },
    setup: {
      _PARAMS: object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        cifs: { _PARAMS: object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
        disk: { _PARAMS: object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
      },
    },
    ssh: { _PARAMS: object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
    state: { _PARAMS: object({}) as z.ZodType<{}, {}>, _RETURN: ApiState },
    util: { _PARAMS: object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
    wifi: {
      _PARAMS: object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        available: { _PARAMS: object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
        country: { _PARAMS: object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
      },
    },
  },
}
