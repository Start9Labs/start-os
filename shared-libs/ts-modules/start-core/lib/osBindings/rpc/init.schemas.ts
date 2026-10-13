import { z } from 'zod'
const wire = <A extends z.ZodType, B extends z.ZodType>(input: A, output: B) =>
  z.codec(input, output, { decode: v => v as never, encode: v => v as never })
const object = <Shape extends z.core.$ZodLooseShape>(shape: Shape) =>
  z.object(shape).meta({ additionalProperties: true })
const literal_0 = z.union([z.number(), z.string()]).nullable()
const literal_1 = z.string()
const literal_2 = z.number()
const literal_3 = z.number().nullable()
const literal_4 = z.string().nullable()
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
export const NamedProgress: z.ZodObject<{
  name: z.ZodString
  progress: typeof Progress
}> = object({
  name: z.string(),
  get progress(): typeof Progress {
    return Progress
  },
}).meta({
  id: 'NamedProgress',
})
export const PackageId: typeof literal_1 = literal_1.meta({
  id: 'PackageId',
})
export const ProgressUnits: z.ZodEnum<{ bytes: 'bytes'; steps: 'steps' }> = z
  .enum(['bytes', 'steps'])
  .meta({
    id: 'ProgressUnits',
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
  ipv4: z.ZodCodec<z.ZodOptional<typeof literal_4>, typeof literal_4>
  ipv6: z.ZodCodec<z.ZodOptional<typeof literal_4>, typeof literal_4>
}> = object({
  ipv4: wire(literal_4.optional(), literal_4),
  ipv6: wire(literal_4.optional(), literal_4),
}).meta({
  id: 'QueryDnsRes',
  description:
    "What a public domain currently resolves to, per address family. A public\ndomain is DualStack: the operator points an `A` record at the gateway's WAN\nIPv4 and an `AAAA` at the box's IPv6 GUA. Either may be absent.",
})
export const FullProgress: z.ZodObject<{
  overall: typeof Progress
  phases: z.ZodArray<typeof NamedProgress>
}> = object({
  get overall(): typeof Progress {
    return Progress
  },
  phases: z.array(NamedProgress),
}).meta({
  id: 'FullProgress',
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
export const InitProgressRes: z.ZodObject<{
  progress: typeof FullProgress
  guid: typeof Guid
}> = object({ progress: FullProgress, guid: Guid }).meta({
  id: 'InitProgressRes',
})
export const Progress: z.ZodUnion<
  readonly [
    z.ZodNull,
    z.ZodBoolean,
    z.ZodObject<{
      done: typeof literal_2
      total: z.ZodCodec<z.ZodOptional<typeof literal_3>, typeof literal_3>
      units: z.ZodCodec<
        z.ZodOptional<z.ZodNullable<typeof ProgressUnits>>,
        z.ZodNullable<typeof ProgressUnits>
      >
    }>,
    typeof FullProgress,
  ]
> = z
  .union([
    z.null(),
    z.boolean(),
    object({
      done: literal_2,
      total: wire(literal_3.optional(), literal_3),
      units: wire(
        ProgressUnits.nullable().optional(),
        ProgressUnits.nullable(),
      ),
    }),
    FullProgress.meta({
      description:
        'A sub-tree of progress. Used by `NamedProgress.progress` so a phase\ncan carry its own `FullProgress` with sub-phases. Producers are\nexpected to keep `FullProgress.overall` scalar (not nested again).',
    }),
  ])
  .meta({
    id: 'Progress',
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
    init: {
      _PARAMS: object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
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
        subscribe: {
          _PARAMS: object({}) as z.ZodType<{}, {}>,
          _RETURN: InitProgressRes,
        },
      },
    },
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
