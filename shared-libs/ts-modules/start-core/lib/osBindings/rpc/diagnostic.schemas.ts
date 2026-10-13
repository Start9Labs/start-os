import { z } from 'zod'
const wire = <A extends z.ZodType, B extends z.ZodType>(input: A, output: B) =>
  z.codec(input, output, { decode: v => v as never, encode: v => v as never })
export const ApiState = z.enum(['error', 'initializing', 'running']).meta({
  id: 'ApiState',
})
export const Guid = z.string().meta({
  id: 'Guid',
})
export const HostId = z.string().meta({
  id: 'HostId',
})
export const IoFormat = z
  .enum(['json', 'json-pretty', 'yaml', 'cbor', 'toml', 'toml-pretty'])
  .meta({
    id: 'IoFormat',
  })
export const LogEntry = z
  .object({ timestamp: z.string(), message: z.string(), bootId: z.string() })
  .meta({
    id: 'LogEntry',
  })
export const PackageId = z.string().meta({
  id: 'PackageId',
})
export const QueryDnsParams = z.object({ fqdn: z.string() }).meta({
  id: 'QueryDnsParams',
})

/**
 * What a public domain currently resolves to, per address family. A public
 * domain is DualStack: the operator points an `A` record at the gateway's WAN
 * IPv4 and an `AAAA` at the box's IPv6 GUA. Either may be absent.
 */
export const QueryDnsRes = z
  .object({
    ipv4: wire(z.string().nullable().optional(), z.string().nullable()),
    ipv6: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'QueryDnsRes',
    description:
      "What a public domain currently resolves to, per address family. A public\ndomain is DualStack: the operator points an `A` record at the gateway's WAN\nIPv4 and an `AAAA` at the box's IPv6 GUA. Either may be absent.",
  })
export const UpdateSystemParams = z
  .object({
    registry: z.string(),
    targetVersion: wire(
      z.string().nullable().optional(),
      z.string().nullable(),
    ),
    progress: wire(z.boolean().optional(), z.boolean()),
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
export const LogFollowResponse = z
  .object({
    startCursor: wire(z.string().nullable().optional(), z.string().nullable()),
    guid: Guid,
  })
  .meta({
    id: 'LogFollowResponse',
  })
export const LogResponse = z
  .object({
    entries: z.array(LogEntry),
    startCursor: wire(z.string().nullable().optional(), z.string().nullable()),
    endCursor: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'LogResponse',
  })
export const RequiresHostId = z.object({ host: HostId }).meta({
  id: 'RequiresHostId',
})
export const RequiresPackageId = z.object({ package: PackageId }).meta({
  id: 'RequiresPackageId',
})
export const Api = {
  _PARAMS: z.object({}) as z.ZodType<{}, {}>,
  _CHILDREN: {
    auth: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        session: { _PARAMS: z.object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
      },
    },
    backup: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        target: {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _CHILDREN: {
            cifs: { _PARAMS: z.object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
          },
        },
      },
    },
    db: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        put: { _PARAMS: z.object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
      },
    },
    diagnostic: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        disk: {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _CHILDREN: {
            forget: {
              _PARAMS: z.object({}) as z.ZodType<{}, {}>,
              _RETURN: z.null(),
            },
            repair: {
              _PARAMS: z.object({}) as z.ZodType<{}, {}>,
              _RETURN: z.null(),
            },
          },
        },
        error: {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _RETURN: z.object({
            code: z.number(),
            message: z.string(),
            data: z.unknown().optional(),
          }),
        },
        'kernel-logs': {
          _PARAMS: z.intersection(
            z.object({
              limit: wire(z.int().nullable().optional(), z.int().nullable()),
              cursor: wire(
                z.string().nullable().optional(),
                z.string().nullable(),
              ),
              boot: wire(
                z.union([z.number(), z.string()]).nullable().optional(),
                z.union([z.number(), z.string()]).nullable(),
              ),
              before: wire(z.boolean().optional(), z.boolean()),
            }),
            z.object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: LogResponse,
          _CHILDREN: {
            follow: {
              _PARAMS: z.object({}) as z.ZodType<{}, {}>,
              _RETURN: LogFollowResponse,
            },
          },
        },
        logs: {
          _PARAMS: z.intersection(
            z.object({
              limit: wire(z.int().nullable().optional(), z.int().nullable()),
              cursor: wire(
                z.string().nullable().optional(),
                z.string().nullable(),
              ),
              boot: wire(
                z.union([z.number(), z.string()]).nullable().optional(),
                z.union([z.number(), z.string()]).nullable(),
              ),
              before: wire(z.boolean().optional(), z.boolean()),
            }),
            z.object({}) as z.ZodType<{}, {}>,
          ),
          _RETURN: LogResponse,
          _CHILDREN: {
            follow: {
              _PARAMS: z.object({}) as z.ZodType<{}, {}>,
              _RETURN: LogFollowResponse,
            },
          },
        },
        rebuild: {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _RETURN: z.null(),
        },
        restart: {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _RETURN: z.null(),
        },
        update: { _PARAMS: UpdateSystemParams, _RETURN: UpdateSystemRes },
      },
    },
    disk: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        repair: {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _RETURN: z.null(),
        },
      },
    },
    'git-info': {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _RETURN: z.string(),
    },
    init: { _PARAMS: z.object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
    kiosk: { _PARAMS: z.object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
    net: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        acme: { _PARAMS: z.object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
        dns: {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _CHILDREN: {
            query: {
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
                QueryDnsParams,
              ),
              _RETURN: QueryDnsRes,
            },
          },
        },
        forward: { _PARAMS: z.object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
        gateway: { _PARAMS: z.object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
        ssl: { _PARAMS: z.object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
        tunnel: { _PARAMS: z.object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
        vhost: { _PARAMS: z.object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
      },
    },
    notification: { _PARAMS: z.object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
    package: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        action: { _PARAMS: z.object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
        backup: { _PARAMS: z.object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
        host: {
          _PARAMS: RequiresPackageId,
          _CHILDREN: {
            address: {
              _PARAMS: RequiresHostId,
              _CHILDREN: {
                domain: {
                  _PARAMS: z.object({}) as z.ZodType<{}, {}>,
                  _CHILDREN: {
                    private: {
                      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
                      _CHILDREN: {},
                    },
                    public: {
                      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
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
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        'device-info': {
          _PARAMS: z.intersection(
            z.object({
              format: wire(IoFormat.nullable().optional(), IoFormat.nullable()),
            }),
            z.object({}) as z.ZodType<{}, {}>,
          ),
          _CHILDREN: {},
        },
        experimental: {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _CHILDREN: {},
        },
        host: {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _CHILDREN: {
            address: {
              _PARAMS: z.object({}) as z.ZodType<{}, {}>,
              _CHILDREN: {
                domain: {
                  _PARAMS: z.object({}) as z.ZodType<{}, {}>,
                  _CHILDREN: {
                    private: {
                      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
                      _CHILDREN: {},
                    },
                    public: {
                      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
                      _CHILDREN: {},
                    },
                  },
                },
              },
            },
            binding: {
              _PARAMS: z.object({}) as z.ZodType<{}, {}>,
              _CHILDREN: {},
            },
          },
        },
        metrics: {
          _PARAMS: z.intersection(
            z.object({
              format: wire(IoFormat.nullable().optional(), IoFormat.nullable()),
            }),
            z.object({}) as z.ZodType<{}, {}>,
          ),
          _CHILDREN: {},
        },
      },
    },
    setup: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        cifs: { _PARAMS: z.object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
        disk: { _PARAMS: z.object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
      },
    },
    ssh: { _PARAMS: z.object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
    state: { _PARAMS: z.object({}) as z.ZodType<{}, {}>, _RETURN: ApiState },
    util: { _PARAMS: z.object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
    wifi: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        available: {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _CHILDREN: {},
        },
        country: { _PARAMS: z.object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
      },
    },
  },
}
