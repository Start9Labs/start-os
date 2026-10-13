import { z } from 'zod'
const wire = <A extends z.ZodType, B extends z.ZodType>(input: A, output: B) =>
  z.codec(input, output, { decode: v => v as never, encode: v => v as never })
const object = <Shape extends z.core.$ZodLooseShape>(shape: Shape) =>
  z.object(shape).meta({ additionalProperties: true })
const literal_0 = z.string().nullable()
const literal_1 = z.string()
const literal_2 = z.number()
const literal_3 = z.number().nullable()
const literal_4 = z.union([z.number(), z.string()]).nullable()
export const Cifs: z.ZodObject<{
  hostname: z.ZodString
  path: z.ZodString
  username: z.ZodString
  password: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<z.ZodString>>,
    z.ZodNullable<z.ZodString>
  >
}> = object({
  hostname: z.string(),
  path: z.string(),
  username: z.string(),
  password: wire(z.string().nullable().optional(), z.string().nullable()),
}).meta({
  id: 'Cifs',
})
export const DataDrive: z.ZodObject<{
  stablePath: z.ZodString
  wipe: z.ZodBoolean
}> = object({ stablePath: z.string(), wipe: z.boolean() }).meta({
  id: 'DataDrive',
})
export const EncryptedWire: z.ZodObject<{ encrypted: z.ZodUnknown }> = object({
  encrypted: z.unknown(),
}).meta({
  id: 'EncryptedWire',
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
export const KeyboardOptions: z.ZodObject<{
  layout: z.ZodString
  keymap: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<z.ZodString>>,
    z.ZodNullable<z.ZodString>
  >
  model: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<z.ZodString>>,
    z.ZodNullable<z.ZodString>
  >
  variant: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<z.ZodString>>,
    z.ZodNullable<z.ZodString>
  >
  options: z.ZodCodec<
    z.ZodOptional<z.ZodArray<z.ZodString>>,
    z.ZodArray<z.ZodString>
  >
}> = object({
  layout: z.string(),
  keymap: wire(z.string().nullable().optional(), z.string().nullable()),
  model: wire(z.string().nullable().optional(), z.string().nullable()),
  variant: wire(z.string().nullable().optional(), z.string().nullable()),
  options: wire(z.array(z.string()).optional(), z.array(z.string())),
}).meta({
  id: 'KeyboardOptions',
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
export const PartitionTable: z.ZodEnum<{ mbr: 'mbr'; gpt: 'gpt' }> = z
  .enum(['mbr', 'gpt'])
  .meta({
    id: 'PartitionTable',
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
  ipv4: z.ZodCodec<z.ZodOptional<typeof literal_0>, typeof literal_0>
  ipv6: z.ZodCodec<z.ZodOptional<typeof literal_0>, typeof literal_0>
}> = object({
  ipv4: wire(literal_0.optional(), literal_0),
  ipv6: wire(literal_0.optional(), literal_0),
}).meta({
  id: 'QueryDnsRes',
  description:
    "What a public domain currently resolves to, per address family. A public\ndomain is DualStack: the operator points an `A` record at the gateway's WAN\nIPv4 and an `AAAA` at the box's IPv6 GUA. Either may be absent.",
})

/**
 * A migration or backup recovery source with a caller-selected backup password representation.
 */
export const RecoverySource = <Password extends z.ZodType>(
  Password: Password,
) => {
  const self = z.union([
    z.intersection(
      object({ type: z.literal('migrate') }),
      object({ guid: z.string() }),
    ),
    z.intersection(
      object({ type: z.literal('backup') }),
      object({
        target: BackupTargetFS,
        password: Password,
        serverId: z.string(),
      }),
    ),
  ])
  return self
}
export const ServerHostname: typeof literal_1 = literal_1.meta({
  id: 'ServerHostname',
})
export const SetLanguageParams: z.ZodObject<{ language: z.ZodString }> = object(
  { language: z.string() },
).meta({
  id: 'SetLanguageParams',
})
export const SetupInfo: z.ZodObject<{
  guid: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<z.ZodString>>,
    z.ZodNullable<z.ZodString>
  >
  attach: z.ZodBoolean
  mokEnrolled: z.ZodBoolean
  /**
   * The whole disk the OS is installed on, recorded by whatever installed it
   * (os_install, or init_resize for a pre-installed image). When the device
   * is pre-installed the wizard fixes this as the OS drive and asks only for
   * a data drive.
   */
  osDrive: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<z.ZodString>>,
    z.ZodNullable<z.ZodString>
  >
}> = object({
  guid: wire(z.string().nullable().optional(), z.string().nullable()),
  attach: z.boolean(),
  mokEnrolled: z.boolean(),
  /**
   * The whole disk the OS is installed on, recorded by whatever installed it
   * (os_install, or init_resize for a pre-installed image). When the device
   * is pre-installed the wizard fixes this as the OS drive and asks only for
   * a data drive.
   */
  osDrive: wire(z.string().nullable().optional(), z.string().nullable()).meta({
    description:
      'The whole disk the OS is installed on, recorded by whatever installed it\n(os_install, or init_resize for a pre-installed image). When the device\nis pre-installed the wizard fixes this as the OS drive and asks only for\na data drive.',
  }),
}).meta({
  id: 'SetupInfo',
})
export const SetupResult: z.ZodObject<{
  hostname: typeof literal_1
  rootCa: typeof literal_1
  needsRestart: z.ZodBoolean
}> = object({
  hostname: literal_1,
  rootCa: literal_1,
  needsRestart: z.boolean(),
}).meta({
  id: 'SetupResult',
})
export const AttachParams: z.ZodObject<{
  password: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<typeof EncryptedWire>>,
    z.ZodNullable<typeof EncryptedWire>
  >
  guid: z.ZodString
  kiosk: z.ZodBoolean
}> = object({
  password: wire(EncryptedWire.nullable().optional(), EncryptedWire.nullable()),
  guid: z.string(),
  kiosk: z.boolean(),
}).meta({
  id: 'AttachParams',
})
export const BackupTargetFS: z.ZodUnion<
  readonly [
    z.ZodIntersection<
      z.ZodObject<{ type: z.ZodLiteral<'disk'> }>,
      z.ZodObject<{ logicalname: z.ZodString }>
    >,
    z.ZodIntersection<z.ZodObject<{ type: z.ZodLiteral<'cifs'> }>, typeof Cifs>,
  ]
> = z
  .union([
    z.intersection(
      object({ type: z.literal('disk') }),
      object({ logicalname: z.string() }),
    ),
    z.intersection(object({ type: z.literal('cifs') }), Cifs),
  ])
  .meta({
    id: 'BackupTargetFS',
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
export const InstallOsParams: z.ZodObject<{
  osDrive: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<z.ZodString>>,
    z.ZodNullable<z.ZodString>
  >
  dataDrive: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<typeof DataDrive>>,
    z.ZodNullable<typeof DataDrive>
  >
}> = object({
  osDrive: wire(z.string().nullable().optional(), z.string().nullable()),
  dataDrive: wire(DataDrive.nullable().optional(), DataDrive.nullable()),
}).meta({
  id: 'InstallOsParams',
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
export const SetupExecuteParams: z.ZodObject<{
  guid: z.ZodString
  password: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<typeof EncryptedWire>>,
    z.ZodNullable<typeof EncryptedWire>
  >
  recoverySource: z.ZodCodec<
    z.ZodOptional<
      z.ZodNullable<ReturnType<typeof RecoverySource<typeof EncryptedWire>>>
    >,
    z.ZodNullable<ReturnType<typeof RecoverySource<typeof EncryptedWire>>>
  >
  kiosk: z.ZodBoolean
  hostname: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<z.ZodString>>,
    z.ZodNullable<z.ZodString>
  >
}> = object({
  guid: z.string(),
  password: wire(EncryptedWire.nullable().optional(), EncryptedWire.nullable()),
  recoverySource: wire(
    RecoverySource(EncryptedWire).nullable().optional(),
    RecoverySource(EncryptedWire).nullable(),
  ),
  kiosk: z.boolean(),
  hostname: wire(z.string().nullable().optional(), z.string().nullable()),
}).meta({
  id: 'SetupExecuteParams',
})

/**
 * The public view of a backup found on a target: enough to identify it, and none of
 * [`BackupUnencryptedMetadata`]'s key material.
 */
export const StartOsRecoveryInfo: z.ZodObject<{
  hostname: typeof ServerHostname
  version: z.ZodString
  timestamp: typeof literal_1
}> = object({
  hostname: ServerHostname,
  version: z.string(),
  timestamp: literal_1,
}).meta({
  id: 'StartOsRecoveryInfo',
  description:
    "The public view of a backup found on a target: enough to identify it, and none of\n[`BackupUnencryptedMetadata`]'s key material.",
})
export const VerifyCifsParams: z.ZodObject<{
  hostname: z.ZodString
  path: z.ZodString
  username: z.ZodString
  password: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<typeof EncryptedWire>>,
    z.ZodNullable<typeof EncryptedWire>
  >
}> = object({
  hostname: z.string(),
  path: z.string(),
  username: z.string(),
  password: wire(EncryptedWire.nullable().optional(), EncryptedWire.nullable()),
}).meta({
  id: 'VerifyCifsParams',
})
export const PartitionInfo: z.ZodObject<{
  logicalname: z.ZodString
  stablePath: z.ZodString
  label: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<z.ZodString>>,
    z.ZodNullable<z.ZodString>
  >
  capacity: typeof literal_2
  used: z.ZodCodec<z.ZodOptional<typeof literal_3>, typeof literal_3>
  available: z.ZodCodec<z.ZodOptional<typeof literal_3>, typeof literal_3>
  startOs: z.ZodRecord<z.ZodString, typeof StartOsRecoveryInfo>
  legacyBackup: z.ZodBoolean
  guid: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<z.ZodString>>,
    z.ZodNullable<z.ZodString>
  >
  filesystem: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<z.ZodString>>,
    z.ZodNullable<z.ZodString>
  >
}> = object({
  logicalname: z.string(),
  stablePath: z.string(),
  label: wire(z.string().nullable().optional(), z.string().nullable()),
  capacity: literal_2,
  used: wire(literal_3.optional(), literal_3),
  available: wire(literal_3.optional(), literal_3),
  startOs: z.record(z.string(), StartOsRecoveryInfo),
  legacyBackup: z.boolean(),
  guid: wire(z.string().nullable().optional(), z.string().nullable()),
  filesystem: wire(z.string().nullable().optional(), z.string().nullable()),
}).meta({
  id: 'PartitionInfo',
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
export const SetupProgress: z.ZodObject<{
  progress: typeof FullProgress
  guid: typeof Guid
}> = object({ progress: FullProgress, guid: Guid }).meta({
  id: 'SetupProgress',
})
export const DiskInfo: z.ZodObject<{
  logicalname: z.ZodString
  stablePath: z.ZodString
  partitionTable: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<typeof PartitionTable>>,
    z.ZodNullable<typeof PartitionTable>
  >
  vendor: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<z.ZodString>>,
    z.ZodNullable<z.ZodString>
  >
  model: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<z.ZodString>>,
    z.ZodNullable<z.ZodString>
  >
  partitions: z.ZodArray<typeof PartitionInfo>
  capacity: z.ZodInt
  guid: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<z.ZodString>>,
    z.ZodNullable<z.ZodString>
  >
  filesystem: z.ZodCodec<
    z.ZodOptional<z.ZodNullable<z.ZodString>>,
    z.ZodNullable<z.ZodString>
  >
}> = object({
  logicalname: z.string(),
  stablePath: z.string(),
  partitionTable: wire(
    PartitionTable.nullable().optional(),
    PartitionTable.nullable(),
  ),
  vendor: wire(z.string().nullable().optional(), z.string().nullable()),
  model: wire(z.string().nullable().optional(), z.string().nullable()),
  partitions: z.array(PartitionInfo),
  capacity: z.int(),
  guid: wire(z.string().nullable().optional(), z.string().nullable()),
  filesystem: wire(z.string().nullable().optional(), z.string().nullable()),
}).meta({
  id: 'DiskInfo',
})
export const SetupStatusRes: z.ZodUnion<
  readonly [
    z.ZodObject<{ status: z.ZodLiteral<'needs-install'> }>,
    z.ZodIntersection<
      z.ZodObject<{ status: z.ZodLiteral<'incomplete'> }>,
      typeof SetupInfo
    >,
    z.ZodIntersection<
      z.ZodObject<{ status: z.ZodLiteral<'running'> }>,
      typeof SetupProgress
    >,
    z.ZodIntersection<
      z.ZodObject<{ status: z.ZodLiteral<'complete'> }>,
      typeof SetupResult
    >,
  ]
> = z
  .union([
    object({ status: z.literal('needs-install') }),
    z.intersection(object({ status: z.literal('incomplete') }), SetupInfo),
    z.intersection(object({ status: z.literal('running') }), SetupProgress),
    z.intersection(object({ status: z.literal('complete') }), SetupResult),
  ])
  .meta({
    id: 'SetupStatusRes',
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
        attach: { _PARAMS: AttachParams, _RETURN: SetupProgress },
        cifs: {
          _PARAMS: object({}) as z.ZodType<{}, {}>,
          _CHILDREN: {
            verify: {
              _PARAMS: VerifyCifsParams,
              _RETURN: z.record(z.string(), StartOsRecoveryInfo),
            },
          },
        },
        complete: {
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
          _RETURN: SetupResult,
        },
        disk: {
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
              _RETURN: z.array(DiskInfo),
            },
          },
        },
        execute: { _PARAMS: SetupExecuteParams, _RETURN: SetupProgress },
        exit: { _PARAMS: object({}) as z.ZodType<{}, {}>, _RETURN: z.null() },
        'get-pubkey': {
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
          _RETURN: z.record(z.string(), z.unknown()),
        },
        'install-os': { _PARAMS: InstallOsParams, _RETURN: SetupInfo },
        logs: {
          _PARAMS: z.intersection(
            object({
              limit: wire(z.int().nullable().optional(), z.int().nullable()),
              cursor: wire(
                z.string().nullable().optional(),
                z.string().nullable(),
              ),
              boot: wire(literal_4.optional(), literal_4),
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
        restart: {
          _PARAMS: object({}) as z.ZodType<{}, {}>,
          _RETURN: z.null(),
        },
        'set-keyboard': { _PARAMS: KeyboardOptions, _RETURN: z.null() },
        'set-language': { _PARAMS: SetLanguageParams, _RETURN: z.null() },
        shutdown: {
          _PARAMS: object({}) as z.ZodType<{}, {}>,
          _RETURN: z.null(),
        },
        status: {
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
          _RETURN: SetupStatusRes,
        },
      },
    },
    ssh: { _PARAMS: object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
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
