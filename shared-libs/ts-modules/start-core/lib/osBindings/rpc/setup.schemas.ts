import { z } from 'zod'
const wire = <A extends z.ZodType, B extends z.ZodType>(input: A, output: B) =>
  z.codec(input, output, { decode: v => v as never, encode: v => v as never })
export const Cifs = z
  .object({
    hostname: z.string(),
    path: z.string(),
    username: z.string(),
    password: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'Cifs',
  })
export const DataDrive = z
  .object({ stablePath: z.string(), wipe: z.boolean() })
  .meta({
    id: 'DataDrive',
  })
export const EncryptedWire = z.object({ encrypted: z.unknown() }).meta({
  id: 'EncryptedWire',
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
export const KeyboardOptions = z
  .object({
    layout: z.string(),
    keymap: wire(z.string().nullable().optional(), z.string().nullable()),
    model: wire(z.string().nullable().optional(), z.string().nullable()),
    variant: wire(z.string().nullable().optional(), z.string().nullable()),
    options: wire(z.array(z.string()).optional(), z.array(z.string())),
  })
  .meta({
    id: 'KeyboardOptions',
  })
export const LogEntry = z
  .object({ timestamp: z.string(), message: z.string(), bootId: z.string() })
  .meta({
    id: 'LogEntry',
  })
export const NamedProgress = z
  .object({
    name: z.string(),
    get progress(): typeof Progress {
      return Progress
    },
  })
  .meta({
    id: 'NamedProgress',
  })
export const PackageId = z.string().meta({
  id: 'PackageId',
})
export const PartitionTable = z.enum(['mbr', 'gpt']).meta({
  id: 'PartitionTable',
})
export const ProgressUnits = z.enum(['bytes', 'steps']).meta({
  id: 'ProgressUnits',
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

/**
 * A migration or backup recovery source with a caller-selected backup password representation.
 */
export const RecoverySource = <Password extends z.ZodType>(
  Password: Password,
) => {
  const self = z.union([
    z.intersection(
      z.object({ type: z.literal('migrate') }),
      z.object({ guid: z.string() }),
    ),
    z.intersection(
      z.object({ type: z.literal('backup') }),
      z.object({
        target: BackupTargetFS,
        password: Password,
        serverId: z.string(),
      }),
    ),
  ])
  return self
}
export const ServerHostname = z.string().meta({
  id: 'ServerHostname',
})
export const SetLanguageParams = z.object({ language: z.string() }).meta({
  id: 'SetLanguageParams',
})
export const SetupInfo = z
  .object({
    guid: wire(z.string().nullable().optional(), z.string().nullable()),
    attach: z.boolean(),
    mokEnrolled: z.boolean(),
    /**
     * The whole disk the OS is installed on, recorded by whatever installed it
     * (os_install, or init_resize for a pre-installed image). When the device
     * is pre-installed the wizard fixes this as the OS drive and asks only for
     * a data drive.
     */
    osDrive: wire(z.string().nullable().optional(), z.string().nullable()).meta(
      {
        description:
          'The whole disk the OS is installed on, recorded by whatever installed it\n(os_install, or init_resize for a pre-installed image). When the device\nis pre-installed the wizard fixes this as the OS drive and asks only for\na data drive.',
      },
    ),
  })
  .meta({
    id: 'SetupInfo',
  })
export const SetupResult = z
  .object({
    hostname: z.string(),
    rootCa: z.string(),
    needsRestart: z.boolean(),
  })
  .meta({
    id: 'SetupResult',
  })
export const AttachParams = z
  .object({
    password: wire(
      EncryptedWire.nullable().optional(),
      EncryptedWire.nullable(),
    ),
    guid: z.string(),
    kiosk: z.boolean(),
  })
  .meta({
    id: 'AttachParams',
  })
export const BackupTargetFS = z
  .union([
    z.intersection(
      z.object({ type: z.literal('disk') }),
      z.object({ logicalname: z.string() }),
    ),
    z.intersection(z.object({ type: z.literal('cifs') }), Cifs),
  ])
  .meta({
    id: 'BackupTargetFS',
  })
export const FullProgress = z
  .object({
    get overall(): typeof Progress {
      return Progress
    },
    phases: z.array(NamedProgress),
  })
  .meta({
    id: 'FullProgress',
  })
export const InstallOsParams = z
  .object({
    osDrive: wire(z.string().nullable().optional(), z.string().nullable()),
    dataDrive: wire(DataDrive.nullable().optional(), DataDrive.nullable()),
  })
  .meta({
    id: 'InstallOsParams',
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
export const SetupExecuteParams = z
  .object({
    guid: z.string(),
    password: wire(
      EncryptedWire.nullable().optional(),
      EncryptedWire.nullable(),
    ),
    recoverySource: wire(
      RecoverySource(EncryptedWire).nullable().optional(),
      RecoverySource(EncryptedWire).nullable(),
    ),
    kiosk: z.boolean(),
    hostname: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'SetupExecuteParams',
  })

/**
 * The public view of a backup found on a target: enough to identify it, and none of
 * [`BackupUnencryptedMetadata`]'s key material.
 */
export const StartOsRecoveryInfo = z
  .object({
    hostname: ServerHostname,
    version: z.string(),
    timestamp: z.string(),
  })
  .meta({
    id: 'StartOsRecoveryInfo',
    description:
      "The public view of a backup found on a target: enough to identify it, and none of\n[`BackupUnencryptedMetadata`]'s key material.",
  })
export const VerifyCifsParams = z
  .object({
    hostname: z.string(),
    path: z.string(),
    username: z.string(),
    password: wire(
      EncryptedWire.nullable().optional(),
      EncryptedWire.nullable(),
    ),
  })
  .meta({
    id: 'VerifyCifsParams',
  })
export const PartitionInfo = z
  .object({
    logicalname: z.string(),
    stablePath: z.string(),
    label: wire(z.string().nullable().optional(), z.string().nullable()),
    capacity: z.number(),
    used: wire(z.number().nullable().optional(), z.number().nullable()),
    available: wire(z.number().nullable().optional(), z.number().nullable()),
    startOs: z.record(z.string(), StartOsRecoveryInfo),
    legacyBackup: z.boolean(),
    guid: wire(z.string().nullable().optional(), z.string().nullable()),
    filesystem: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'PartitionInfo',
  })
export const Progress = z
  .union([
    z.null(),
    z.boolean(),
    z.object({
      done: z.number(),
      total: wire(z.number().nullable().optional(), z.number().nullable()),
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
export const SetupProgress = z
  .object({ progress: FullProgress, guid: Guid })
  .meta({
    id: 'SetupProgress',
  })
export const DiskInfo = z
  .object({
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
  })
  .meta({
    id: 'DiskInfo',
  })
export const SetupStatusRes = z
  .union([
    z.object({ status: z.literal('needs-install') }),
    z.intersection(z.object({ status: z.literal('incomplete') }), SetupInfo),
    z.intersection(z.object({ status: z.literal('running') }), SetupProgress),
    z.intersection(z.object({ status: z.literal('complete') }), SetupResult),
  ])
  .meta({
    id: 'SetupStatusRes',
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
        attach: { _PARAMS: AttachParams, _RETURN: SetupProgress },
        cifs: {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
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
          _RETURN: SetupResult,
        },
        disk: {
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
              _RETURN: z.array(DiskInfo),
            },
          },
        },
        execute: { _PARAMS: SetupExecuteParams, _RETURN: SetupProgress },
        exit: { _PARAMS: z.object({}) as z.ZodType<{}, {}>, _RETURN: z.null() },
        'get-pubkey': {
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
          _RETURN: z.record(z.string(), z.unknown()),
        },
        'install-os': { _PARAMS: InstallOsParams, _RETURN: SetupInfo },
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
        restart: {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _RETURN: z.null(),
        },
        'set-keyboard': { _PARAMS: KeyboardOptions, _RETURN: z.null() },
        'set-language': { _PARAMS: SetLanguageParams, _RETURN: z.null() },
        shutdown: {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _RETURN: z.null(),
        },
        status: {
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
          _RETURN: SetupStatusRes,
        },
      },
    },
    ssh: { _PARAMS: z.object({}) as z.ZodType<{}, {}>, _CHILDREN: {} },
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
