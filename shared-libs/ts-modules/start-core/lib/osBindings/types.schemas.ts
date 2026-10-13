import { z } from 'zod'
const wire = <A extends z.ZodType, B extends z.ZodType>(input: A, output: B) =>
  z.codec(input, output, { decode: v => v as never, encode: v => v as never })
export const AcmeProvider = z.string().meta({
  id: 'AcmeProvider',
})
export const AcmeSettings = z.object({ contact: z.array(z.string()) }).meta({
  id: 'AcmeSettings',
})
export const ActionAccess = z.enum(['public', 'dependent', 'user']).meta({
  id: 'ActionAccess',
})
export const ActionId = z.string().meta({
  id: 'ActionId',
})
export const ActionResultV0 = z
  .object({
    message: z.string(),
    value: wire(z.string().nullable().optional(), z.string().nullable()),
    copyable: z.boolean(),
    qr: z.boolean(),
  })
  .meta({
    id: 'ActionResultV0',
  })
export const ActionResultValue = z
  .union([
    z.intersection(
      z.object({ type: z.literal('single') }),
      z.object({
        /**
         * The actual string value to display. The UI renders it as a single-line field —
         * multi-line text belongs in a `multiline` value.
         */
        value: z.string().meta({
          description:
            'The actual string value to display. The UI renders it as a single-line field —\nmulti-line text belongs in a `multiline` value.',
        }),
        /**
         * (optional) Whether or not to include a copy to clipboard icon to copy the value
         */
        copyable: wire(
          z.boolean().nullable().optional(),
          z.boolean().nullable(),
        ).meta({
          description:
            '(optional) Whether or not to include a copy to clipboard icon to copy the value',
        }),
        /**
         * (optional) Whether or not to also display the value as a QR code
         */
        qr: wire(
          z.boolean().nullable().optional(),
          z.boolean().nullable(),
        ).meta({
          description:
            '(optional) Whether or not to also display the value as a QR code',
        }),
        /**
         * (optional) Whether or not to mask the value using ●●●●●●●, which is useful for password or other sensitive information
         */
        masked: wire(
          z.boolean().nullable().optional(),
          z.boolean().nullable(),
        ).meta({
          description:
            '(optional) Whether or not to mask the value using ●●●●●●●, which is useful for password or other sensitive information',
        }),
        /**
         * (optional) Whether or not to include an open in new tab icon to launch the value, which must be an http(s) URL
         */
        launchable: wire(
          z.boolean().nullable().optional(),
          z.boolean().nullable(),
        ).meta({
          description:
            '(optional) Whether or not to include an open in new tab icon to launch the value, which must be an http(s) URL',
        }),
      }),
    ),
    z.intersection(
      z.object({ type: z.literal('multiline') }),
      z.object({
        /**
         * The actual string value to display. The UI renders it verbatim in a read-only monospace field that keeps its line breaks
         */
        value: z.string().meta({
          description:
            'The actual string value to display. The UI renders it verbatim in a read-only monospace field that keeps its line breaks',
        }),
        /**
         * (optional) Whether or not to include a copy to clipboard icon to copy the value
         */
        copyable: wire(
          z.boolean().nullable().optional(),
          z.boolean().nullable(),
        ).meta({
          description:
            '(optional) Whether or not to include a copy to clipboard icon to copy the value',
        }),
        /**
         * (optional) Whether or not to also display the value as a QR code
         */
        qr: wire(
          z.boolean().nullable().optional(),
          z.boolean().nullable(),
        ).meta({
          description:
            '(optional) Whether or not to also display the value as a QR code',
        }),
        /**
         * (optional) Whether or not to blur the value until the user reveals it, which is useful for a private key or other sensitive information
         */
        masked: wire(
          z.boolean().nullable().optional(),
          z.boolean().nullable(),
        ).meta({
          description:
            '(optional) Whether or not to blur the value until the user reveals it, which is useful for a private key or other sensitive information',
        }),
        /**
         * (optional) Also offer the value as a download under this file name, such as "diagnostics.txt"
         */
        filename: wire(
          z.string().nullable().optional(),
          z.string().nullable(),
        ).meta({
          description:
            '(optional) Also offer the value as a download under this file name, such as "diagnostics.txt"',
        }),
      }),
    ),
    z.intersection(
      z.object({ type: z.literal('group') }),
      z.object({
        /**
         * An new group of nested values, experienced by the user as an accordion dropdown
         */
        get value(): z.ZodArray<typeof ActionResultMember> {
          return z.array(ActionResultMember).meta({
            description:
              'An new group of nested values, experienced by the user as an accordion dropdown',
          })
        },
      }),
    ),
  ])
  .meta({
    id: 'ActionResultValue',
  })
export const ActionVisibility = z
  .union([
    z.literal('hidden'),
    z.object({ disabled: z.string() }),
    z.literal('enabled'),
  ])
  .meta({
    id: 'ActionVisibility',
  })
export const AddVersionParams = z
  .object({
    version: z.string(),
    headline: z.string(),
    releaseNotes: z.string(),
    sourceVersion: z.string(),
  })
  .meta({
    id: 'AddVersionParams',
  })
export const Algorithm = z.enum(['ecdsa', 'ed25519']).meta({
  id: 'Algorithm',
})
export const AllowedStatuses = z
  .enum(['only-running', 'only-stopped', 'any'])
  .meta({
    id: 'AllowedStatuses',
  })
export const AnySignature = z.string().meta({
  id: 'AnySignature',
})
export const AnySigningKey = z.string().meta({
  id: 'AnySigningKey',
})
export const AnyVerifyingKey = z.string().meta({
  id: 'AnyVerifyingKey',
})
export const ApiState = z.enum(['error', 'initializing', 'running']).meta({
  id: 'ApiState',
})
export const BackupTargetId = z.string().meta({
  id: 'BackupTargetId',
})
export const Base64 = z.string().meta({
  id: 'Base64',
})
export const BasicCredential = z
  .object({ username: z.string(), password: z.string() })
  .meta({
    id: 'BasicCredential',
  })
export const Blake3Commitment = z
  .object({ hash: z.string(), size: z.number() })
  .meta({
    id: 'Blake3Commitment',
  })
export const BlockDev = z.object({ logicalname: z.string() }).meta({
  id: 'BlockDev',
})
export const BuildArg = z
  .union([z.string(), z.object({ env: z.string() })])
  .meta({
    id: 'BuildArg',
  })
export const CallbackId = z.number().meta({
  id: 'CallbackId',
})
export const CapabilityVerdict = z
  .object({
    /**
     * `None` = never probed.
     */
    supported: wire(
      z.boolean().nullable().optional(),
      z.boolean().nullable(),
    ).meta({
      description: '`None` = never probed.',
    }),
    at: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'CapabilityVerdict',
  })
export const Celsius = z.object({ value: z.string(), unit: z.string() }).meta({
  id: 'Celsius',
})
export const CheckPortRes = z
  .object({
    ip: z.string(),
    port: z.int(),
    openExternally: z.boolean(),
    openInternally: z.boolean(),
    hairpinning: z.boolean(),
  })
  .meta({
    id: 'CheckPortRes',
  })

/**
 * v6 reachability of the box's GUA at a port. v6 is NAT-free (the GUA is the
 * box's own address), so there is no hairpinning. Queried separately from
 * [`CheckPortRes`] so the IPv4 and IPv6 checks can run independently.
 */
export const CheckPortV6Res = z
  .object({
    ip: z.string(),
    openExternally: z.boolean(),
    openInternally: z.boolean(),
  })
  .meta({
    id: 'CheckPortV6Res',
    description:
      "v6 reachability of the box's GUA at a port. v6 is NAT-free (the GUA is the\nbox's own address), so there is no hairpinning. Queried separately from\n[`CheckPortRes`] so the IPv4 and IPv6 checks can run independently.",
  })
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
export const CifsAddParams = z
  .object({
    hostname: z.string(),
    path: z.string(),
    username: z.string(),
    password: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'CifsAddParams',
  })
export const ClearCallbacksParams = z
  .union([
    z.object({ only: z.array(z.number()) }),
    z.object({ except: z.array(z.number()) }),
  ])
  .meta({
    id: 'ClearCallbacksParams',
  })
export const ClearTasksParams = z
  .union([
    z.object({ only: z.array(z.string()) }),
    z.object({ except: z.array(z.string()) }),
  ])
  .meta({
    id: 'ClearTasksParams',
  })
export const CliSetIconParams = z
  .object({
    icon: wire(z.string().nullable().optional(), z.string().nullable()),
    clear: z.boolean(),
  })
  .meta({
    id: 'CliSetIconParams',
  })
export const ContactInfo = z
  .union([
    z.object({ email: z.string() }),
    z.object({ matrix: z.string() }),
    z.object({ website: z.string() }),
  ])
  .meta({
    id: 'ContactInfo',
  })
export const CountEntry = z.object({ label: z.string(), count: z.int() }).meta({
  id: 'CountEntry',
})
export const CurrentDependencyKind = z
  .union([
    z.object({ kind: z.literal('exists') }),
    z.intersection(
      z.object({ kind: z.literal('running') }),
      z.object({
        healthChecks: wire(z.array(z.string()).optional(), z.array(z.string())),
      }),
    ),
  ])
  .meta({
    id: 'CurrentDependencyKind',
  })
export const DataUrl = z.string().meta({
  id: 'DataUrl',
})
export const DeviceFilter = z
  .object({
    description: z.string(),
    class: z.enum(['processor', 'display']),
    product: wire(z.string().nullable().optional(), z.string().nullable()),
    vendor: wire(z.string().nullable().optional(), z.string().nullable()),
    capabilities: wire(
      z.array(z.string()).nullable().optional(),
      z.array(z.string()).nullable(),
    ),
    driver: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'DeviceFilter',
  })
export const DnsSettings = z
  .object({
    dhcpServers: z.array(z.string()),
    staticServers: wire(
      z.array(z.string()).nullable().optional(),
      z.array(z.string()).nullable(),
    ),
  })
  .meta({
    id: 'DnsSettings',
  })
export const Duration = z.string().meta({
  id: 'Duration',
})
export const EchoParams = z.object({ message: z.string() }).meta({
  id: 'EchoParams',
})
export const EncryptedWire = z.object({ encrypted: z.unknown() }).meta({
  id: 'EncryptedWire',
})

/**
 * Selects how aggressively an EPP-capable CPU pursues performance.
 */
export const Epp = z.string().meta({
  id: 'Epp',
  description:
    'Selects how aggressively an EPP-capable CPU pursues performance.',
})
export const ErrorData = z
  .object({
    details: z.string(),
    debug: z.string(),
    info: wire(z.unknown().optional(), z.unknown()),
  })
  .meta({
    id: 'ErrorData',
  })
export const FileType = z.enum(['file', 'directory', 'infer']).meta({
  id: 'FileType',
})
export const GatewayId = z.string().meta({
  id: 'GatewayId',
})
export const GatewayType = z.enum(['inbound-outbound', 'outbound-only']).meta({
  id: 'GatewayType',
})
export const GenerateCertificateParams = z
  .object({
    hostnames: z.array(z.string()),
    ed25519: wire(z.boolean().optional(), z.boolean()),
  })
  .meta({
    id: 'GenerateCertificateParams',
  })
export const GenerateCertificateResponse = z
  .object({ key: z.string(), fullchain: z.string() })
  .meta({
    id: 'GenerateCertificateResponse',
  })
export const GetDownloadsParams = z
  .object({
    /**
     * Filter by package ID
     */
    pkgId: wire(z.string().nullable().optional(), z.string().nullable()).meta({
      description: 'Filter by package ID',
    }),
    /**
     * Filter by version
     */
    version: wire(z.string().nullable().optional(), z.string().nullable()).meta(
      {
        description: 'Filter by version',
      },
    ),
    /**
     * Start of time range (RFC 3339)
     */
    after: wire(z.string().nullable().optional(), z.string().nullable()).meta({
      description: 'Start of time range (RFC 3339)',
    }),
    /**
     * End of time range (RFC 3339)
     */
    before: wire(z.string().nullable().optional(), z.string().nullable()).meta({
      description: 'End of time range (RFC 3339)',
    }),
  })
  .meta({
    id: 'GetDownloadsParams',
  })
export const GetOsAssetParams = z
  .object({ version: z.string(), platform: z.string() })
  .meta({
    id: 'GetOsAssetParams',
  })
export const GetOsVersionParams = z
  .object({
    sourceVersion: wire(
      z.string().nullable().optional(),
      z.string().nullable(),
    ),
    targetVersion: wire(
      z.string().nullable().optional(),
      z.string().nullable(),
    ),
    serverId: wire(z.string().nullable().optional(), z.string().nullable()),
    platform: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'GetOsVersionParams',
  })
export const GetUsersParams = z
  .object({
    /**
     * Start of time range (RFC 3339)
     */
    after: wire(z.string().nullable().optional(), z.string().nullable()).meta({
      description: 'Start of time range (RFC 3339)',
    }),
    /**
     * End of time range (RFC 3339)
     */
    before: wire(z.string().nullable().optional(), z.string().nullable()).meta({
      description: 'End of time range (RFC 3339)',
    }),
  })
  .meta({
    id: 'GetUsersParams',
  })
export const GigaBytes = z
  .object({ value: z.string(), unit: z.string() })
  .meta({
    id: 'GigaBytes',
  })
export const GitHash = z.string().meta({
  id: 'GitHash',
})
export const Governor = z.string().meta({
  id: 'Governor',
})
export const Guid = z.string().meta({
  id: 'Guid',
})
export const HealthCheckId = z.string().meta({
  id: 'HealthCheckId',
})
export const HostId = z.string().meta({
  id: 'HostId',
})
export const IdMap = z
  .object({ fromId: z.int(), toId: z.int(), range: z.int() })
  .meta({
    id: 'IdMap',
  })
export const ImageId = z.string().meta({
  id: 'ImageId',
})
export const ImageMetadata = z
  .object({
    workdir: z.string(),
    user: z.string(),
    entrypoint: wire(
      z.array(z.string()).nullable().optional(),
      z.array(z.string()).nullable(),
    ),
    cmd: wire(
      z.array(z.string()).nullable().optional(),
      z.array(z.string()).nullable(),
    ),
  })
  .meta({
    id: 'ImageMetadata',
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
export const KillParams = z.object({ ids: z.array(z.string()) }).meta({
  id: 'KillParams',
})
export const ListNotificationParams = z
  .object({
    before: wire(z.number().nullable().optional(), z.number().nullable()),
    limit: wire(z.number().nullable().optional(), z.number().nullable()),
  })
  .meta({
    id: 'ListNotificationParams',
  })
export const ListVersionSignersParams = z.object({ version: z.string() }).meta({
  id: 'ListVersionSignersParams',
})
export const LocaleString = z
  .union([z.string(), z.record(z.string(), z.string())])
  .meta({
    id: 'LocaleString',
  })
export const LogEntry = z
  .object({ timestamp: z.string(), message: z.string(), bootId: z.string() })
  .meta({
    id: 'LogEntry',
  })
export const LogsParams = z
  .intersection(
    z.object({
      limit: wire(z.int().nullable().optional(), z.int().nullable()),
      cursor: wire(z.string().nullable().optional(), z.string().nullable()),
      boot: wire(
        z.union([z.number(), z.string()]).nullable().optional(),
        z.union([z.number(), z.string()]).nullable(),
      ),
      before: wire(z.boolean().optional(), z.boolean()),
    }),
    z.object({}) as z.ZodType<{}, {}>,
  )
  .meta({
    id: 'LogsParams',
  })
export const LshwDisplay = z
  .object({
    product: wire(z.string().nullable().optional(), z.string().nullable()),
    vendor: wire(z.string().nullable().optional(), z.string().nullable()),
    capabilities: z.array(z.string()),
    driver: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'LshwDisplay',
  })
export const LshwProcessor = z
  .object({
    product: wire(z.string().nullable().optional(), z.string().nullable()),
    vendor: wire(z.string().nullable().optional(), z.string().nullable()),
    capabilities: z.array(z.string()),
  })
  .meta({
    id: 'LshwProcessor',
  })
export const MaybeUtf8String = z.union([z.string(), z.array(z.number())]).meta({
  id: 'MaybeUtf8String',
})
export const MebiBytes = z
  .object({ value: z.string(), unit: z.string() })
  .meta({
    id: 'MebiBytes',
  })
export const MerkleArchiveCommitment = z
  .object({ rootSighash: z.string(), rootMaxsize: z.number() })
  .meta({
    id: 'MerkleArchiveCommitment',
  })
export const ModifyNotificationBeforeParams = z
  .object({ before: z.number() })
  .meta({
    id: 'ModifyNotificationBeforeParams',
  })
export const ModifyNotificationParams = z
  .object({ ids: z.array(z.number()) })
  .meta({
    id: 'ModifyNotificationParams',
  })
export const NamedHealthCheckResultKind = z
  .union([
    z.intersection(
      z.object({ result: z.literal('success') }),
      z.object({
        message: wire(z.string().nullable().optional(), z.string().nullable()),
      }),
    ),
    z.intersection(
      z.object({ result: z.literal('disabled') }),
      z.object({
        message: wire(z.string().nullable().optional(), z.string().nullable()),
      }),
    ),
    z.intersection(
      z.object({ result: z.literal('starting') }),
      z.object({
        message: wire(z.string().nullable().optional(), z.string().nullable()),
      }),
    ),
    z.intersection(
      z.object({ result: z.literal('waiting') }),
      z.object({
        message: wire(z.string().nullable().optional(), z.string().nullable()),
      }),
    ),
    z.intersection(
      z.object({ result: z.literal('loading') }),
      z.object({ message: z.string() }),
    ),
    z.intersection(
      z.object({ result: z.literal('failure') }),
      z.object({ message: z.string() }),
    ),
  ])
  .meta({
    id: 'NamedHealthCheckResultKind',
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
export const NetInfo = z
  .object({
    assignedPort: wire(z.int().nullable().optional(), z.int().nullable()),
    assignedSslPort: wire(z.int().nullable().optional(), z.int().nullable()),
  })
  .meta({
    id: 'NetInfo',
  })
export const NetworkInterfaceType = z
  .enum(['ethernet', 'wireless', 'bridge', 'wireguard', 'loopback'])
  .meta({
    id: 'NetworkInterfaceType',
  })
export const NotificationLevel = z
  .enum(['success', 'info', 'warning', 'error'])
  .meta({
    id: 'NotificationLevel',
  })
export const PackageBackupReport = z
  .object({
    error: wire(z.string().nullable().optional(), z.string().nullable()),
    duration_ms: z.number(),
  })
  .meta({
    id: 'PackageBackupReport',
  })
export const PackageDetailLevel = z.enum(['none', 'short', 'full']).meta({
  id: 'PackageDetailLevel',
})
export const PackageId = z.string().meta({
  id: 'PackageId',
})
export const PackageVersionCount = z
  .object({ pkgId: z.string(), version: z.string(), count: z.int() })
  .meta({
    id: 'PackageVersionCount',
  })
export const PassthroughInfo = z
  .object({
    hostname: z.string(),
    listenPort: z.int(),
    backend: z.string(),
    publicGateways: z.array(z.string()),
    privateIps: z.array(z.string()),
  })
  .meta({
    id: 'PassthroughInfo',
  })
export const PathOrUrl = z.string().meta({
  id: 'PathOrUrl',
})
export const Pem = z.string().meta({
  id: 'Pem',
})
export const Percentage = z
  .object({ value: z.string(), unit: z.string() })
  .meta({
    id: 'Percentage',
  })
export const PluginId = z.enum(['url-v0']).meta({
  id: 'PluginId',
})
export const PreDownloadAlertWhen = z
  .object({ sourceVersion: z.string() })
  .meta({
    id: 'PreDownloadAlertWhen',
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

/**
 * A registry asset with a caller-selected commitment representation.
 */
export const RegistryAsset = <Commitment extends z.ZodType>(
  Commitment: Commitment,
) => {
  const self = z.object({
    publishedAt: z.string(),
    urls: z.array(z.string()),
    commitment: Commitment,
    signatures: z.record(z.string(), AnySignature),
  })
  return self
}
export const RemoveAssetParams = z
  .object({ version: z.string(), platform: z.string() })
  .meta({
    id: 'RemoveAssetParams',
  })
export const RemoveCategoryParams = z.object({ id: z.string() }).meta({
  id: 'RemoveCategoryParams',
})
export const RemoveDomainParams = z.object({ fqdn: z.string() }).meta({
  id: 'RemoveDomainParams',
})
export const RemoveVersionParams = z.object({ version: z.string() }).meta({
  id: 'RemoveVersionParams',
})
export const ReplayId = z.string().meta({
  id: 'ReplayId',
})
export const RequestCommitment = z
  .object({
    timestamp: z.number(),
    nonce: z.number(),
    size: z.number(),
    blake3: z.string(),
  })
  .meta({
    id: 'RequestCommitment',
  })
export const RestartReason = z
  .enum(['mdns', 'language', 'kiosk', 'update'])
  .meta({
    id: 'RestartReason',
  })
export const Security = z.object({ ssl: z.boolean() }).meta({
  id: 'Security',
})
export const ServerBackupReport = z
  .object({
    attempted: z.boolean(),
    error: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'ServerBackupReport',
  })
export const ServerHostname = z.string().meta({
  id: 'ServerHostname',
})
export const ServerSpecs = z
  .object({ cpu: z.string(), disk: z.string(), memory: z.string() })
  .meta({
    id: 'ServerSpecs',
  })
export const ServiceInterfaceId = z.string().meta({
  id: 'ServiceInterfaceId',
})
export const ServiceInterfaceType = z.enum(['ui', 'p2p', 'api']).meta({
  id: 'ServiceInterfaceType',
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
export const SetCountryParams = z.object({ country: z.string() }).meta({
  id: 'SetCountryParams',
})
export const SetDataVersionParams = z
  .object({
    version: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'SetDataVersionParams',
  })
export const SetLanguageParams = z.object({ language: z.string() }).meta({
  id: 'SetLanguageParams',
})
export const SetMainStatusStatus = z.enum(['running', 'stopped']).meta({
  id: 'SetMainStatusStatus',
})
export const SetNameParams = z.object({ name: z.string() }).meta({
  id: 'SetNameParams',
})
export const SetServerHostnameParams = z
  .object({
    /**
     * The server's `.local` hostname: up to 32 lowercase letters, numbers, and
     * hyphens, not starting or ending with a hyphen
     */
    hostname: z.string().meta({
      description:
        "The server's `.local` hostname: up to 32 lowercase letters, numbers, and\nhyphens, not starting or ending with a hyphen",
    }),
  })
  .meta({
    id: 'SetServerHostnameParams',
  })
export const SetStaticDnsParams = z
  .object({
    servers: wire(
      z.array(z.string()).nullable().optional(),
      z.array(z.string()).nullable(),
    ),
  })
  .meta({
    id: 'SetStaticDnsParams',
  })
export const SetWifiEnabledParams = z.object({ enabled: z.boolean() }).meta({
  id: 'SetWifiEnabledParams',
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
export const ShutdownParams = z
  .object({
    /**
     * Block until graceful teardown completes (default over the CLI; the
     * frontend omits this and gets an immediate reply). Cleared with
     * `--nowait`. The wait can't outlive the webserver teardown that follows
     * container shutdown, so the connection drops once services are stopped.
     */
    wait: wire(z.boolean().optional(), z.boolean()).meta({
      description:
        "Block until graceful teardown completes (default over the CLI; the\nfrontend omits this and gets an immediate reply). Cleared with\n`--nowait`. The wait can't outlive the webserver teardown that follows\ncontainer shutdown, so the connection drops once services are stopped.",
    }),
  })
  .meta({
    id: 'ShutdownParams',
  })
export const SideloadParams = (z.object({}) as z.ZodType<{}, {}>).meta({
  id: 'SideloadParams',
})

/**
 * So a signal strength is a number between 0-100, I want the null option to be 0 since there is no signal
 */
export const SignalStrength = z.int().meta({
  id: 'SignalStrength',
  description:
    'So a signal strength is a number between 0-100, I want the null option to be 0 since there is no signal',
})
export const SmtpSecurity = z.enum(['starttls', 'tls']).meta({
  id: 'SmtpSecurity',
})
export const SshDeleteParams = z.object({ fingerprint: z.string() }).meta({
  id: 'SshDeleteParams',
})
export const SshKeyResponse = z
  .object({
    alg: z.string(),
    fingerprint: z.string(),
    hostname: z.string(),
    createdAt: z.string(),
  })
  .meta({
    id: 'SshKeyResponse',
  })
export const SshPubKey = z.string().meta({
  id: 'SshPubKey',
})

/**
 * Ssid are the names of the wifis, usually human readable.
 */
export const Ssid = z.string().meta({
  id: 'Ssid',
  description: 'Ssid are the names of the wifis, usually human readable.',
})
export const StartStop = z.enum(['start', 'stop']).meta({
  id: 'StartStop',
})
export const TaskCondition = z.enum(['input-not-matches']).meta({
  id: 'TaskCondition',
})
export const TaskInputRepr = z
  .intersection(
    z.object({ kind: z.literal('partial') }),
    z.object({
      accept: wire(
        z.array(z.unknown()).nullable().optional(),
        z.array(z.unknown()).nullable(),
      ),
      set: wire(z.unknown().nullable().optional(), z.unknown().nullable()),
      value: wire(z.unknown().nullable().optional(), z.unknown().nullable()),
    }),
  )
  .meta({
    id: 'TaskInputRepr',
  })
export const TaskSeverity = z.enum(['optional', 'important', 'critical']).meta({
  id: 'TaskSeverity',
})
export const TimeInfo = z.object({ now: z.string(), uptime: z.int() }).meta({
  id: 'TimeInfo',
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

/**
 * How the OS reverse proxy validates the container's TLS certificate when it
 * rewraps SSL (`add_ssl` set AND `secure.ssl == true`, so the OS terminates
 * the client's TLS and initiates a fresh TLS connection to the container).
 * Absent (`None`) means validate against the StartOS root CA — the default.
 */
export const UpstreamCertValidation = z
  .union([
    z.literal('disable').meta({
      description:
        "Do not validate the container's certificate at all. Use when the\ncontainer serves a self-signed cert on the trusted internal bridge.",
    }),
    z
      .object({
        /**
         * Validate against this PEM-encoded certificate (or chain) instead of the
         * StartOS root CA.
         */
        certificate: z.string().meta({
          description:
            'Validate against this PEM-encoded certificate (or chain) instead of the\nStartOS root CA.',
        }),
      })
      .meta({
        description:
          'Validate against this PEM-encoded certificate (or chain) instead of the\nStartOS root CA.',
      }),
  ])
  .meta({
    id: 'UpstreamCertValidation',
    description:
      "How the OS reverse proxy validates the container's TLS certificate when it\nrewraps SSL (`add_ssl` set AND `secure.ssl == true`, so the OS terminates\nthe client's TLS and initiates a fresh TLS connection to the container).\nAbsent (`None`) means validate against the StartOS root CA — the default.",
  })
export const UsersResponse = z
  .object({ uniqueServers: z.int(), totalCheckins: z.int() })
  .meta({
    id: 'UsersResponse',
  })
export const Version = z.string().meta({
  id: 'Version',
})
export const VolumeId = z.string().meta({
  id: 'VolumeId',
})
export const WifiAddParams = z
  .object({ ssid: z.string(), password: z.string() })
  .meta({
    id: 'WifiAddParams',
  })
export const WifiSsidParams = z.object({ ssid: z.string() }).meta({
  id: 'WifiSsidParams',
})
export const AcceptSigners = z
  .union([
    z.object({ signer: AnyVerifyingKey }),
    z.object({
      get any(): z.ZodArray<typeof AcceptSigners> {
        return z.array(AcceptSigners)
      },
    }),
    z.object({
      get all(): z.ZodArray<typeof AcceptSigners> {
        return z.array(AcceptSigners)
      },
    }),
  ])
  .meta({
    id: 'AcceptSigners',
  })
export const ActionInput = z
  .object({
    eventId: wire(Guid.optional(), Guid),
    spec: z.record(z.string(), z.unknown()),
    value: wire(
      z.record(z.string(), z.unknown()).nullable().optional(),
      z.record(z.string(), z.unknown()).nullable(),
    ),
  })
  .meta({
    id: 'ActionInput',
  })
export const ActionMetadata = z
  .object({
    /**
     * A human-readable name
     */
    name: z.string().meta({
      description: 'A human-readable name',
    }),
    /**
     * A detailed description of what the action will do
     */
    description: z.string().meta({
      description: 'A detailed description of what the action will do',
    }),
    /**
     * Presents as an alert prior to executing the action. Should be used sparingly but important if the action could have harmful, unintended consequences
     */
    warning: wire(z.string().nullable().optional(), z.string().nullable()).meta(
      {
        description:
          'Presents as an alert prior to executing the action. Should be used sparingly but important if the action could have harmful, unintended consequences',
      },
    ),
    /**
     * One of: "enabled", "hidden", or { disabled: "" }
     *   - "enabled" - the action is available be run
     *   - "hidden" - the action cannot be seen or run
     *   - { disabled: "example explanation" } means the action is visible but cannot be run. Replace "example explanation" with a reason why the action is disable to prevent user confusion.
     */
    visibility: wire(ActionVisibility.optional(), ActionVisibility).meta({
      description:
        'One of: "enabled", "hidden", or { disabled: "" }\n  - "enabled" - the action is available be run\n  - "hidden" - the action cannot be seen or run\n  - { disabled: "example explanation" } means the action is visible but cannot be run. Replace "example explanation" with a reason why the action is disable to prevent user confusion.',
    }),
    /**
     * One of: "only-stopped", "only-running", "all"
     *   - "only-stopped" - the action can only be run when the service is stopped
     *   - "only-running" - the action can only be run when the service is running
     *   - "any" - the action can only be run regardless of the service's status
     */
    allowedStatuses: AllowedStatuses.meta({
      description:
        'One of: "only-stopped", "only-running", "all"\n  - "only-stopped" - the action can only be run when the service is stopped\n  - "only-running" - the action can only be run when the service is running\n  - "any" - the action can only be run regardless of the service\'s status',
    }),
    hasInput: z.boolean(),
    /**
     * If provided, this action will be nested under a header of this value, along with other actions of the same group
     */
    group: wire(z.string().nullable().optional(), z.string().nullable()).meta({
      description:
        'If provided, this action will be nested under a header of this value, along with other actions of the same group',
    }),
    /**
     * Who is allowed to invoke this action directly via `effects.action.run`.
     *   - "public" — any installed package
     *   - "dependent" — only services that declare this package as a current dependency
     *   - "user" — only the user (other services must create a task). Default when omitted.
     * Services that lack direct access can always queue a task with `effects.action.createTask`.
     */
    access: wire(
      ActionAccess.nullable().optional(),
      ActionAccess.nullable(),
    ).meta({
      description:
        'Who is allowed to invoke this action directly via `effects.action.run`.\n  - "public" — any installed package\n  - "dependent" — only services that declare this package as a current dependency\n  - "user" — only the user (other services must create a task). Default when omitted.\nServices that lack direct access can always queue a task with `effects.action.createTask`.',
    }),
  })
  .meta({
    id: 'ActionMetadata',
  })
export const ActionResultMember = z
  .intersection(
    z.object({
      /**
       * A human-readable name or title of the value, such as "Last Active" or "Login Password"
       */
      name: z.string().meta({
        description:
          'A human-readable name or title of the value, such as "Last Active" or "Login Password"',
      }),
      /**
       * (optional) A description of the value, such as an explaining why it exists or how to use it
       */
      description: wire(
        z.string().nullable().optional(),
        z.string().nullable(),
      ).meta({
        description:
          '(optional) A description of the value, such as an explaining why it exists or how to use it',
      }),
    }),
    ActionResultValue,
  )
  .meta({
    id: 'ActionResultMember',
  })
export const ActionResultV1 = z
  .object({
    /**
     * Primary text to display as the header of the response modal. e.g. "Success!", "Name Updated", or "Service Information", whatever makes sense
     */
    title: z.string().meta({
      description:
        'Primary text to display as the header of the response modal. e.g. "Success!", "Name Updated", or "Service Information", whatever makes sense',
    }),
    /**
     * (optional) A general message for the user, just under the title. Rendered as Markdown, with a single line break kept as a line break.
     */
    message: wire(z.string().nullable().optional(), z.string().nullable()).meta(
      {
        description:
          '(optional) A general message for the user, just under the title. Rendered as Markdown, with a single line break kept as a line break.',
      },
    ),
    /**
     * (optional) Structured data to present inside the modal
     */
    result: wire(
      ActionResultValue.nullable().optional(),
      ActionResultValue.nullable(),
    ).meta({
      description: '(optional) Structured data to present inside the modal',
    }),
  })
  .meta({
    id: 'ActionResultV1',
  })
export const AddAdminParams = z.object({ signer: Guid }).meta({
  id: 'AddAdminParams',
})
export const AddAssetParams = z
  .object({
    version: z.string(),
    platform: z.string(),
    url: z.string(),
    signature: AnySignature,
    commitment: Blake3Commitment,
  })
  .meta({
    id: 'AddAssetParams',
  })
export const AddCategoryParams = z
  .object({ id: z.string(), name: LocaleString })
  .meta({
    id: 'AddCategoryParams',
  })
export const AddMirrorParams = z
  .object({
    url: z.string(),
    commitment: MerkleArchiveCommitment,
    signature: AnySignature,
  })
  .meta({
    id: 'AddMirrorParams',
  })
export const AddPackageParams = z
  .object({
    urls: z.array(z.string()),
    commitment: MerkleArchiveCommitment,
    signature: AnySignature,
  })
  .meta({
    id: 'AddPackageParams',
  })
export const AddPackageSignerParams = z
  .object({
    id: PackageId,
    signer: Guid,
    versions: wire(z.string().nullable().optional(), z.string().nullable()),
    merge: z.boolean(),
  })
  .meta({
    id: 'AddPackageSignerParams',
  })
export const AddPackageToCategoryParams = z
  .object({ id: z.string(), package: PackageId })
  .meta({
    id: 'AddPackageToCategoryParams',
  })
export const AddPrivateDomainParams = z
  .object({ fqdn: z.string(), gateway: GatewayId })
  .meta({
    id: 'AddPrivateDomainParams',
  })
export const AddPublicDomainParams = z
  .object({
    fqdn: z.string(),
    acme: wire(AcmeProvider.nullable().optional(), AcmeProvider.nullable()),
    gateway: GatewayId,
    internalPort: z.int(),
  })
  .meta({
    id: 'AddPublicDomainParams',
  })
export const AddTunnelParams = z
  .object({
    name: z.string(),
    config: z.string(),
    type: wire(GatewayType.nullable().optional(), GatewayType.nullable()),
    setAsDefaultOutbound: z.boolean(),
  })
  .meta({
    id: 'AddTunnelParams',
  })
export const AddressInfo = z
  .object({
    username: wire(z.string().nullable().optional(), z.string().nullable()),
    hostId: HostId,
    internalPort: z.int(),
    scheme: wire(z.string().nullable().optional(), z.string().nullable()),
    sslScheme: wire(z.string().nullable().optional(), z.string().nullable()),
    suffix: z.string(),
  })
  .meta({
    id: 'AddressInfo',
  })

/**
 * The protocols a binding answers with, carried on the wire as the list itself.
 */
export const AlpnInfo = z.array(MaybeUtf8String).meta({
  id: 'AlpnInfo',
  description:
    'The protocols a binding answers with, carried on the wire as the list itself.',
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
export const BackupReport = z
  .object({
    server: ServerBackupReport,
    packages: z.record(z.string(), PackageBackupReport),
  })
  .meta({
    id: 'BackupReport',
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
export const BindId = z.object({ id: HostId, internalPort: z.int() }).meta({
  id: 'BindId',
})
export const BindRangeParams = z
  .object({
    id: HostId,
    internalStartPort: z.int(),
    externalStartPort: z.int(),
    numberOfPorts: z.int(),
  })
  .meta({
    id: 'BindRangeParams',
  })
export const CancelInstallParams = z.object({ id: PackageId }).meta({
  id: 'CancelInstallParams',
})
export const Category = z.object({ name: LocaleString }).meta({
  id: 'Category',
})
export const CheckChallengeParams = z
  .object({ fqdn: z.string(), gateway: GatewayId, acme: AcmeProvider })
  .meta({
    id: 'CheckChallengeParams',
  })

/**
 * Reachability of the port a certificate authority validates on. A null leg
 * went unprobed.
 */
export const CheckChallengeRes = z
  .object({
    port: wire(CheckPortRes.nullable().optional(), CheckPortRes.nullable()),
    portV6: wire(
      CheckPortV6Res.nullable().optional(),
      CheckPortV6Res.nullable(),
    ),
  })
  .meta({
    id: 'CheckChallengeRes',
    description:
      'Reachability of the port a certificate authority validates on. A null leg\nwent unprobed.',
  })
export const CheckDependenciesParam = z
  .object({
    packageIds: wire(
      z.array(PackageId).nullable().optional(),
      z.array(PackageId).nullable(),
    ),
  })
  .meta({
    id: 'CheckDependenciesParam',
  })
export const CheckDnsParams = z
  .object({ gateway: GatewayId, fqdn: z.string() })
  .meta({
    id: 'CheckDnsParams',
  })
export const CheckPortParams = z
  .object({ port: z.int(), gateway: GatewayId })
  .meta({
    id: 'CheckPortParams',
  })
export const CifsRemoveParams = z.object({ id: BackupTargetId }).meta({
  id: 'CifsRemoveParams',
})
export const CifsUpdateParams = z
  .object({
    id: BackupTargetId,
    hostname: z.string(),
    path: z.string(),
    username: z.string(),
    password: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'CifsUpdateParams',
  })
export const ClearActionsParams = z.object({ except: z.array(ActionId) }).meta({
  id: 'ClearActionsParams',
})
export const ClearServiceInterfacesParams = z
  .object({ except: z.array(ServiceInterfaceId) })
  .meta({
    id: 'ClearServiceInterfacesParams',
  })
export const ClearTaskParams = z
  .object({
    packageId: PackageId,
    replayId: ReplayId,
    force: wire(z.boolean().optional(), z.boolean()),
  })
  .meta({
    id: 'ClearTaskParams',
  })
export const CliSetDescriptionParams = z
  .object({
    description: wire(
      LocaleString.nullable().optional(),
      LocaleString.nullable(),
    ),
    clear: z.boolean(),
  })
  .meta({
    id: 'CliSetDescriptionParams',
  })
export const ControlParams = z.object({ id: PackageId }).meta({
  id: 'ControlParams',
})
export const CreateNotificationParams = z
  .object({
    level: NotificationLevel,
    title: z.string(),
    message: z.string(),
    /**
     * Optional long-form, markdown-formatted body that the UI renders in a
     * "View Details" modal when present (release notes, post-update
     * changelogs, structured error reports). When omitted, the notification
     * has no extra payload.
     */
    data: wire(z.string().nullable().optional(), z.string().nullable()).meta({
      description:
        'Optional long-form, markdown-formatted body that the UI renders in a\n"View Details" modal when present (release notes, post-update\nchangelogs, structured error reports). When omitted, the notification\nhas no extra payload.',
    }),
  })
  .meta({
    id: 'CreateNotificationParams',
  })
export const CreateSubcontainerFsParams = z
  .object({
    imageId: ImageId,
    name: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'CreateSubcontainerFsParams',
  })
export const CurrentDependencyInfo = z
  .intersection(
    z.object({
      title: wire(LocaleString.nullable().optional(), LocaleString.nullable()),
      versionRange: z.string(),
    }),
    CurrentDependencyKind,
  )
  .meta({
    id: 'CurrentDependencyInfo',
  })
export const DeleteLegacyParams = z.object({ targetId: BackupTargetId }).meta({
  id: 'DeleteLegacyParams',
})
export const DependencyMetadata = z
  .intersection(
    z.object({
      title: wire(LocaleString.nullable().optional(), LocaleString.nullable()),
      icon: DataUrl.nullable().optional(),
      description: wire(
        LocaleString.nullable().optional(),
        LocaleString.nullable(),
      ),
      optional: z.boolean(),
      versionRange: wire(
        z.string().nullable().optional(),
        z.string().nullable(),
      ),
    }),
    z.union([
      CurrentDependencyKind.nullable(),
      z.object({}) as z.ZodType<{}, {}>,
    ]),
  )
  .meta({
    id: 'DependencyMetadata',
  })
export const DependencyRequirement = z
  .union([
    z.intersection(
      z.object({ kind: z.literal('running') }),
      z.object({
        id: PackageId,
        healthChecks: z.array(HealthCheckId),
        versionRange: z.string(),
      }),
    ),
    z.intersection(
      z.object({ kind: z.literal('exists') }),
      z.object({ id: PackageId, versionRange: z.string() }),
    ),
  ])
  .meta({
    id: 'DependencyRequirement',
  })
export const Description = z
  .object({ short: LocaleString, long: LocaleString })
  .meta({
    id: 'Description',
  })
export const DesiredStatus = z
  .union([
    z.object({ main: z.literal('stopped') }),
    z.intersection(
      z.object({ main: z.literal('restarting') }),
      z.object({ restartAgain: wire(z.boolean().optional(), z.boolean()) }),
    ),
    z.object({ main: z.literal('running') }),
    z.intersection(
      z.object({ main: z.literal('backing-up') }),
      z.object({ onComplete: StartStop }),
    ),
    z.intersection(
      z.object({ main: z.literal('updating') }),
      z.object({ onComplete: StartStop }),
    ),
  ])
  .meta({
    id: 'DesiredStatus',
  })
export const DestroySubcontainerFsParams = z.object({ guid: Guid }).meta({
  id: 'DestroySubcontainerFsParams',
})
export const DomainSettings = z.object({ gateway: GatewayId }).meta({
  id: 'DomainSettings',
})
export const DownloadsResponse = z
  .object({
    totalRequests: z.int(),
    byPackage: z.array(CountEntry),
    byPackageVersion: z.array(PackageVersionCount),
  })
  .meta({
    id: 'DownloadsResponse',
  })
export const EditSignerParams = z
  .object({
    id: Guid,
    setName: wire(z.string().nullable().optional(), z.string().nullable()),
    addContact: z.array(ContactInfo),
    addKey: z.array(AnyVerifyingKey),
    removeContact: z.array(ContactInfo),
    removeKey: z.array(AnyVerifyingKey),
  })
  .meta({
    id: 'EditSignerParams',
  })
export const EffectsRunActionParams = z
  .object({
    packageId: wire(PackageId.nullable().optional(), PackageId.nullable()),
    actionId: ActionId,
    input: z.unknown(),
  })
  .meta({
    id: 'EffectsRunActionParams',
  })
export const ExportRangeServiceInterfaceParams = z
  .object({
    hostId: HostId,
    internalStartPort: z.int(),
    id: ServiceInterfaceId,
    name: z.string(),
    description: z.string(),
    scheme: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'ExportRangeServiceInterfaceParams',
  })
export const ForgetGatewayParams = z.object({ gateway: GatewayId }).meta({
  id: 'ForgetGatewayParams',
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
export const GatewayInfo = z
  .object({ id: GatewayId, name: z.string(), public: z.boolean() })
  .meta({
    id: 'GatewayInfo',
  })

/**
 * Whether the gateway reachable via this interface speaks each port-mapping
 * protocol, as last probed. Fed by the watcher's periodic probes and by
 * failure/success evidence from the port-map client, and synced to the db so a
 * chronically uncooperative gateway is visible (and skipped) instead of being
 * retried forever.
 */
export const GatewayPortMapCapabilities = z
  .object({
    pcp: CapabilityVerdict,
    natPmp: CapabilityVerdict,
    upnp: CapabilityVerdict,
    /**
     * The PCP server answers ANNOUNCE with the Start9 capability marker, i.e.
     * it honors OPTION_HOSTNAME (SNI demux).
     */
    pcpHostname: CapabilityVerdict.meta({
      description:
        'The PCP server answers ANNOUNCE with the Start9 capability marker, i.e.\nit honors OPTION_HOSTNAME (SNI demux).',
    }),
  })
  .meta({
    id: 'GatewayPortMapCapabilities',
    description:
      "Whether the gateway reachable via this interface speaks each port-mapping\nprotocol, as last probed. Fed by the watcher's periodic probes and by\nfailure/success evidence from the port-map client, and synced to the db so a\nchronically uncooperative gateway is visible (and skipped) instead of being\nretried forever.",
  })
export const GetActionInputParams = z
  .object({
    packageId: wire(PackageId.nullable().optional(), PackageId.nullable()),
    actionId: ActionId,
    prefill: wire(
      z.record(z.string(), z.unknown()).nullable().optional(),
      z.record(z.string(), z.unknown()).nullable(),
    ),
  })
  .meta({
    id: 'GetActionInputParams',
  })
export const GetContainerIpParams = z
  .object({
    packageId: wire(PackageId.nullable().optional(), PackageId.nullable()),
    callback: wire(CallbackId.nullable().optional(), CallbackId.nullable()),
  })
  .meta({
    id: 'GetContainerIpParams',
  })
export const GetHostInfoParams = z
  .object({
    hostId: HostId,
    packageId: wire(PackageId.nullable().optional(), PackageId.nullable()),
    callback: wire(CallbackId.nullable().optional(), CallbackId.nullable()),
  })
  .meta({
    id: 'GetHostInfoParams',
  })
export const GetOutboundGatewayParams = z
  .object({
    callback: wire(CallbackId.nullable().optional(), CallbackId.nullable()),
  })
  .meta({
    id: 'GetOutboundGatewayParams',
  })
export const GetPackageParams = z
  .object({
    id: wire(PackageId.nullable().optional(), PackageId.nullable()),
    targetVersion: wire(
      z.string().nullable().optional(),
      z.string().nullable(),
    ),
    sourceVersion: wire(Version.nullable().optional(), Version.nullable()),
    otherVersions: wire(
      PackageDetailLevel.nullable().optional(),
      PackageDetailLevel.nullable(),
    ),
    allRevisions: wire(z.boolean().optional(), z.boolean()),
  })
  .meta({
    id: 'GetPackageParams',
  })
export const GetServiceInterfaceParams = z
  .object({
    packageId: wire(PackageId.nullable().optional(), PackageId.nullable()),
    serviceInterfaceId: ServiceInterfaceId,
    callback: wire(CallbackId.nullable().optional(), CallbackId.nullable()),
  })
  .meta({
    id: 'GetServiceInterfaceParams',
  })
export const GetServiceManifestParams = z
  .object({
    packageId: PackageId,
    callback: wire(CallbackId.nullable().optional(), CallbackId.nullable()),
  })
  .meta({
    id: 'GetServiceManifestParams',
  })
export const GetServicePortForwardParams = z
  .object({
    packageId: wire(PackageId.nullable().optional(), PackageId.nullable()),
    hostId: HostId,
    internalPort: z.int(),
  })
  .meta({
    id: 'GetServicePortForwardParams',
  })
export const GetSslCertificateParams = z
  .object({
    hostnames: z.array(z.string()),
    algorithm: wire(Algorithm.nullable().optional(), Algorithm.nullable()),
    callback: wire(CallbackId.nullable().optional(), CallbackId.nullable()),
  })
  .meta({
    id: 'GetSslCertificateParams',
  })
export const GetSslKeyParams = z
  .object({
    hostnames: z.array(z.string()),
    algorithm: wire(Algorithm.nullable().optional(), Algorithm.nullable()),
  })
  .meta({
    id: 'GetSslKeyParams',
  })
export const GetStatusParams = z
  .object({
    packageId: wire(PackageId.nullable().optional(), PackageId.nullable()),
    callback: wire(CallbackId.nullable().optional(), CallbackId.nullable()),
  })
  .meta({
    id: 'GetStatusParams',
  })
export const GetSystemSmtpParams = z
  .object({
    callback: wire(CallbackId.nullable().optional(), CallbackId.nullable()),
  })
  .meta({
    id: 'GetSystemSmtpParams',
  })
export const HardwareRequirements = z
  .object({
    device: wire(z.array(DeviceFilter).optional(), z.array(DeviceFilter)),
    ram: wire(z.number().nullable().optional(), z.number().nullable()),
    arch: wire(
      z.array(z.string()).nullable().optional(),
      z.array(z.string()).nullable(),
    ),
  })
  .meta({
    id: 'HardwareRequirements',
  })
export const HostnameMetadata = z
  .union([
    z.intersection(
      z.object({ kind: z.literal('ipv4') }),
      z.object({ gateway: GatewayId }),
    ),
    z.intersection(
      z.object({ kind: z.literal('ipv6') }),
      z.object({ gateway: GatewayId, scopeId: z.int() }),
    ),
    z.intersection(
      z.object({ kind: z.literal('mdns') }),
      z.object({ gateways: z.array(GatewayId) }),
    ),
    z.intersection(
      z.object({ kind: z.literal('private-domain') }),
      z.object({ gateways: z.array(GatewayId) }),
    ),
    z.intersection(
      z.object({ kind: z.literal('public-domain') }),
      z.object({ gateway: GatewayId }),
    ),
    z.intersection(
      z.object({ kind: z.literal('plugin') }),
      z.object({
        packageId: PackageId,
        removeAction: wire(ActionId.nullable().optional(), ActionId.nullable()),
        overflowActions: z.array(ActionId),
        info: wire(z.unknown().optional(), z.unknown()),
      }),
    ),
  ])
  .meta({
    id: 'HostnameMetadata',
  })
export const ImageSource = z
  .union([
    z.literal('packed'),
    z.object({
      dockerBuild: z.object({
        workdir: wire(z.string().nullable().optional(), z.string().nullable()),
        dockerfile: wire(
          z.string().nullable().optional(),
          z.string().nullable(),
        ),
        buildArgs: z.record(z.string(), BuildArg).nullable().optional(),
      }),
    }),
    z.object({ dockerTag: z.string() }),
  ])
  .meta({
    id: 'ImageSource',
  })
export const InfoParams = z
  .object({
    targetId: BackupTargetId,
    serverId: z.string(),
    password: z.string(),
  })
  .meta({
    id: 'InfoParams',
  })
export const InitAcmeParams = z
  .object({ provider: AcmeProvider, contact: z.array(z.string()) })
  .meta({
    id: 'InitAcmeParams',
  })
export const InstallParams = z
  .object({ registry: z.string(), id: PackageId, version: Version })
  .meta({
    id: 'InstallParams',
  })
export const InstalledVersionParams = z.object({ id: PackageId }).meta({
  id: 'InstalledVersionParams',
})
export const IpInfo = z
  .object({
    name: z.string(),
    scopeId: z.int(),
    deviceType: wire(
      NetworkInterfaceType.nullable().optional(),
      NetworkInterfaceType.nullable(),
    ),
    subnets: z.array(z.string()),
    lanIp: z.array(z.string()),
    wanIp: wire(z.string().nullable().optional(), z.string().nullable()),
    ntpServers: z.array(z.string()),
    dnsServers: z.array(z.string()),
  })
  .meta({
    id: 'IpInfo',
  })
export const LegacyAlpnInfo = z
  .union([
    z.literal('reflect'),
    z.object({ specified: z.array(MaybeUtf8String) }),
  ])
  .meta({
    id: 'LegacyAlpnInfo',
  })
export const ListPackageSignersParams = z.object({ id: PackageId }).meta({
  id: 'ListPackageSignersParams',
})
export const ListServiceInterfacesParams = z
  .object({
    packageId: wire(PackageId.nullable().optional(), PackageId.nullable()),
    callback: wire(CallbackId.nullable().optional(), CallbackId.nullable()),
  })
  .meta({
    id: 'ListServiceInterfacesParams',
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
export const LshwDevice = z
  .union([
    z.intersection(z.object({ class: z.literal('processor') }), LshwProcessor),
    z.intersection(z.object({ class: z.literal('display') }), LshwDisplay),
  ])
  .meta({
    id: 'LshwDevice',
  })
export const Metadata = z
  .object({ title: LocaleString, icon: PathOrUrl })
  .meta({
    id: 'Metadata',
  })
export const MetricsCpu = z
  .object({
    percentageUsed: Percentage,
    idle: Percentage,
    userSpace: Percentage,
    kernelSpace: Percentage,
    wait: Percentage,
  })
  .meta({
    id: 'MetricsCpu',
  })
export const MetricsDisk = z
  .object({
    percentageUsed: Percentage,
    used: GigaBytes,
    available: GigaBytes,
    capacity: GigaBytes,
  })
  .meta({
    id: 'MetricsDisk',
  })
export const MetricsGeneral = z
  .object({
    temperature: wire(Celsius.nullable().optional(), Celsius.nullable()),
  })
  .meta({
    id: 'MetricsGeneral',
  })
export const MetricsMemory = z
  .object({
    percentageUsed: Percentage,
    total: MebiBytes,
    available: MebiBytes,
    used: MebiBytes,
    zramTotal: MebiBytes,
    zramAvailable: MebiBytes,
    zramUsed: MebiBytes,
  })
  .meta({
    id: 'MetricsMemory',
  })
export const MetricsSummary = z
  .object({
    totalCheckins: z.int(),
    uniqueServers: z.int(),
    totalPackageRequests: z.int(),
    byArch: z.array(CountEntry),
    byOsVersion: z.array(CountEntry),
  })
  .meta({
    id: 'MetricsSummary',
  })
export const MountTarget = wire(
  z.object({
    packageId: PackageId,
    volumeId: VolumeId,
    subpath: z.string().nullable().optional(),
    readonly: z.boolean(),
    idmap: z.array(IdMap).optional(),
  }),
  z.object({
    packageId: PackageId,
    volumeId: VolumeId,
    subpath: z.string().nullable(),
    readonly: z.boolean(),
    idmap: z.array(IdMap),
  }),
).meta({
  id: 'MountTarget',
})
export const NamedHealthCheckResult = z
  .intersection(z.object({ name: z.string() }), NamedHealthCheckResultKind)
  .meta({
    id: 'NamedHealthCheckResult',
  })
export const Notification = z
  .object({
    packageId: wire(PackageId.nullable().optional(), PackageId.nullable()),
    createdAt: z.string(),
    code: z.int(),
    level: NotificationLevel,
    title: z.string(),
    message: z.string(),
    data: z.unknown(),
    seen: wire(z.boolean().optional(), z.boolean()),
  })
  .meta({
    id: 'Notification',
  })
export const OsVersionInfo = z
  .object({
    headline: z.string(),
    releaseNotes: z.string(),
    sourceVersion: z.string(),
    authorized: z.array(Guid),
    iso: z.record(z.string(), RegistryAsset(Blake3Commitment)),
    squashfs: z.record(z.string(), RegistryAsset(Blake3Commitment)),
    img: z.record(z.string(), RegistryAsset(Blake3Commitment)),
  })
  .meta({
    id: 'OsVersionInfo',
  })
export const PackageBackupInfo = z
  .object({
    title: z.string(),
    version: Version,
    osVersion: z.string(),
    timestamp: z.string(),
  })
  .meta({
    id: 'PackageBackupInfo',
  })
export const PackageInfoShort = z.object({ releaseNotes: LocaleString }).meta({
  id: 'PackageInfoShort',
})
export const PasswordType = z.union([EncryptedWire, z.string()]).meta({
  id: 'PasswordType',
})
export const PluginHostnameInfo = wire(
  z.object({
    /**
     * [`PackageId::start_os`] identifies the server's own host (the StartOS UI).
     */
    packageId: PackageId.nullable().optional().meta({
      description:
        "[`PackageId::start_os`] identifies the server's own host (the StartOS UI).",
    }),
    hostId: HostId,
    internalPort: z.int(),
    ssl: z.boolean(),
    public: z.boolean(),
    hostname: z.string(),
    port: z.int().nullable().optional(),
    info: z.unknown().optional(),
  }),
  z.object({
    /**
     * [`PackageId::start_os`] identifies the server's own host (the StartOS UI).
     */
    packageId: PackageId.meta({
      description:
        "[`PackageId::start_os`] identifies the server's own host (the StartOS UI).",
    }),
    hostId: HostId,
    internalPort: z.int(),
    ssl: z.boolean(),
    public: z.boolean(),
    hostname: z.string(),
    port: z.int().nullable(),
    info: z.unknown(),
  }),
).meta({
  id: 'PluginHostnameInfo',
})
export const PortForward = z
  .object({
    src: z.string(),
    dst: z.string(),
    gateway: GatewayId,
    /**
     * Number of contiguous ports covered by this forward (always >= 1).
     * Set to >1 by [`crate::net::host::binding::RangeBindInfo`] entries to
     * represent a single iptables port-range rule rather than N per-port
     * rules. Defaults to 1 to keep the on-disk shape compatible with
     * existing single-port `PortForward`s.
     */
    count: wire(z.int().optional(), z.int()).meta({
      description:
        'Number of contiguous ports covered by this forward (always >= 1).\nSet to >1 by [`crate::net::host::binding::RangeBindInfo`] entries to\nrepresent a single iptables port-range rule rather than N per-port\nrules. Defaults to 1 to keep the on-disk shape compatible with\nexisting single-port `PortForward`s.',
    }),
    /**
     * Whether this forward receives local traffic. Used to gate hairpinning check.
     */
    local: wire(z.boolean().optional(), z.boolean()).meta({
      description:
        'Whether this forward receives local traffic. Used to gate hairpinning check.',
    }),
  })
  .meta({
    id: 'PortForward',
  })
export const PreDownloadAlert = z
  .object({ message: LocaleString, when: PreDownloadAlertWhen })
  .meta({
    id: 'PreDownloadAlert',
  })

/**
 * Auth gate enforced by the OS reverse proxy on incoming requests.
 *
 * - `Bearer { tokens }`: any of `tokens` is accepted as `Authorization: Bearer <token>`.
 * - `Basic  { credentials }`: any `(username, password)` pair in `credentials` is
 *   accepted as `Authorization: Basic <base64(username:password)>`. The matched
 *   `username` is forwarded upstream as `X-Forwarded-User`.
 *
 * `realm` is the authentication realm advertised in the
 * `WWW-Authenticate` challenge sent on 401 responses (RFC 7235
 * §2.2). Defaults to `"StartOS"` when unset. Packages that share
 * credentials across multiple bindings should pick a stable realm
 * so that browsers reuse cached credentials across them.
 */
export const ProxyAuth = z
  .union([
    z.intersection(
      z.object({ type: z.literal('bearer') }),
      z.object({
        tokens: z.array(z.string()),
        realm: wire(z.string().nullable().optional(), z.string().nullable()),
      }),
    ),
    z.intersection(
      z.object({ type: z.literal('basic') }),
      z.object({
        credentials: z.array(BasicCredential),
        realm: wire(z.string().nullable().optional(), z.string().nullable()),
      }),
    ),
  ])
  .meta({
    id: 'ProxyAuth',
    description:
      'Auth gate enforced by the OS reverse proxy on incoming requests.\n\n- `Bearer { tokens }`: any of `tokens` is accepted as `Authorization: Bearer <token>`.\n- `Basic  { credentials }`: any `(username, password)` pair in `credentials` is\n  accepted as `Authorization: Basic <base64(username:password)>`. The matched\n  `username` is forwarded upstream as `X-Forwarded-User`.\n\n`realm` is the authentication realm advertised in the\n`WWW-Authenticate` challenge sent on 401 responses (RFC 7235\n§2.2). Defaults to `"StartOS"` when unset. Packages that share\ncredentials across multiple bindings should pick a stable realm\nso that browsers reuse cached credentials across them.',
  })
export const PublicDomainConfig = z
  .object({
    gateway: GatewayId,
    acme: wire(AcmeProvider.nullable().optional(), AcmeProvider.nullable()),
  })
  .meta({
    id: 'PublicDomainConfig',
  })

/**
 * The single restricted service interface a port-range binding may export.
 *
 * Unlike [`ServiceInterface`], a range interface is always `api`-typed and
 * carries no `masked` / `username` / `path` / `query` and no per-address
 * [`AddressInfo`] — its address is the host plus the range's external port
 * span, taken from the [`RangeBindInfo`](crate::net::host::binding::RangeBindInfo)
 * it lives under. `scheme` is an optional transport prefix (e.g. `tcp` for
 * bitcoin ZMQ endpoints); most ranges (coturn RTP, FTP data) omit it.
 */
export const RangeServiceInterface = z
  .object({
    id: ServiceInterfaceId,
    name: z.string(),
    description: z.string(),
    scheme: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'RangeServiceInterface',
    description:
      "The single restricted service interface a port-range binding may export.\n\nUnlike [`ServiceInterface`], a range interface is always `api`-typed and\ncarries no `masked` / `username` / `path` / `query` and no per-address\n[`AddressInfo`] — its address is the host plus the range's external port\nspan, taken from the [`RangeBindInfo`](crate::net::host::binding::RangeBindInfo)\nit lives under. `scheme` is an optional transport prefix (e.g. `tcp` for\nbitcoin ZMQ endpoints); most ranges (coturn RTP, FTP data) omit it.",
  })
export const RebuildParams = z.object({ id: PackageId }).meta({
  id: 'RebuildParams',
})
export const RemoveAcmeParams = z.object({ provider: AcmeProvider }).meta({
  id: 'RemoveAcmeParams',
})
export const RemoveAdminParams = z.object({ signer: Guid }).meta({
  id: 'RemoveAdminParams',
})
export const RemoveMirrorParams = z
  .object({ id: PackageId, version: Version, url: z.string() })
  .meta({
    id: 'RemoveMirrorParams',
  })
export const RemovePackageFromCategoryParams = z
  .object({ id: z.string(), package: PackageId })
  .meta({
    id: 'RemovePackageFromCategoryParams',
  })
export const RemovePackageParams = z
  .object({
    id: PackageId,
    version: wire(Version.nullable().optional(), Version.nullable()),
    sighash: wire(z.string().nullable().optional(), z.string().nullable()),
    force: wire(z.boolean().optional(), z.boolean()),
  })
  .meta({
    id: 'RemovePackageParams',
  })
export const RemovePackageSignerParams = z
  .object({ id: PackageId, signer: Guid })
  .meta({
    id: 'RemovePackageSignerParams',
  })
export const RemoveSignerParams = z.object({ id: Guid }).meta({
  id: 'RemoveSignerParams',
})
export const RemoveTunnelParams = z.object({ id: GatewayId }).meta({
  id: 'RemoveTunnelParams',
})
export const RenameGatewayParams = z
  .object({ id: GatewayId, name: z.string() })
  .meta({
    id: 'RenameGatewayParams',
  })
export const RestorePackageParams = z
  .object({
    targetId: BackupTargetId,
    password: z.string(),
    ids: z.array(PackageId),
    serverId: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'RestorePackageParams',
  })
export const RetireBindingParams = z
  .object({ id: HostId, internalPort: z.int() })
  .meta({
    id: 'RetireBindingParams',
  })
export const RetireHostParams = z.object({ id: HostId }).meta({
  id: 'RetireHostParams',
})
export const RunActionParams = z
  .object({
    packageId: PackageId,
    eventId: wire(Guid.nullable().optional(), Guid.nullable()),
    actionId: ActionId,
    input: wire(z.unknown().nullable().optional(), z.unknown().nullable()),
  })
  .meta({
    id: 'RunActionParams',
  })
export const ServiceDependencyMetadata = z
  .object({ title: LocaleString })
  .meta({
    id: 'ServiceDependencyMetadata',
  })
export const SetDefaultOutboundParams = z
  .object({
    gateway: wire(GatewayId.nullable().optional(), GatewayId.nullable()),
  })
  .meta({
    id: 'SetDefaultOutboundParams',
  })
export const SetDescriptionParams = z
  .object({ description: LocaleString.nullable() })
  .meta({
    id: 'SetDescriptionParams',
  })
export const SetGatewaySecureParams = z
  .object({
    gateway: GatewayId,
    secure: wire(z.boolean().nullable().optional(), z.boolean().nullable()),
  })
  .meta({
    id: 'SetGatewaySecureParams',
  })
export const SetIconParams = z.object({ icon: DataUrl.nullable() }).meta({
  id: 'SetIconParams',
})
export const SetMainStatus = z.object({ status: SetMainStatusStatus }).meta({
  id: 'SetMainStatus',
})
export const SetOutboundGatewayParams = z
  .object({
    package: PackageId,
    gateway: wire(GatewayId.nullable().optional(), GatewayId.nullable()),
  })
  .meta({
    id: 'SetOutboundGatewayParams',
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
export const SideloadResponse = z
  .object({ upload: Guid, progress: Guid })
  .meta({
    id: 'SideloadResponse',
  })
export const SignAssetParams = z
  .object({
    version: z.string(),
    platform: z.string(),
    signature: AnySignature,
  })
  .meta({
    id: 'SignAssetParams',
  })
export const SignerInfo = z
  .object({
    name: z.string(),
    contact: z.array(ContactInfo),
    keys: z.array(AnyVerifyingKey),
  })
  .meta({
    id: 'SignerInfo',
  })
export const SmtpValue = wire(
  z.intersection(
    z.intersection(
      z.object({
        host: z.string().optional(),
        server: z.string().optional(),
        port: z.int(),
        from: z.string(),
        login: z.string().optional(),
        username: z.string().optional(),
        password: z.string().nullable().optional(),
        security: SmtpSecurity.optional(),
      }),
      z.union([
        z.object({ host: z.string() }),
        z.object({ server: z.string() }),
      ]),
    ),
    z.union([
      z.object({ login: z.string() }),
      z.object({ username: z.string() }),
    ]),
  ),
  z.object({
    host: z.string(),
    port: z.int(),
    from: z.string(),
    username: z.string(),
    password: z.string().nullable(),
    security: SmtpSecurity,
  }),
).meta({
  id: 'SmtpValue',
})
export const SshAddParams = z.object({ key: SshPubKey }).meta({
  id: 'SshAddParams',
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
export const TaskInput = wire(
  TaskInputRepr,
  z.intersection(
    z.object({ kind: z.literal('partial') }),
    z.object({
      accept: z.array(z.record(z.string(), z.unknown())),
      set: z.record(z.string(), z.unknown()),
    }),
  ),
).meta({
  id: 'TaskInput',
})
export const TaskTrigger = z
  .object({
    once: wire(z.boolean().optional(), z.boolean()),
    condition: TaskCondition,
  })
  .meta({
    id: 'TaskTrigger',
  })
export const TestSmtpParams = z
  .object({
    host: z.string(),
    port: z.int(),
    from: z.string(),
    to: z.string(),
    username: z.string(),
    password: z.string(),
    security: wire(SmtpSecurity.optional(), SmtpSecurity),
  })
  .meta({
    id: 'TestSmtpParams',
  })
export const UmountParams = z
  .object({
    targetId: wire(
      BackupTargetId.nullable().optional(),
      BackupTargetId.nullable(),
    ),
  })
  .meta({
    id: 'UmountParams',
  })
export const UninstallParams = z
  .object({
    id: PackageId,
    soft: wire(z.boolean().optional(), z.boolean()),
    force: wire(z.boolean().optional(), z.boolean()),
  })
  .meta({
    id: 'UninstallParams',
  })
export const UnsetGatewaySecureParams = z.object({ gateway: GatewayId }).meta({
  id: 'UnsetGatewaySecureParams',
})
export const UpdateTunnelParams = z
  .object({ id: GatewayId, config: z.string() })
  .meta({
    id: 'UpdateTunnelParams',
  })
export const UrlPluginRegisterParams = z
  .object({ tableAction: ActionId })
  .meta({
    id: 'UrlPluginRegisterParams',
  })
export const UrlPluginRegistration = z.object({ tableAction: ActionId }).meta({
  id: 'UrlPluginRegistration',
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
export const VersionSignerParams = z
  .object({ version: z.string(), signer: Guid })
  .meta({
    id: 'VersionSignerParams',
  })
export const WifiInfo = z
  .object({
    enabled: z.boolean(),
    interface: wire(GatewayId.nullable().optional(), GatewayId.nullable()),
    ssids: z.array(z.string()),
    selected: wire(z.string().nullable().optional(), z.string().nullable()),
    lastRegion: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'WifiInfo',
  })
export const WifiListOut = z
  .object({
    ssid: Ssid,
    strength: SignalStrength,
    security: z.array(z.string()),
  })
  .meta({
    id: 'WifiListOut',
  })
export const ActionResult = z
  .union([
    z.intersection(z.object({ version: z.literal('0') }), ActionResultV0),
    z.intersection(z.object({ version: z.literal('1') }), ActionResultV1),
  ])
  .meta({
    id: 'ActionResult',
  })
export const AddPublicDomainRes = z
  .object({
    dns: QueryDnsRes,
    port: CheckPortRes,
    portV6: wire(
      CheckPortV6Res.nullable().optional(),
      CheckPortV6Res.nullable(),
    ),
    /**
     * The authority's own reachability requirement, where the domain has one.
     */
    challenge: wire(
      CheckChallengeRes.nullable().optional(),
      CheckChallengeRes.nullable(),
    ).meta({
      description:
        "The authority's own reachability requirement, where the domain has one.",
    }),
  })
  .meta({
    id: 'AddPublicDomainRes',
  })
export const BackupInfo = z
  .object({
    version: z.string(),
    timestamp: wire(z.string().nullable().optional(), z.string().nullable()),
    packageBackups: z.record(z.string(), PackageBackupInfo),
  })
  .meta({
    id: 'BackupInfo',
  })
export const BackupParams = z
  .object({
    targetId: BackupTargetId,
    oldPassword: wire(
      PasswordType.nullable().optional(),
      PasswordType.nullable(),
    ),
    packageIds: wire(
      z.array(PackageId).nullable().optional(),
      z.array(PackageId).nullable(),
    ),
    password: PasswordType,
  })
  .meta({
    id: 'BackupParams',
  })
export const CifsBackupTarget = z
  .object({
    hostname: z.string(),
    path: z.string(),
    username: z.string(),
    mountable: z.boolean(),
    available: wire(z.number().nullable().optional(), z.number().nullable()),
    startOs: z.record(z.string(), StartOsRecoveryInfo),
    legacyBackup: z.boolean(),
  })
  .meta({
    id: 'CifsBackupTarget',
  })
export const ClearBindingsParams = z
  .object({ except: wire(z.array(BindId).optional(), z.array(BindId)) })
  .meta({
    id: 'ClearBindingsParams',
  })
export const CompatibleAlpnInfo = z
  .union([z.array(MaybeUtf8String), LegacyAlpnInfo])
  .meta({
    id: 'CompatibleAlpnInfo',
  })
export const CurrentDependencies = z
  .record(z.string(), CurrentDependencyInfo)
  .meta({
    id: 'CurrentDependencies',
  })
export const ExportActionParams = z
  .object({ id: ActionId, metadata: ActionMetadata })
  .meta({
    id: 'ExportActionParams',
  })
export const ExportServiceInterfaceParams = z
  .object({
    id: ServiceInterfaceId,
    name: z.string(),
    description: z.string(),
    masked: z.boolean(),
    addressInfo: AddressInfo,
    type: ServiceInterfaceType,
    /**
     * The interface address Open UI should prefer.
     */
    preferredLauncherAddress: wire(
      z.string().nullable().optional(),
      z.string().nullable(),
    ).meta({
      description: 'The interface address Open UI should prefer.',
    }),
  })
  .meta({
    id: 'ExportServiceInterfaceParams',
  })
export const HostnameInfo = z
  .object({
    ssl: z.boolean(),
    public: z.boolean(),
    hostname: z.string(),
    port: wire(z.int().nullable().optional(), z.int().nullable()),
    metadata: HostnameMetadata,
  })
  .meta({
    id: 'HostnameInfo',
  })
export const ImageConfigRepr = z
  .object({
    source: ImageSource,
    arch: z.array(z.string()),
    emulateMissing: wire(
      z.boolean().nullable().optional(),
      z.boolean().nullable(),
    ),
    emulateMissingAs: wire(
      z.string().nullable().optional(),
      z.string().nullable(),
    ),
    nvidiaContainer: wire(z.boolean().optional(), z.boolean()),
  })
  .meta({
    id: 'ImageConfigRepr',
  })
export const InitProgressRes = z
  .object({ progress: FullProgress, guid: Guid })
  .meta({
    id: 'InitProgressRes',
  })
export const MetadataSrc = z
  .union([
    z.object({ metadata: Metadata }),
    z.object({ s9pk: PathOrUrl.nullable() }),
  ])
  .meta({
    id: 'MetadataSrc',
  })
export const Metrics = z
  .object({
    general: MetricsGeneral,
    memory: MetricsMemory,
    cpu: MetricsCpu,
    disk: MetricsDisk,
  })
  .meta({
    id: 'Metrics',
  })
export const MountParams = z
  .object({ location: z.string(), target: MountTarget })
  .meta({
    id: 'MountParams',
  })
export const NetworkInterfaceInfo = wire(
  z.object({
    name: z.string().nullable().optional(),
    secure: z.boolean().nullable().optional(),
    ipInfo: IpInfo.nullable().optional(),
    type: GatewayType.nullable().optional(),
    portMap: GatewayPortMapCapabilities.optional(),
    /**
     * The gateway's resolver accepted our last RFC 2136 DNS UPDATE — evidence
     * from the update client (`net::dns_update`). A WireGuard gateway only
     * serves the injected `<hostname>.local` while this is `supported`, so
     * only then is the name listed on it.
     */
    dnsUpdate: CapabilityVerdict.optional().meta({
      description:
        "The gateway's resolver accepted our last RFC 2136 DNS UPDATE — evidence\nfrom the update client (`net::dns_update`). A WireGuard gateway only\nserves the injected `<hostname>.local` while this is `supported`, so\nonly then is the name listed on it.",
    }),
  }),
  z.object({
    name: z.string().nullable(),
    secure: z.boolean().nullable(),
    ipInfo: IpInfo.nullable(),
    type: GatewayType,
    portMap: GatewayPortMapCapabilities,
    /**
     * The gateway's resolver accepted our last RFC 2136 DNS UPDATE — evidence
     * from the update client (`net::dns_update`). A WireGuard gateway only
     * serves the injected `<hostname>.local` while this is `supported`, so
     * only then is the name listed on it.
     */
    dnsUpdate: CapabilityVerdict.meta({
      description:
        "The gateway's resolver accepted our last RFC 2136 DNS UPDATE — evidence\nfrom the update client (`net::dns_update`). A WireGuard gateway only\nserves the injected `<hostname>.local` while this is `supported`, so\nonly then is the name listed on it.",
    }),
  }),
).meta({
  id: 'NetworkInterfaceInfo',
})
export const NotificationWithId = z
  .intersection(z.object({ id: z.int() }), Notification)
  .meta({
    id: 'NotificationWithId',
  })
export const OsVersionInfoMap = z.record(z.string(), OsVersionInfo).meta({
  id: 'OsVersionInfoMap',
})
export const PackageMetadata = z
  .object({
    title: z.string(),
    description: Description,
    releaseNotes: LocaleString,
    preDownloadAlert: PreDownloadAlert.nullable().optional(),
    gitHash: wire(GitHash.nullable().optional(), GitHash.nullable()),
    license: z.string(),
    packageRepo: wire(z.string().optional(), z.string()),
    upstreamRepo: z.string(),
    marketingUrl: wire(z.string().nullable().optional(), z.string().nullable()),
    donationUrl: wire(z.string().nullable().optional(), z.string().nullable()),
    osVersion: wire(z.string().optional(), z.string()),
    sdkVersion: wire(z.string().nullable().optional(), z.string().nullable()),
    hardwareAcceleration: wire(z.boolean().optional(), z.boolean()),
    /**
     * Grants access to `/dev/fuse`.
     */
    userspaceFilesystems: wire(z.boolean().optional(), z.boolean()).meta({
      description: 'Grants access to `/dev/fuse`.',
    }),
    /**
     * Grants access to `/dev/net/tun`.
     */
    virtualNetworking: wire(z.boolean().optional(), z.boolean()).meta({
      description: 'Grants access to `/dev/net/tun`.',
    }),
    /**
     * Grants /dev/kvm when present. The opening process must run as container root.
     */
    hardwareVirtualization: wire(z.boolean().optional(), z.boolean()).meta({
      description:
        'Grants /dev/kvm when present. The opening process must run as container root.',
    }),
    plugins: wire(z.array(PluginId).optional(), z.array(PluginId)),
    satisfies: wire(z.array(Version).optional(), z.array(Version)),
  })
  .meta({
    id: 'PackageMetadata',
  })
export const PackagePlugin = z
  .object({
    url: wire(
      UrlPluginRegistration.nullable().optional(),
      UrlPluginRegistration.nullable(),
    ),
  })
  .meta({
    id: 'PackagePlugin',
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
export const RegistryInfo = z
  .object({
    name: wire(z.string().nullable().optional(), z.string().nullable()),
    icon: wire(DataUrl.nullable().optional(), DataUrl.nullable()),
    /**
     * Markdown, shown above the registry's services.
     */
    description: wire(
      LocaleString.nullable().optional(),
      LocaleString.nullable(),
    ).meta({
      description: "Markdown, shown above the registry's services.",
    }),
    categories: z.record(z.string(), Category),
  })
  .meta({
    id: 'RegistryInfo',
  })
export const ResetPasswordParams = z
  .object({
    newPassword: wire(
      PasswordType.nullable().optional(),
      PasswordType.nullable(),
    ),
  })
  .meta({
    id: 'ResetPasswordParams',
  })
export const ServerStatus = z
  .object({
    backupProgress: wire(
      FullProgress.nullable().optional(),
      FullProgress.nullable(),
    ),
    updateProgress: wire(
      FullProgress.nullable().optional(),
      FullProgress.nullable(),
    ),
    shuttingDown: wire(z.boolean().optional(), z.boolean()),
    restarting: wire(z.boolean().optional(), z.boolean()),
    restart: wire(
      RestartReason.nullable().optional(),
      RestartReason.nullable(),
    ),
  })
  .meta({
    id: 'ServerStatus',
  })
export const ServiceInterface = z
  .object({
    id: ServiceInterfaceId,
    name: z.string(),
    description: z.string(),
    masked: z.boolean(),
    addressInfo: AddressInfo,
    type: ServiceInterfaceType,
    /**
     * The interface address Open UI should prefer.
     */
    preferredLauncherAddress: wire(
      z.string().nullable().optional(),
      z.string().nullable(),
    ).meta({
      description: 'The interface address Open UI should prefer.',
    }),
  })
  .meta({
    id: 'ServiceInterface',
  })
export const SessionList = z
  .object({
    current: wire(z.string().nullable().optional(), z.string().nullable()),
    sessions: AuthKeys,
  })
  .meta({
    id: 'SessionList',
  })
export const SetDependenciesParams = z
  .object({ dependencies: z.array(DependencyRequirement) })
  .meta({
    id: 'SetDependenciesParams',
  })
export const SetHealth = z
  .intersection(z.object({ id: HealthCheckId }), NamedHealthCheckResult)
  .meta({
    id: 'SetHealth',
  })
export const SetupProgress = z
  .object({ progress: FullProgress, guid: Guid })
  .meta({
    id: 'SetupProgress',
  })
export const StatusInfo = z
  .object({
    health: z.record(z.string(), NamedHealthCheckResult),
    error: wire(ErrorData.nullable().optional(), ErrorData.nullable()),
    started: wire(z.string().nullable().optional(), z.string().nullable()),
    desired: DesiredStatus,
  })
  .meta({
    id: 'StatusInfo',
  })
export const Task = z
  .object({
    packageId: PackageId,
    actionId: ActionId,
    severity: wire(TaskSeverity.optional(), TaskSeverity),
    reason: wire(z.string().nullable().optional(), z.string().nullable()),
    when: wire(TaskTrigger.nullable().optional(), TaskTrigger.nullable()),
    input: wire(TaskInput.nullable().optional(), TaskInput.nullable()),
  })
  .meta({
    id: 'Task',
  })
export const UrlPluginClearUrlsParams = z
  .object({ except: z.array(PluginHostnameInfo) })
  .meta({
    id: 'UrlPluginClearUrlsParams',
  })
export const UrlPluginExportUrlParams = z
  .object({
    hostnameInfo: PluginHostnameInfo,
    removeAction: wire(ActionId.nullable().optional(), ActionId.nullable()),
    overflowActions: z.array(ActionId),
  })
  .meta({
    id: 'UrlPluginExportUrlParams',
  })
export const WifiListInfo = z
  .object({
    ssids: z.record(z.string(), SignalStrength),
    connected: wire(Ssid.nullable().optional(), Ssid.nullable()),
    country: wire(z.string().nullable().optional(), z.string().nullable()),
    ethernet: z.boolean(),
    availableWifi: z.array(WifiListOut),
  })
  .meta({
    id: 'WifiListInfo',
  })
export const AddSslOptions = wire(
  z.object({
    preferredExternalPort: z.int(),
    /**
     * When `true`, the OS reverse proxy adds `X-Forwarded-Proto: https`
     * and `X-Forwarded-For: <client-ip>` to incoming HTTP requests before
     * forwarding them upstream. Setting this implies HTTP-aware proxying.
     */
    addXForwardedHeaders: z.boolean().optional().meta({
      description:
        'When `true`, the OS reverse proxy adds `X-Forwarded-Proto: https`\nand `X-Forwarded-For: <client-ip>` to incoming HTTP requests before\nforwarding them upstream. Setting this implies HTTP-aware proxying.',
    }),
    /**
     * The application protocols StartOS answers a client with, from those it
     * asked for. Unset answers with whatever it asked for.
     */
    alpn: CompatibleAlpnInfo.nullable().optional().meta({
      description:
        'The application protocols StartOS answers a client with, from those it\nasked for. Unset answers with whatever it asked for.',
    }),
    /**
     * Certificate validation for the OS→container TLS leg when rewrapping.
     * `None` (the default) validates against the StartOS root CA.
     */
    upstreamCertValidation: UpstreamCertValidation.nullable().optional().meta({
      description:
        'Certificate validation for the OS→container TLS leg when rewrapping.\n`None` (the default) validates against the StartOS root CA.',
    }),
    /**
     * Optional reverse-proxy auth gate. When set, the OS reverse proxy
     * will validate the `Authorization` header on incoming HTTP requests
     * against this configuration before forwarding them upstream.
     * Unauthenticated requests get `401 Unauthorized` with an appropriate
     * `WWW-Authenticate` challenge. For `Basic`, the authenticated
     * username is forwarded to the upstream service as `X-Forwarded-User`.
     * Setting this implies HTTP-aware proxying.
     */
    auth: ProxyAuth.nullable().optional().meta({
      description:
        'Optional reverse-proxy auth gate. When set, the OS reverse proxy\nwill validate the `Authorization` header on incoming HTTP requests\nagainst this configuration before forwarding them upstream.\nUnauthenticated requests get `401 Unauthorized` with an appropriate\n`WWW-Authenticate` challenge. For `Basic`, the authenticated\nusername is forwarded to the upstream service as `X-Forwarded-User`.\nSetting this implies HTTP-aware proxying.',
    }),
  }),
  z.object({
    preferredExternalPort: z.int(),
    /**
     * When `true`, the OS reverse proxy adds `X-Forwarded-Proto: https`
     * and `X-Forwarded-For: <client-ip>` to incoming HTTP requests before
     * forwarding them upstream. Setting this implies HTTP-aware proxying.
     */
    addXForwardedHeaders: z.boolean().meta({
      description:
        'When `true`, the OS reverse proxy adds `X-Forwarded-Proto: https`\nand `X-Forwarded-For: <client-ip>` to incoming HTTP requests before\nforwarding them upstream. Setting this implies HTTP-aware proxying.',
    }),
    /**
     * The application protocols StartOS answers a client with, from those it
     * asked for. Unset answers with whatever it asked for.
     */
    alpn: AlpnInfo.nullable().meta({
      description:
        'The application protocols StartOS answers a client with, from those it\nasked for. Unset answers with whatever it asked for.',
    }),
    /**
     * Certificate validation for the OS→container TLS leg when rewrapping.
     * `None` (the default) validates against the StartOS root CA.
     */
    upstreamCertValidation: UpstreamCertValidation.nullable().meta({
      description:
        'Certificate validation for the OS→container TLS leg when rewrapping.\n`None` (the default) validates against the StartOS root CA.',
    }),
    /**
     * Optional reverse-proxy auth gate. When set, the OS reverse proxy
     * will validate the `Authorization` header on incoming HTTP requests
     * against this configuration before forwarding them upstream.
     * Unauthenticated requests get `401 Unauthorized` with an appropriate
     * `WWW-Authenticate` challenge. For `Basic`, the authenticated
     * username is forwarded to the upstream service as `X-Forwarded-User`.
     * Setting this implies HTTP-aware proxying.
     */
    auth: ProxyAuth.nullable().meta({
      description:
        'Optional reverse-proxy auth gate. When set, the OS reverse proxy\nwill validate the `Authorization` header on incoming HTTP requests\nagainst this configuration before forwarding them upstream.\nUnauthenticated requests get `401 Unauthorized` with an appropriate\n`WWW-Authenticate` challenge. For `Basic`, the authenticated\nusername is forwarded to the upstream service as `X-Forwarded-User`.\nSetting this implies HTTP-aware proxying.',
    }),
  }),
).meta({
  id: 'AddSslOptions',
})
export const BackupTarget = z
  .union([
    z.intersection(
      z.object({ type: z.literal('disk') }),
      z.intersection(
        z.object({
          vendor: wire(z.string().nullable().optional(), z.string().nullable()),
          model: wire(z.string().nullable().optional(), z.string().nullable()),
        }),
        PartitionInfo,
      ),
    ),
    z.intersection(z.object({ type: z.literal('cifs') }), CifsBackupTarget),
  ])
  .meta({
    id: 'BackupTarget',
  })
export const BindingSetAddressEnabledParams = z
  .object({
    internalPort: z.int(),
    address: HostnameInfo,
    enabled: wire(z.boolean().nullable().optional(), z.boolean().nullable()),
  })
  .meta({
    id: 'BindingSetAddressEnabledParams',
  })
export const BindingSetGuaWanParams = z
  .object({ internalPort: z.int(), address: HostnameInfo, wan: z.boolean() })
  .meta({
    id: 'BindingSetGuaWanParams',
  })
export const CreateTaskParams = z
  .intersection(z.object({ replayId: ReplayId }), Task)
  .meta({
    id: 'CreateTaskParams',
  })
export const DepInfo = z
  .intersection(
    z.intersection(
      z.object({
        description: wire(
          LocaleString.nullable().optional(),
          LocaleString.nullable(),
        ),
        optional: z.boolean(),
        versionRange: wire(
          z.string().nullable().optional(),
          z.string().nullable(),
        ),
      }),
      z.union([
        CurrentDependencyKind.nullable(),
        z.object({}) as z.ZodType<{}, {}>,
      ]),
    ),
    z.union([MetadataSrc.nullable(), z.object({}) as z.ZodType<{}, {}>]),
  )
  .meta({
    id: 'DepInfo',
  })
export const DerivedAddressInfo = z
  .object({
    /**
     * User override: enable these addresses (only for public IP & port)
     */
    enabled: z.array(z.string()).meta({
      description:
        'User override: enable these addresses (only for public IP & port)',
    }),
    /**
     * User override: disable these addresses (only for domains and private IP & port)
     */
    disabled: z.array(z.tuple([z.string(), z.int()])).meta({
      description:
        'User override: disable these addresses (only for domains and private IP & port)',
    }),
    /**
     * User override: IPv6 global-unicast addresses opted into WAN exposure.
     * Projected into `HostnameInfo.public` by `update_addresses`, so `public`
     * stays the single source of truth for WAN reachability (P2P address
     * selection, upstream pinholes). A GUA not in this set is LAN-only.
     */
    guaWan: wire(z.array(z.string()).optional(), z.array(z.string())).meta({
      description:
        'User override: IPv6 global-unicast addresses opted into WAN exposure.\nProjected into `HostnameInfo.public` by `update_addresses`, so `public`\nstays the single source of truth for WAN reachability (P2P address\nselection, upstream pinholes). A GUA not in this set is LAN-only.',
    }),
    /**
     * User override: enable these private IPs regardless of their mDNS address.
     */
    lanEnabled: wire(
      z.array(z.tuple([z.string(), z.int()])).optional(),
      z.array(z.tuple([z.string(), z.int()])),
    ).meta({
      description:
        'User override: enable these private IPs regardless of their mDNS address.',
    }),
    /**
     * COMPUTED: NetServiceData::update — all possible addresses for this binding
     */
    available: z.array(HostnameInfo).meta({
      description:
        'COMPUTED: NetServiceData::update — all possible addresses for this binding',
    }),
  })
  .meta({
    id: 'DerivedAddressInfo',
  })
export const ImageConfig = wire(
  ImageConfigRepr,
  z.object({
    source: ImageSource,
    arch: z.array(z.string()),
    emulateMissing: z.boolean(),
    nvidiaContainer: z.boolean(),
  }),
).meta({
  id: 'ImageConfig',
})
export const MetricsFollowResponse = z
  .object({ guid: Guid, metrics: Metrics })
  .meta({
    id: 'MetricsFollowResponse',
  })
export const OsIndex = z.object({ versions: OsVersionInfoMap }).meta({
  id: 'OsIndex',
})
export const PackageVersionInfo = z
  .intersection(
    z.object({
      /**
       * Absent for clients that load icons from the registry's `/icons/` route.
       */
      icon: DataUrl.nullable().optional().meta({
        description:
          "Absent for clients that load icons from the registry's `/icons/` route.",
      }),
      dependencyMetadata: z.record(z.string(), DependencyMetadata),
      sourceVersion: wire(
        z.string().nullable().optional(),
        z.string().nullable(),
      ),
      s9pks: z.array(
        z.tuple([HardwareRequirements, RegistryAsset(MerkleArchiveCommitment)]),
      ),
    }),
    PackageMetadata,
  )
  .meta({
    id: 'PackageVersionInfo',
  })
export const SetBackupProgress = z.object({ progress: Progress }).meta({
  id: 'SetBackupProgress',
})
export const SetInitProgress = z.object({ progress: Progress }).meta({
  id: 'SetInitProgress',
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
export const TaskEntry = z.object({ task: Task, active: z.boolean() }).meta({
  id: 'TaskEntry',
})
export const BindOptions = z
  .object({
    preferredExternalPort: z.int(),
    addSsl: wire(AddSslOptions.nullable().optional(), AddSslOptions.nullable()),
    secure: wire(Security.nullable().optional(), Security.nullable()),
  })
  .meta({
    id: 'BindOptions',
  })
export const CheckDependenciesResult = z
  .object({
    packageId: PackageId,
    title: wire(z.string().nullable().optional(), z.string().nullable()),
    installedVersion: wire(Version.nullable().optional(), Version.nullable()),
    satisfies: z.array(Version),
    isRunning: z.boolean(),
    tasks: z.record(z.string(), TaskEntry),
    healthChecks: z.record(z.string(), NamedHealthCheckResult),
  })
  .meta({
    id: 'CheckDependenciesResult',
  })
export const Dependencies = z.record(z.string(), DepInfo).meta({
  id: 'Dependencies',
})
export const GetPackageResponse = z
  .object({
    categories: z.array(z.string()),
    best: z.record(z.string(), PackageVersionInfo),
    otherVersions: z.record(z.string(), PackageInfoShort).nullable().optional(),
  })
  .meta({
    id: 'GetPackageResponse',
  })
export const GetPackageResponseFull = z
  .object({
    categories: z.array(z.string()),
    best: z.record(z.string(), PackageVersionInfo),
    otherVersions: z.record(z.string(), PackageVersionInfo),
  })
  .meta({
    id: 'GetPackageResponseFull',
  })
export const PackageInfo = z
  .object({
    authorized: z.record(z.string(), z.string()),
    versions: z.record(z.string(), PackageVersionInfo),
    categories: z.array(z.string()),
  })
  .meta({
    id: 'PackageInfo',
  })

/**
 * Contiguous port-range binding (e.g. WebRTC/STUN/TURN RTP ranges).
 *
 * Keyed by `internal_start_port` in [`BindingRanges`]. The range covers
 * `internal_start_port..(internal_start_port + number_of_ports)` and is
 * forwarded through a single iptables rule per protocol per gateway,
 * preserving the destination port number.
 */
export const RangeBindInfo = z
  .object({
    enabled: z.boolean(),
    externalStartPort: z.int(),
    numberOfPorts: z.int(),
    /**
     * Reachable addresses for this range (LAN IPv4 / WAN IPv4 / mDNS /
     * domains) with per-address enabled/disabled overrides — the same model as
     * a single-port binding, but IPv4-only and non-SSL. COMPUTED by
     * `update_addresses`; every entry uses `external_start_port` as its
     * representative port. Public IPs are disabled by default (WAN is opt-in);
     * LAN/mDNS/domains are enabled by default.
     */
    addresses: wire(DerivedAddressInfo.optional(), DerivedAddressInfo).meta({
      description:
        'Reachable addresses for this range (LAN IPv4 / WAN IPv4 / mDNS /\ndomains) with per-address enabled/disabled overrides — the same model as\na single-port binding, but IPv4-only and non-SSL. COMPUTED by\n`update_addresses`; every entry uses `external_start_port` as its\nrepresentative port. Public IPs are disabled by default (WAN is opt-in);\nLAN/mDNS/domains are enabled by default.',
    }),
    /**
     * The single restricted `api` interface exported from this range, if any
     * (`RangeOrigin.export`). Preserved across idempotent re-binds.
     */
    interface: wire(
      RangeServiceInterface.nullable().optional(),
      RangeServiceInterface.nullable(),
    ).meta({
      description:
        'The single restricted `api` interface exported from this range, if any\n(`RangeOrigin.export`). Preserved across idempotent re-binds.',
    }),
  })
  .meta({
    id: 'RangeBindInfo',
    description:
      'Contiguous port-range binding (e.g. WebRTC/STUN/TURN RTP ranges).\n\nKeyed by `internal_start_port` in [`BindingRanges`]. The range covers\n`internal_start_port..(internal_start_port + number_of_ports)` and is\nforwarded through a single iptables rule per protocol per gateway,\npreserving the destination port number.',
  })
export const BindInfo = z
  .object({
    enabled: z.boolean(),
    options: BindOptions,
    net: NetInfo,
    addresses: DerivedAddressInfo,
    /**
     * Service interfaces exported from this binding (`Origin.export`). A single
     * binding (host + internal port) may back several interfaces (e.g. a `ui`
     * and an `api` on the same port), so this is keyed by interface id.
     */
    interfaces: wire(
      z.record(z.string(), ServiceInterface).optional(),
      z.record(z.string(), ServiceInterface),
    ).meta({
      description:
        'Service interfaces exported from this binding (`Origin.export`). A single\nbinding (host + internal port) may back several interfaces (e.g. a `ui`\nand an `api` on the same port), so this is keyed by interface id.',
    }),
  })
  .meta({
    id: 'BindInfo',
  })
export const BindParams = z
  .intersection(z.object({ id: HostId, internalPort: z.int() }), BindOptions)
  .meta({
    id: 'BindParams',
  })
export const BindingRanges = z.record(z.string(), RangeBindInfo).meta({
  id: 'BindingRanges',
})
export const Manifest = z
  .intersection(
    z.object({
      id: PackageId,
      version: Version,
      canMigrateTo: z.string(),
      canMigrateFrom: z.string(),
      images: z.record(z.string(), ImageConfig),
      volumes: z.array(VolumeId),
      dependencies: wire(Dependencies.optional(), Dependencies),
      hardwareRequirements: wire(
        HardwareRequirements.optional(),
        HardwareRequirements,
      ),
    }),
    PackageMetadata,
  )
  .meta({
    id: 'Manifest',
  })
export const PackageIndex = z
  .object({
    categories: z.record(z.string(), Category),
    packages: z.record(z.string(), PackageInfo),
  })
  .meta({
    id: 'PackageIndex',
  })
export const Bindings = z.record(z.string(), BindInfo).meta({
  id: 'Bindings',
})
export const FullIndex = z
  .object({
    name: wire(z.string().nullable().optional(), z.string().nullable()),
    icon: wire(DataUrl.nullable().optional(), DataUrl.nullable()),
    description: wire(
      LocaleString.nullable().optional(),
      LocaleString.nullable(),
    ),
    package: PackageIndex,
    os: OsIndex,
    signers: z.record(z.string(), SignerInfo),
  })
  .meta({
    id: 'FullIndex',
  })
export const InstalledState = z.object({ manifest: Manifest }).meta({
  id: 'InstalledState',
})
export const InstallingInfo = z
  .object({ newManifest: Manifest, progress: FullProgress })
  .meta({
    id: 'InstallingInfo',
  })
export const Host = z
  .object({
    bindings: Bindings,
    bindingRanges: wire(BindingRanges.optional(), BindingRanges),
    publicDomains: z.record(z.string(), PublicDomainConfig),
    privateDomains: z.record(z.string(), z.array(GatewayId)),
    /**
     * COMPUTED: port forwarding rules needed on gateways for public addresses to work.
     */
    portForwards: wire(
      z.array(PortForward).optional(),
      z.array(PortForward),
    ).meta({
      description:
        'COMPUTED: port forwarding rules needed on gateways for public addresses to work.',
    }),
  })
  .meta({
    id: 'Host',
  })
export const InstallingState = z
  .object({ installingInfo: InstallingInfo })
  .meta({
    id: 'InstallingState',
  })
export const UpdatingState = z
  .object({
    manifest: Manifest,
    s9pk: z.string(),
    installingInfo: InstallingInfo,
  })
  .meta({
    id: 'UpdatingState',
  })
export const Hosts = z.record(z.string(), Host).meta({
  id: 'Hosts',
})
export const NetworkInfo = z
  .object({
    wifi: WifiInfo,
    host: Host,
    gateways: wire(
      z.record(z.string(), NetworkInterfaceInfo).optional(),
      z.record(z.string(), NetworkInterfaceInfo),
    ),
    acme: wire(
      z.record(z.string(), AcmeSettings).optional(),
      z.record(z.string(), AcmeSettings),
    ),
    dns: wire(DnsSettings.optional(), DnsSettings),
    defaultOutbound: wire(
      z.string().nullable().optional(),
      z.string().nullable(),
    ),
    passthroughs: wire(
      z.array(PassthroughInfo).optional(),
      z.array(PassthroughInfo),
    ),
  })
  .meta({
    id: 'NetworkInfo',
  })
export const PackageState = z
  .union([
    z.intersection(
      z.object({ state: z.literal('installing') }),
      InstallingState,
    ),
    z.intersection(
      z.object({ state: z.literal('restoring') }),
      InstallingState,
    ),
    z.intersection(z.object({ state: z.literal('updating') }), UpdatingState),
    z.intersection(z.object({ state: z.literal('installed') }), InstalledState),
    z.intersection(z.object({ state: z.literal('removing') }), InstalledState),
  ])
  .meta({
    id: 'PackageState',
  })
export const PackageDataEntry = z
  .object({
    stateInfo: PackageState,
    s9pk: z.string(),
    statusInfo: StatusInfo,
    registry: wire(z.string().nullable().optional(), z.string().nullable()),
    developerKey: z.string(),
    lastBackup: wire(z.string().nullable().optional(), z.string().nullable()),
    currentDependencies: CurrentDependencies,
    actions: z.record(z.string(), ActionMetadata),
    tasks: z.record(z.string(), TaskEntry),
    hosts: Hosts,
    storeExposedDependents: z.array(z.string()),
    outboundGateway: wire(
      z.string().nullable().optional(),
      z.string().nullable(),
    ),
    plugin: wire(PackagePlugin.optional(), PackagePlugin),
  })
  .meta({
    id: 'PackageDataEntry',
  })
export const ServerInfo = z
  .object({
    id: z.string(),
    hostname: z.string(),
    version: z.string(),
    packageVersionCompat: z.string(),
    postInitMigrationTodos: z.record(z.string(), z.unknown()),
    latestMigrationRevision: wire(z.int().optional(), z.int()),
    lastBackup: wire(z.string().nullable().optional(), z.string().nullable()),
    network: NetworkInfo,
    statusInfo: wire(ServerStatus.optional(), ServerStatus),
    unreadNotificationCount: z.number(),
    pubkey: z.string(),
    caFingerprint: z.string(),
    ntpSynced: wire(z.boolean().optional(), z.boolean()),
    zram: wire(z.boolean().optional(), z.boolean()),
    governor: wire(Governor.nullable().optional(), Governor.nullable()),
    epp: wire(Epp.nullable().optional(), Epp.nullable()),
    smtp: wire(SmtpValue.nullable().optional(), SmtpValue.nullable()),
    echoipUrls: wire(z.array(z.string()).optional(), z.array(z.string())),
    ram: z.number(),
    devices: z.array(LshwDevice),
    kiosk: wire(z.boolean().nullable().optional(), z.boolean().nullable()),
    language: wire(z.string().nullable().optional(), z.string().nullable()),
    keyboard: wire(
      KeyboardOptions.nullable().optional(),
      KeyboardOptions.nullable(),
    ),
  })
  .meta({
    id: 'ServerInfo',
  })
export const AllPackageData = z.record(z.string(), PackageDataEntry).meta({
  id: 'AllPackageData',
})
export const Public = z
  .object({
    serverInfo: ServerInfo,
    packageData: AllPackageData,
    ui: z.unknown(),
  })
  .meta({
    id: 'Public',
  })
