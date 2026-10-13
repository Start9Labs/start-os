import { z } from 'zod'
const wire = <A extends z.ZodType, B extends z.ZodType>(input: A, output: B) =>
  z.codec(input, output, { decode: v => v as never, encode: v => v as never })
export const AcmeProvider = z.string().meta({
  id: 'AcmeProvider',
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
export const Algorithm = z.enum(['ecdsa', 'ed25519']).meta({
  id: 'Algorithm',
})
export const AllowedStatuses = z
  .enum(['only-running', 'only-stopped', 'any'])
  .meta({
    id: 'AllowedStatuses',
  })
export const BasicCredential = z
  .object({ username: z.string(), password: z.string() })
  .meta({
    id: 'BasicCredential',
  })
export const BuildArg = z
  .union([z.string(), z.object({ env: z.string() })])
  .meta({
    id: 'BuildArg',
  })
export const CallbackId = z.number().meta({
  id: 'CallbackId',
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
export const EchoParams = z.object({ message: z.string() }).meta({
  id: 'EchoParams',
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
export const GatewayId = z.string().meta({
  id: 'GatewayId',
})
export const GitHash = z.string().meta({
  id: 'GitHash',
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
export const IoFormat = z
  .enum(['json', 'json-pretty', 'yaml', 'cbor', 'toml', 'toml-pretty'])
  .meta({
    id: 'IoFormat',
  })
export const LocaleString = z
  .union([z.string(), z.record(z.string(), z.string())])
  .meta({
    id: 'LocaleString',
  })
export const MaybeUtf8String = z.union([z.string(), z.array(z.number())]).meta({
  id: 'MaybeUtf8String',
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
export const NotificationLevel = z
  .enum(['success', 'info', 'warning', 'error'])
  .meta({
    id: 'NotificationLevel',
  })
export const PackageId = z.string().meta({
  id: 'PackageId',
})
export const PathOrUrl = z.string().meta({
  id: 'PathOrUrl',
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
export const ReplayId = z.string().meta({
  id: 'ReplayId',
})
export const Security = z.object({ ssl: z.boolean() }).meta({
  id: 'Security',
})
export const ServiceInterfaceId = z.string().meta({
  id: 'ServiceInterfaceId',
})
export const ServiceInterfaceType = z.enum(['ui', 'p2p', 'api']).meta({
  id: 'ServiceInterfaceType',
})
export const SetDataVersionParams = z
  .object({
    version: wire(z.string().nullable().optional(), z.string().nullable()),
  })
  .meta({
    id: 'SetDataVersionParams',
  })
export const SetMainStatusStatus = z.enum(['running', 'stopped']).meta({
  id: 'SetMainStatusStatus',
})
export const SmtpSecurity = z.enum(['starttls', 'tls']).meta({
  id: 'SmtpSecurity',
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
export const Version = z.string().meta({
  id: 'Version',
})
export const VolumeId = z.string().meta({
  id: 'VolumeId',
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
export const ClearActionsParams = z.object({ except: z.array(ActionId) }).meta({
  id: 'ClearActionsParams',
})
export const ClearServiceInterfacesParams = z
  .object({ except: z.array(ServiceInterfaceId) })
  .meta({
    id: 'ClearServiceInterfacesParams',
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
export const LegacyAlpnInfo = z
  .union([
    z.literal('reflect'),
    z.object({ specified: z.array(MaybeUtf8String) }),
  ])
  .meta({
    id: 'LegacyAlpnInfo',
  })
export const ListServiceInterfacesParams = z
  .object({
    packageId: wire(PackageId.nullable().optional(), PackageId.nullable()),
    callback: wire(CallbackId.nullable().optional(), CallbackId.nullable()),
  })
  .meta({
    id: 'ListServiceInterfacesParams',
  })
export const Metadata = z
  .object({ title: LocaleString, icon: PathOrUrl })
  .meta({
    id: 'Metadata',
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
export const RetireBindingParams = z
  .object({ id: HostId, internalPort: z.int() })
  .meta({
    id: 'RetireBindingParams',
  })
export const RetireHostParams = z.object({ id: HostId }).meta({
  id: 'RetireHostParams',
})
export const SetMainStatus = z.object({ status: SetMainStatusStatus }).meta({
  id: 'SetMainStatus',
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
export const UrlPluginRegisterParams = z
  .object({ tableAction: ActionId })
  .meta({
    id: 'UrlPluginRegisterParams',
  })
export const ActionResult = z
  .union([
    z.intersection(z.object({ version: z.literal('0') }), ActionResultV0),
    z.intersection(z.object({ version: z.literal('1') }), ActionResultV1),
  ])
  .meta({
    id: 'ActionResult',
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
export const MetadataSrc = z
  .union([
    z.object({ metadata: Metadata }),
    z.object({ s9pk: PathOrUrl.nullable() }),
  ])
  .meta({
    id: 'MetadataSrc',
  })
export const MountParams = z
  .object({ location: z.string(), target: MountTarget })
  .meta({
    id: 'MountParams',
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
export const SetBackupProgress = z.object({ progress: Progress }).meta({
  id: 'SetBackupProgress',
})
export const SetInitProgress = z.object({ progress: Progress }).meta({
  id: 'SetInitProgress',
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
export const Bindings = z.record(z.string(), BindInfo).meta({
  id: 'Bindings',
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
export const Api = {
  _PARAMS: z.object({}) as z.ZodType<{}, {}>,
  _CHILDREN: {
    action: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        clear: { _PARAMS: ClearActionsParams, _RETURN: z.null() },
        'clear-tasks': { _PARAMS: ClearTasksParams, _RETURN: z.null() },
        'create-task': { _PARAMS: CreateTaskParams, _RETURN: z.null() },
        export: { _PARAMS: ExportActionParams, _RETURN: z.null() },
        'get-input': {
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
            GetActionInputParams,
          ),
          _RETURN: ActionInput.nullable(),
        },
        run: {
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
            EffectsRunActionParams,
          ),
          _RETURN: ActionResult.nullable(),
        },
      },
    },
    bind: { _PARAMS: BindParams, _RETURN: z.null() },
    'bind-range': { _PARAMS: BindRangeParams, _RETURN: z.null() },
    'check-dependencies': {
      _PARAMS: CheckDependenciesParam,
      _RETURN: z.array(CheckDependenciesResult),
    },
    'clear-bindings': { _PARAMS: ClearBindingsParams, _RETURN: z.null() },
    'clear-callbacks': { _PARAMS: ClearCallbacksParams, _RETURN: z.null() },
    'clear-service-interfaces': {
      _PARAMS: ClearServiceInterfacesParams,
      _RETURN: z.null(),
    },
    echo: { _PARAMS: EchoParams, _RETURN: z.string() },
    'export-range-service-interface': {
      _PARAMS: ExportRangeServiceInterfaceParams,
      _RETURN: z.null(),
    },
    'export-service-interface': {
      _PARAMS: ExportServiceInterfaceParams,
      _RETURN: z.null(),
    },
    'get-container-ip': {
      _PARAMS: GetContainerIpParams,
      _RETURN: z.string().nullable(),
    },
    'get-data-version': {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _RETURN: z.string().nullable(),
    },
    'get-dependencies': {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _RETURN: z.array(DependencyRequirement),
    },
    'get-host-info': { _PARAMS: GetHostInfoParams, _RETURN: Host.nullable() },
    'get-installed-packages': {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _RETURN: z.array(PackageId),
    },
    'get-os-ip': {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _RETURN: z.string(),
    },
    'get-outbound-gateway': {
      _PARAMS: GetOutboundGatewayParams,
      _RETURN: GatewayId,
    },
    'get-service-interface': {
      _PARAMS: GetServiceInterfaceParams,
      _RETURN: ServiceInterface.nullable(),
    },
    'get-service-manifest': {
      _PARAMS: GetServiceManifestParams,
      _RETURN: Manifest.nullable(),
    },
    'get-service-port-forward': {
      _PARAMS: GetServicePortForwardParams,
      _RETURN: NetInfo.nullable(),
    },
    'get-ssl-certificate': {
      _PARAMS: GetSslCertificateParams,
      _RETURN: z.array(z.string()),
    },
    'get-ssl-key': { _PARAMS: GetSslKeyParams, _RETURN: z.string() },
    'get-status': { _PARAMS: GetStatusParams, _RETURN: StatusInfo.nullable() },
    'get-system-smtp': {
      _PARAMS: GetSystemSmtpParams,
      _RETURN: SmtpValue.nullable(),
    },
    'git-info': {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _RETURN: z.string(),
    },
    'list-service-interfaces': {
      _PARAMS: ListServiceInterfacesParams,
      _RETURN: z.record(z.string(), ServiceInterface),
    },
    mount: { _PARAMS: MountParams, _RETURN: z.null() },
    notification: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        create: { _PARAMS: CreateNotificationParams, _RETURN: z.null() },
      },
    },
    plugin: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        url: {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _CHILDREN: {
            'clear-urls': {
              _PARAMS: UrlPluginClearUrlsParams,
              _RETURN: z.null(),
            },
            'export-url': {
              _PARAMS: UrlPluginExportUrlParams,
              _RETURN: z.null(),
            },
            register: { _PARAMS: UrlPluginRegisterParams, _RETURN: z.null() },
          },
        },
      },
    },
    rebuild: { _PARAMS: z.object({}) as z.ZodType<{}, {}>, _RETURN: z.null() },
    restart: { _PARAMS: z.object({}) as z.ZodType<{}, {}>, _RETURN: z.null() },
    'retire-binding': { _PARAMS: RetireBindingParams, _RETURN: z.boolean() },
    'retire-host': { _PARAMS: RetireHostParams, _RETURN: z.boolean() },
    'set-backup-progress': { _PARAMS: SetBackupProgress, _RETURN: z.null() },
    'set-data-version': { _PARAMS: SetDataVersionParams, _RETURN: z.null() },
    'set-dependencies': { _PARAMS: SetDependenciesParams, _RETURN: z.null() },
    'set-health': { _PARAMS: SetHealth, _RETURN: z.null() },
    'set-init-progress': { _PARAMS: SetInitProgress, _RETURN: z.null() },
    'set-main-status': { _PARAMS: SetMainStatus, _RETURN: z.null() },
    shutdown: { _PARAMS: z.object({}) as z.ZodType<{}, {}>, _RETURN: z.null() },
    subcontainer: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        'create-fs': {
          _PARAMS: CreateSubcontainerFsParams,
          _RETURN: z.tuple([z.string(), Guid]),
        },
        'destroy-fs': {
          _PARAMS: DestroySubcontainerFsParams,
          _RETURN: z.null(),
        },
      },
    },
  },
}
