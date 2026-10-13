import { z } from 'zod'
const wire = <A extends z.ZodType, B extends z.ZodType>(input: A, output: B) =>
  z.codec(input, output, { decode: v => v as never, encode: v => v as never })
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
export const AnySignature = z.string().meta({
  id: 'AnySignature',
})
export const AnyVerifyingKey = z.string().meta({
  id: 'AnyVerifyingKey',
})
export const ApplyParams = z.object({ expr: z.string() }).meta({
  id: 'ApplyParams',
})
export const Blake3Commitment = z
  .object({ hash: z.string(), size: z.number() })
  .meta({
    id: 'Blake3Commitment',
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
export const GitHash = z.string().meta({
  id: 'GitHash',
})
export const Guid = z.string().meta({
  id: 'Guid',
})
export const IoFormat = z
  .enum(['json', 'json-pretty', 'yaml', 'cbor', 'toml', 'toml-pretty'])
  .meta({
    id: 'IoFormat',
  })
export const ListVersionSignersParams = z.object({ version: z.string() }).meta({
  id: 'ListVersionSignersParams',
})
export const LocaleString = z
  .union([z.string(), z.record(z.string(), z.string())])
  .meta({
    id: 'LocaleString',
  })
export const MerkleArchiveCommitment = z
  .object({ rootSighash: z.string(), rootMaxsize: z.number() })
  .meta({
    id: 'MerkleArchiveCommitment',
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
export const PluginId = z.enum(['url-v0']).meta({
  id: 'PluginId',
})
export const PreDownloadAlertWhen = z
  .object({ sourceVersion: z.string() })
  .meta({
    id: 'PreDownloadAlertWhen',
  })

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
export const RemoveVersionParams = z.object({ version: z.string() }).meta({
  id: 'RemoveVersionParams',
})
export const SetNameParams = z.object({ name: z.string() }).meta({
  id: 'SetNameParams',
})
export const UsersResponse = z
  .object({ uniqueServers: z.int(), totalCheckins: z.int() })
  .meta({
    id: 'UsersResponse',
  })
export const Version = z.string().meta({
  id: 'Version',
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
export const Category = z.object({ name: LocaleString }).meta({
  id: 'Category',
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
export const Description = z
  .object({ short: LocaleString, long: LocaleString })
  .meta({
    id: 'Description',
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
export const ListPackageSignersParams = z.object({ id: PackageId }).meta({
  id: 'ListPackageSignersParams',
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
export const PackageInfoShort = z.object({ releaseNotes: LocaleString }).meta({
  id: 'PackageInfoShort',
})
export const PreDownloadAlert = z
  .object({ message: LocaleString, when: PreDownloadAlertWhen })
  .meta({
    id: 'PreDownloadAlert',
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
export const SetDescriptionParams = z
  .object({ description: LocaleString.nullable() })
  .meta({
    id: 'SetDescriptionParams',
  })
export const SetIconParams = z.object({ icon: DataUrl.nullable() }).meta({
  id: 'SetIconParams',
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
export const SubscribeRes = z.object({ dump: Dump, guid: Guid }).meta({
  id: 'SubscribeRes',
})
export const VersionSignerParams = z
  .object({ version: z.string(), signer: Guid })
  .meta({
    id: 'VersionSignerParams',
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
export const GetPackageResult = z
  .union([
    GetPackageResponseFull,
    GetPackageResponse,
    z.record(z.string(), GetPackageResponseFull),
    z.record(z.string(), GetPackageResponse),
  ])
  .meta({
    id: 'GetPackageResult',
  })
export const PackageIndex = z
  .object({
    categories: z.record(z.string(), Category),
    packages: z.record(z.string(), PackageInfo),
  })
  .meta({
    id: 'PackageIndex',
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
export const Api = {
  _PARAMS: z.object({}) as z.ZodType<{}, {}>,
  _CHILDREN: {
    admin: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        add: { _PARAMS: AddAdminParams, _RETURN: z.null() },
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
          _RETURN: z.record(z.string(), SignerInfo),
        },
        remove: { _PARAMS: RemoveAdminParams, _RETURN: z.null() },
        signer: {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _CHILDREN: {
            add: { _PARAMS: SignerInfo, _RETURN: Guid },
            edit: { _PARAMS: EditSignerParams, _RETURN: z.null() },
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
              _RETURN: z.record(z.string(), SignerInfo),
            },
            remove: { _PARAMS: RemoveSignerParams, _RETURN: z.null() },
          },
        },
      },
    },
    db: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        apply: { _PARAMS: ApplyWithPathParams, _RETURN: z.null() },
        dump: { _PARAMS: DumpParams, _RETURN: Dump },
        subscribe: { _PARAMS: DumpParams, _RETURN: SubscribeRes },
      },
    },
    index: {
      _PARAMS: z.intersection(
        z.intersection(
          z.object({
            format: wire(IoFormat.nullable().optional(), IoFormat.nullable()),
          }),
          z.object({}) as z.ZodType<{}, {}>,
        ),
        z.object({}) as z.ZodType<{}, {}>,
      ),
      _RETURN: FullIndex,
    },
    info: {
      _PARAMS: z.intersection(
        z.object({
          format: wire(IoFormat.nullable().optional(), IoFormat.nullable()),
        }),
        z.object({}) as z.ZodType<{}, {}>,
      ),
      _RETURN: RegistryInfo,
      _CHILDREN: {
        'set-description': { _PARAMS: SetDescriptionParams, _RETURN: z.null() },
        'set-icon': { _PARAMS: SetIconParams, _RETURN: z.null() },
        'set-name': { _PARAMS: SetNameParams, _RETURN: z.null() },
      },
    },
    metrics: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        downloads: {
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
            GetDownloadsParams,
          ),
          _RETURN: DownloadsResponse,
        },
        summary: {
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
          _RETURN: MetricsSummary,
        },
        users: {
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
            GetUsersParams,
          ),
          _RETURN: UsersResponse,
        },
      },
    },
    os: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        asset: {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _CHILDREN: {
            add: {
              _PARAMS: z.object({}) as z.ZodType<{}, {}>,
              _CHILDREN: {
                img: { _PARAMS: AddAssetParams, _RETURN: z.null() },
                iso: { _PARAMS: AddAssetParams, _RETURN: z.null() },
                squashfs: { _PARAMS: AddAssetParams, _RETURN: z.null() },
              },
            },
            get: {
              _PARAMS: z.object({}) as z.ZodType<{}, {}>,
              _CHILDREN: {
                img: {
                  _PARAMS: GetOsAssetParams,
                  _RETURN: RegistryAsset(Blake3Commitment),
                },
                iso: {
                  _PARAMS: GetOsAssetParams,
                  _RETURN: RegistryAsset(Blake3Commitment),
                },
                squashfs: {
                  _PARAMS: GetOsAssetParams,
                  _RETURN: RegistryAsset(Blake3Commitment),
                },
              },
            },
            remove: {
              _PARAMS: z.object({}) as z.ZodType<{}, {}>,
              _CHILDREN: {
                img: { _PARAMS: RemoveAssetParams, _RETURN: z.boolean() },
                iso: { _PARAMS: RemoveAssetParams, _RETURN: z.boolean() },
                squashfs: { _PARAMS: RemoveAssetParams, _RETURN: z.boolean() },
              },
            },
            sign: {
              _PARAMS: z.object({}) as z.ZodType<{}, {}>,
              _CHILDREN: {
                img: { _PARAMS: SignAssetParams, _RETURN: z.null() },
                iso: { _PARAMS: SignAssetParams, _RETURN: z.null() },
                squashfs: { _PARAMS: SignAssetParams, _RETURN: z.null() },
              },
            },
          },
        },
        index: {
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
          _RETURN: OsIndex,
        },
        version: {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _CHILDREN: {
            add: { _PARAMS: AddVersionParams, _RETURN: z.null() },
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
                GetOsVersionParams,
              ),
              _RETURN: OsVersionInfoMap,
            },
            remove: { _PARAMS: RemoveVersionParams, _RETURN: z.null() },
            signer: {
              _PARAMS: z.object({}) as z.ZodType<{}, {}>,
              _CHILDREN: {
                add: { _PARAMS: VersionSignerParams, _RETURN: z.null() },
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
                    ListVersionSignersParams,
                  ),
                  _RETURN: z.record(z.string(), SignerInfo),
                },
                remove: { _PARAMS: VersionSignerParams, _RETURN: z.null() },
              },
            },
          },
        },
      },
    },
    package: {
      _PARAMS: z.object({}) as z.ZodType<{}, {}>,
      _CHILDREN: {
        add: { _PARAMS: AddPackageParams, _RETURN: z.null() },
        'add-mirror': { _PARAMS: AddMirrorParams, _RETURN: z.null() },
        category: {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _CHILDREN: {
            add: { _PARAMS: AddCategoryParams, _RETURN: z.null() },
            'add-package': {
              _PARAMS: AddPackageToCategoryParams,
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
              _RETURN: z.record(z.string(), Category),
            },
            remove: { _PARAMS: RemoveCategoryParams, _RETURN: z.null() },
            'remove-package': {
              _PARAMS: RemovePackageFromCategoryParams,
              _RETURN: z.null(),
            },
          },
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
            GetPackageParams,
          ),
          _RETURN: GetPackageResult,
        },
        index: {
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
          _RETURN: PackageIndex,
        },
        remove: { _PARAMS: RemovePackageParams, _RETURN: z.boolean() },
        'remove-mirror': { _PARAMS: RemoveMirrorParams, _RETURN: z.null() },
        signer: {
          _PARAMS: z.object({}) as z.ZodType<{}, {}>,
          _CHILDREN: {
            add: { _PARAMS: AddPackageSignerParams, _RETURN: z.null() },
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
                ListPackageSignersParams,
              ),
              _RETURN: z.record(z.string(), z.tuple([SignerInfo, z.string()])),
            },
            remove: { _PARAMS: RemovePackageSignerParams, _RETURN: z.null() },
          },
        },
      },
    },
  },
}
