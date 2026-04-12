import { ValidateExVer } from '../../../base/lib/exver'
import * as T from '../../../base/lib/types'

/**
 * Sentinel value indicating that a migration in a given direction is not possible.
 * Use this for `migrations.up` or `migrations.down` to prevent migration.
 */
export const IMPOSSIBLE: unique symbol = Symbol('IMPOSSIBLE')

/**
 * Configuration options for a single service version definition.
 *
 * @typeParam Version - The string literal exver version number
 */
export type VersionOptions<Version extends string> = {
  /** The exver-compliant version number */
  version: Version & ValidateExVer<Version>
  /** The release notes for this version */
  releaseNotes: T.LocaleString
  /** Data migrations for this version */
  migrations: {
    /**
     * A migration from the previous version. Leave empty to indicate no migration is necessary.
     * Set to `IMPOSSIBLE` to indicate migrating from the previous version is not possible.
     */
    up?: ((opts: { effects: T.Effects }) => Promise<void>) | typeof IMPOSSIBLE
    /**
     * A migration to the previous version. Leave blank to indicate no migration is necessary.
     * Set to `IMPOSSIBLE` to indicate downgrades are prohibited
     */
    down?: ((opts: { effects: T.Effects }) => Promise<void>) | typeof IMPOSSIBLE
    /**
     * Additional migrations, such as fast-forward migrations, or migrations from other flavors.
     */
    other?: Record<
      string,
      {
        up?: (opts: { effects: T.Effects }) => Promise<void>
        down?: (opts: { effects: T.Effects }) => Promise<void>
      }
    >
  }
}

/**
 * Represents a single version of the service, including its release notes,
 * migration scripts, and backwards-compatibility declarations.
 *
 * By convention, each version gets its own file (e.g. `versions/v1_0_0.ts`).
 *
 * @typeParam Version - The string literal exver version number
 */
export interface VersionInfo<Version extends string> {
  /** Brand to preserve type parameter distinction across structural checks */
  readonly _version: Version
  readonly options: VersionOptions<Version> & { satisfies: string[] }
  /** Specify a version that this version is 100% backwards compatible to */
  satisfies<V extends string>(
    version: V & ValidateExVer<V>,
  ): VersionInfo<Version>
}

class VersionInfoImpl<Version extends string> implements VersionInfo<Version> {
  readonly _version: Version = null as any as Version
  constructor(
    readonly options: VersionOptions<Version> & { satisfies: string[] },
  ) {}
  satisfies<V extends string>(
    version: V & ValidateExVer<V>,
  ): VersionInfo<Version> {
    return new VersionInfoImpl({
      ...this.options,
      satisfies: [...this.options.satisfies, version],
    })
  }
}

/**
 * @description Use this function to define a new version of the service. By convention, each version should receive its own file.
 * @property {string} version
 * @property {string} releaseNotes
 * @property {object} migrations
 * @returns A VersionInfo instance that is exported, then imported into versions/index.ts.
 */
export const VersionInfo = {
  of<Version extends string>(
    options: VersionOptions<Version>,
  ): VersionInfo<Version> {
    return new VersionInfoImpl<Version>({ ...options, satisfies: [] })
  },
}

function __type_tests() {
  const version: VersionInfo<'1.0.0:0'> = VersionInfo.of({
    version: '1.0.0:0',
    releaseNotes: '',
    migrations: {},
  })
    .satisfies('#other:1.0.0:0')
    .satisfies('#other:2.0.0:0')
    // @ts-expect-error
    .satisfies('#other:2.f.0:0')

  let a: VersionInfo<'1.0.0:0'> = version
  // @ts-expect-error
  let b: VersionInfo<'1.0.0:3'> = version

  VersionInfo.of({
    // @ts-expect-error
    version: 'test',
    releaseNotes: '',
    migrations: {},
  })
  VersionInfo.of({
    // @ts-expect-error
    version: 'test' as string,
    releaseNotes: '',
    migrations: {},
  })
}
