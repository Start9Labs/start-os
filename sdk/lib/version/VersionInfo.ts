import { ValidateExVer } from "../exver"
import * as T from "../types"

export const IMPOSSIBLE = Symbol("IMPOSSIBLE")

export type VersionOptions<Version extends string> = {
  /** The exver-compliant version number */
  version: Version & ValidateExVer<Version>
  /** The release notes for this version */
  releaseNotes: string
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
    other?: Record<string, (opts: { effects: T.Effects }) => Promise<void>>
  }
}

export class VersionInfo<Version extends string> {
  private _version: null | Version = null
  private constructor(
    readonly options: VersionOptions<Version> & { satisfies: string[] },
  ) {}
  /**
   * @description Use this function to define a new version of the service. By convention, each version should receive its own file.
   * @property {string} version
   * @property {string} releaseNotes
   * @property {object} migrations
   * @returns A VersionInfo class instance that is exported, then imported into versions/index.ts.
   */
  static of<Version extends string>(options: VersionOptions<Version>) {
    return new VersionInfo<Version>({ ...options, satisfies: [] })
  }
  /** Specify a version that this version is 100% backwards compatible to */
  satisfies<V extends string>(
    version: V & ValidateExVer<V>,
  ): VersionInfo<Version> {
    return new VersionInfo({
      ...this.options,
      satisfies: [...this.options.satisfies, version],
    })
  }
}

function __type_tests() {
  const version: VersionInfo<"1.0.0:0"> = VersionInfo.of({
    version: "1.0.0:0",
    releaseNotes: "",
    migrations: {},
  })
    .satisfies("#other:1.0.0:0")
    .satisfies("#other:2.0.0:0")
    // @ts-expect-error
    .satisfies("#other:2.f.0:0")

  let a: VersionInfo<"1.0.0:0"> = version
  // @ts-expect-error
  let b: VersionInfo<"1.0.0:3"> = version

  VersionInfo.of({
    // @ts-expect-error
    version: "test",
    releaseNotes: "",
    migrations: {},
  })
  VersionInfo.of({
    // @ts-expect-error
    version: "test" as string,
    releaseNotes: "",
    migrations: {},
  })
}
