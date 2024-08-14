import { ExtendedVersion, ValidateExVer } from "../exver"
import * as T from "../types"

export const IMPOSSIBLE = Symbol("IMPOSSIBLE")

export type VersionOptions<Version extends string> = {
  /** The version being described */
  version: Version & ValidateExVer<Version>
  /** The release notes for this version */
  releaseNotes: string
  /** The versions that this version is 100% backwards compatible to */
  satisfies: string[]
  /** Data migrations for this version */
  migrations: {
    /**
     * A migration from the previous version
     *    Leave blank to indicate no migration is necessary
     *    Set to `IMPOSSIBLE` to indicate migrating from the previous version is not possible
     */
    up?: ((opts: { effects: T.Effects }) => Promise<void>) | typeof IMPOSSIBLE
    /**
     * A migration to the previous version
     *    Leave blank to indicate no migration is necessary
     *    Set to `IMPOSSIBLE` to indicate downgrades are prohibited
     */
    down?: ((opts: { effects: T.Effects }) => Promise<void>) | typeof IMPOSSIBLE
    /**
     * Additional migrations, such as fast-forward migrations, or migrations from other flavors
     */
    other?: Record<string, (opts: { effects: T.Effects }) => Promise<void>>
  }
}

export class VersionInfo<Version extends string> {
  constructor(readonly options: VersionOptions<Version>) {}
  static of<Version extends string>(options: VersionOptions<Version>) {
    return new VersionInfo<Version>(options)
  }
}
