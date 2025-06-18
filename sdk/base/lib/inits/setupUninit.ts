import { ExtendedVersion, VersionRange } from "../../../base/lib/exver"
import * as T from "../../../base/lib/types"

export type UninitFn = (
  effects: T.Effects,
  /**
   * @description the target version to prepare for
   *
   * on update: the canMigrateFrom of the new package
   * on uninstall: null
   * on shutdown: the current version
   */
  target: VersionRange | ExtendedVersion | null,
) => Promise<void | null | undefined>

export interface UninitScript {
  uninit(
    effects: T.Effects,
    /**
     * @description the target version to prepare for
     *
     * on update: the canMigrateFrom of the new package
     * on uninstall: null
     * on shutdown: the current version
     */
    target: VersionRange | ExtendedVersion | null,
  ): Promise<void>
}

export type UninitScriptOrFn = UninitScript | UninitFn

export function setupUninit(
  ...uninits: UninitScriptOrFn[]
): T.ExpectedExports.uninit {
  return async (opts) => {
    for (const uninit of uninits) {
      if ("uninit" in uninit) await uninit.uninit(opts.effects, opts.target)
      else await uninit(opts.effects, opts.target)
    }
  }
}

export function setupOnUninit(onUninit: UninitScriptOrFn): UninitScript {
  return "uninit" in onUninit
    ? onUninit
    : {
        uninit: async (effects, target) => {
          await onUninit(effects, target)
        },
      }
}
