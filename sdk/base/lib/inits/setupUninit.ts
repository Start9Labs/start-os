import { ExtendedVersion, VersionRange } from '../../../base/lib/exver'
import * as T from '../../../base/lib/types'

/**
 * Function signature for an uninit handler that runs during service shutdown/uninstall.
 */
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

/** Object form of an uninit handler — implements an `uninit()` method. */
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

/** Either a {@link UninitScript} object or a {@link UninitFn} function. */
export type UninitScriptOrFn = UninitScript | UninitFn

/**
 * Composes multiple uninit handlers into a single `ExpectedExports.uninit`-compatible function.
 * Handlers are executed sequentially in the order provided.
 *
 * @param uninits - One or more uninit handlers to compose
 */
export function setupUninit(
  ...uninits: UninitScriptOrFn[]
): T.ExpectedExports.uninit {
  return async (opts) => {
    for (const uninit of uninits) {
      if ('uninit' in uninit) await uninit.uninit(opts.effects, opts.target)
      else await uninit(opts.effects, opts.target)
    }
  }
}

/** Normalizes a {@link UninitScriptOrFn} into a {@link UninitScript} object. */
export function setupOnUninit(onUninit: UninitScriptOrFn): UninitScript {
  return 'uninit' in onUninit
    ? onUninit
    : {
        uninit: async (effects, target) => {
          await onUninit(effects, target)
        },
      }
}
