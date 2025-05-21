import { ExtendedVersion, VersionRange } from "../../../base/lib/exver"
import * as T from "../../../base/lib/types"

export type UninitFn = (
  effects: T.Effects,
  target: VersionRange | ExtendedVersion | null,
) => Promise<void | null | undefined>

export interface UninitScript {
  uninit(
    effects: T.Effects,
    target: VersionRange | ExtendedVersion | null,
  ): Promise<void>
}

export function setupUninit(
  ...uninits: (UninitScript | UninitFn)[]
): T.ExpectedExports.uninit {
  return async (opts) => {
    for (const uninit of uninits) {
      if ("uninit" in uninit) await uninit.uninit(opts.effects, opts.target)
      else await uninit(opts.effects, opts.target)
    }
  }
}
