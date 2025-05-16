import { VersionRange } from "../../../base/lib/exver"
import * as T from "../../../base/lib/types"
import { once } from "../util"

export type InitKind = "install" | "update" | "restore" | null

export type InitFn<Kind extends InitKind = InitKind> = (
  effects: T.Effects,
  kind: Kind,
) => Promise<void | null | undefined>

export interface InitScript<Kind extends InitKind = InitKind> {
  init(effects: T.Effects, kind: Kind): Promise<void>
}

export type InitScriptOrFn<Kind extends InitKind = InitKind> =
  | InitScript<Kind>
  | InitFn<Kind>

export function setupInit(...inits: InitScriptOrFn[]): T.ExpectedExports.init {
  return async (opts) => {
    for (const idx in inits) {
      const init = inits[idx]
      const fn = async (effects: T.Effects, kind: InitKind) => {
        let res: (value?: undefined) => void = () => {}
        const complete = new Promise((resolve) => {
          res = resolve
        })
        const e: T.Effects = effects.child(`init_${idx}`)
        e.constRetry = once(() =>
          complete.then(() => fn(effects, null)).catch(console.error),
        )
        try {
          if ("init" in init) await init.init(e, kind)
          else await init(e, kind)
        } finally {
          res()
        }
      }
      await fn(opts.effects, opts.kind)
    }
  }
}
