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
      const fn = async () => {
        let res: (value?: undefined) => void = () => {}
        const complete = new Promise((resolve) => {
          res = resolve
        })
        const e: T.Effects = opts.effects.child(`init_${idx}`)
        e.constRetry = once(() =>
          complete.then(() => fn()).catch(console.error),
        )
        try {
          if ("init" in init) await init.init(e, opts.kind)
          else await init(e, opts.kind)
        } finally {
          res()
        }
      }
      await fn()
    }
  }
}

export function setupOnInit(onInit: InitScriptOrFn): InitScript {
  return "init" in onInit
    ? onInit
    : {
        init: async (effects, kind) => {
          await onInit(effects, kind)
        },
      }
}

export function setupOnInstall(
  onInstall: InitScriptOrFn<"install">,
): InitScript {
  return {
    init: async (effects, kind) => {
      if (kind === "install") {
        if ("init" in onInstall) await onInstall.init(effects, kind)
        else await onInstall(effects, kind)
      }
    },
  }
}

export function setupOnUpdate(onUpdate: InitScriptOrFn<"update">): InitScript {
  return {
    init: async (effects, kind) => {
      if (kind === "update") {
        if ("init" in onUpdate) await onUpdate.init(effects, kind)
        else await onUpdate(effects, kind)
      }
    },
  }
}

export function setupOnInstallOrUpdate(
  onInstallOrUpdate: InitScriptOrFn<"install" | "update">,
): InitScript {
  return {
    init: async (effects, kind) => {
      if (kind === "install" || kind === "update") {
        if ("init" in onInstallOrUpdate)
          await onInstallOrUpdate.init(effects, kind)
        else await onInstallOrUpdate(effects, kind)
      }
    },
  }
}
