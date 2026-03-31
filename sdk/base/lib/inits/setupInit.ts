import { VersionRange } from '../../../base/lib/exver'
import * as T from '../../../base/lib/types'
import { once } from '../util'

/**
 * The reason a service's init function is being called:
 * - `'install'` — first-time installation
 * - `'update'` — after a package update
 * - `'restore'` — after restoring from backup
 * - `null` — regular startup (no special lifecycle event)
 */
export type InitKind = 'install' | 'update' | 'restore' | null

/** Function signature for an init handler that runs during service startup. */
export type InitFn<Kind extends InitKind = InitKind> = (
  effects: T.Effects,
  kind: Kind,
) => Promise<void | null | undefined>

/** Object form of an init handler — implements an `init()` method. */
export interface InitScript<Kind extends InitKind = InitKind> {
  init(effects: T.Effects, kind: Kind): Promise<void>
}

/** Either an {@link InitScript} object or an {@link InitFn} function. */
export type InitScriptOrFn<Kind extends InitKind = InitKind> =
  | InitScript<Kind>
  | InitFn<Kind>

/**
 * Composes multiple init handlers into a single `ExpectedExports.init`-compatible function.
 * Handlers are executed sequentially in the order provided.
 *
 * @param inits - One or more init handlers to compose
 */
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
          if ('init' in init) await init.init(e, opts.kind)
          else await init(e, opts.kind)
        } finally {
          res()
        }
      }
      await fn()
    }
  }
}

/** Normalizes an {@link InitScriptOrFn} into an {@link InitScript} object. */
export function setupOnInit(onInit: InitScriptOrFn): InitScript {
  return 'init' in onInit
    ? onInit
    : {
        init: async (effects, kind) => {
          await onInit(effects, kind)
        },
      }
}
