import { VersionRange } from '../../../base/lib/exver'
import * as T from '../../../base/lib/types'
import { once } from '../util'
import { FullProgressTracker } from '../util/FullProgressTracker'

/**
 * The reason a service's init function is being called:
 * - `'install'` — first-time installation
 * - `'update'` — after a package update
 * - `'restore'` — after restoring from backup
 * - `null` — regular startup (no special lifecycle event)
 */
export type InitKind = 'install' | 'update' | 'restore' | null

/**
 * Function signature for an init handler that runs during service startup.
 *
 * `progress` is this handler's own {@link FullProgressTracker}, created by the
 * harness. Add phases to it and call `progress.sync(effects)` to surface
 * progress in the install/update UI — no need to touch the effect directly.
 * It's safe to ignore if the handler has nothing to report.
 */
export type InitFn<Kind extends InitKind = InitKind> = (
  effects: T.Effects,
  kind: Kind,
  progress: FullProgressTracker,
) => Promise<void | null | undefined>

/** Object form of an init handler — implements an `init()` method. */
export interface InitScript<Kind extends InitKind = InitKind> {
  init(
    effects: T.Effects,
    kind: Kind,
    progress?: FullProgressTracker,
  ): Promise<void>
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
  return async opts => {
    // Root tracker reports to the install/update finalization phase. Each init
    // gets its own nested sub-tracker so handlers stay unaware of each other.
    const tracker = new FullProgressTracker((effects, progress) =>
      effects.setInitProgress({ progress }),
    )
    const phases = inits.map((_, idx) => tracker.addNestedPhase(`init:${idx}`, 1))
    await tracker.sync(opts.effects)

    for (const idx in inits) {
      const init = inits[idx]
      const phase = phases[idx]
      const fn = async () => {
        // A re-run (constRetry) starts the handler's phases fresh.
        phase.reset()
        let res: (value?: undefined) => void = () => {}
        const complete = new Promise(resolve => {
          res = resolve
        })
        const e: T.Effects = opts.effects.child(`init_${idx}`)
        e.constRetry = once(() =>
          complete.then(() => fn()).catch(console.error),
        )
        try {
          if ('init' in init) await init.init(e, opts.kind, phase)
          else await init(e, opts.kind, phase)
        } finally {
          res()
        }
      }
      await fn()
      phase.complete()
      await tracker.sync(opts.effects)
    }
    tracker.complete()
    await tracker.sync(opts.effects)
  }
}

/** Normalizes an {@link InitScriptOrFn} into an {@link InitScript} object. */
export function setupOnInit(onInit: InitScriptOrFn): InitScript {
  return 'init' in onInit
    ? onInit
    : {
        init: async (effects, kind, progress) => {
          await onInit(effects, kind, progress as FullProgressTracker)
        },
      }
}
