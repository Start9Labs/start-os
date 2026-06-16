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
 * harness. Add phases to it and update them — updates auto-report to the
 * install/update UI in the background, so you never touch the effect. Call
 * `progress.sync()` only to force a flush. Safe to ignore if there's nothing
 * to report.
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
    // Root tracker reports to the install/update finalization phase, with the
    // effects context baked into the sink. Each init gets its own nested
    // sub-tracker so handlers stay unaware of each other. Phase updates
    // auto-sync in the background; we only flush at the end.
    const tracker = new FullProgressTracker(progress =>
      opts.effects.setInitProgress({ progress }),
    )
    const phases = inits.map((_, idx) => tracker.addNestedPhase(`init:${idx}`, 1))

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
    }
    tracker.complete()
    await tracker.sync()
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
