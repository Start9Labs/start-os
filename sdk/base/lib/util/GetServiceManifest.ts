import { Effects } from '../Effects'
import { Manifest, PackageId } from '../osBindings'
import { AbortedError } from './AbortedError'
import { DropGenerator, DropPromise } from './Drop'

export class GetServiceManifest {
  constructor(
    readonly effects: Effects,
    readonly opts: { packageId: PackageId },
  ) {}

  /**
   * Returns the service manifest. Reruns the context from which it has been called if the underlying value changes
   */
  const() {
    return this.effects.getServiceManifest({
      ...this.opts,
      callback:
        this.effects.constRetry &&
        (() => this.effects.constRetry && this.effects.constRetry()),
    })
  }
  /**
   * Returns the service manifest. Does nothing if the value changes
   */
  once() {
    return this.effects.getServiceManifest(this.opts)
  }

  private async *watchGen(abort?: AbortSignal) {
    const resolveCell = { resolve: () => {} }
    this.effects.onLeaveContext(() => {
      resolveCell.resolve()
    })
    abort?.addEventListener('abort', () => resolveCell.resolve())
    while (this.effects.isInContext && !abort?.aborted) {
      let callback: () => void = () => {}
      const waitForNext = new Promise<void>((resolve) => {
        callback = resolve
        resolveCell.resolve = resolve
      })
      yield await this.effects.getServiceManifest({
        ...this.opts,
        callback: () => callback(),
      })
      await waitForNext
    }
    return new Promise<never>((_, rej) => rej(new AbortedError()))
  }

  /**
   * Watches the service manifest. Returns an async iterator that yields whenever the value changes
   */
  watch(abort?: AbortSignal): AsyncGenerator<Manifest, never, unknown> {
    const ctrl = new AbortController()
    abort?.addEventListener('abort', () => ctrl.abort())
    return DropGenerator.of(this.watchGen(ctrl.signal), () => ctrl.abort())
  }

  /**
   * Watches the service manifest. Takes a custom callback function to run whenever the value changes
   */
  onChange(
    callback: (
      value: Manifest | null,
      error?: Error,
    ) => { cancel: boolean } | Promise<{ cancel: boolean }>,
  ) {
    ;(async () => {
      const ctrl = new AbortController()
      for await (const value of this.watch(ctrl.signal)) {
        try {
          const res = await callback(value)
          if (res.cancel) {
            ctrl.abort()
            break
          }
        } catch (e) {
          console.error(
            'callback function threw an error @ GetServiceManifest.onChange',
            e,
          )
        }
      }
    })()
      .catch((e) => callback(null, e))
      .catch((e) =>
        console.error(
          'callback function threw an error @ GetServiceManifest.onChange',
          e,
        ),
      )
  }

  /**
   * Watches the service manifest. Returns when the predicate is true
   */
  waitFor(pred: (value: Manifest) => boolean): Promise<Manifest> {
    const ctrl = new AbortController()
    return DropPromise.of(
      Promise.resolve().then(async () => {
        for await (const next of this.watchGen(ctrl.signal)) {
          if (pred(next)) {
            return next
          }
        }
        throw new Error('context left before predicate passed')
      }),
      () => ctrl.abort(),
    )
  }
}
