import { Effects } from '../Effects'
import { Host, HostId, PackageId } from '../osBindings'
import { AbortedError } from './AbortedError'
import { DropGenerator, DropPromise } from './Drop'

export class GetHostInfo {
  constructor(
    readonly effects: Effects,
    readonly opts: { hostId: HostId; packageId?: PackageId },
  ) {}

  /**
   * Returns host info. Reruns the context from which it has been called if the underlying value changes
   */
  const() {
    return this.effects.getHostInfo({
      ...this.opts,
      callback:
        this.effects.constRetry &&
        (() => this.effects.constRetry && this.effects.constRetry()),
    })
  }
  /**
   * Returns host info. Does nothing if the value changes
   */
  once() {
    return this.effects.getHostInfo(this.opts)
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
      yield await this.effects.getHostInfo({
        ...this.opts,
        callback: () => callback(),
      })
      await waitForNext
    }
    return new Promise<never>((_, rej) => rej(new AbortedError()))
  }

  /**
   * Watches host info. Returns an async iterator that yields whenever the value changes
   */
  watch(abort?: AbortSignal): AsyncGenerator<Host | null, never, unknown> {
    const ctrl = new AbortController()
    abort?.addEventListener('abort', () => ctrl.abort())
    return DropGenerator.of(this.watchGen(ctrl.signal), () => ctrl.abort())
  }

  /**
   * Watches host info. Takes a custom callback function to run whenever the value changes
   */
  onChange(
    callback: (
      value: Host | null,
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
            'callback function threw an error @ GetHostInfo.onChange',
            e,
          )
        }
      }
    })()
      .catch((e) => callback(null, e))
      .catch((e) =>
        console.error(
          'callback function threw an error @ GetHostInfo.onChange',
          e,
        ),
      )
  }

  /**
   * Watches host info. Returns when the predicate is true
   */
  waitFor(pred: (value: Host | null) => boolean): Promise<Host | null> {
    const ctrl = new AbortController()
    return DropPromise.of(
      Promise.resolve().then(async () => {
        for await (const next of this.watchGen(ctrl.signal)) {
          if (pred(next)) {
            return next
          }
        }
        return null
      }),
      () => ctrl.abort(),
    )
  }
}
