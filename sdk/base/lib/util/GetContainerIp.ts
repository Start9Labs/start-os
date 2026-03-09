import { Effects } from '../Effects'
import { PackageId } from '../osBindings'
import { AbortedError } from './AbortedError'
import { DropGenerator, DropPromise } from './Drop'

export class GetContainerIp {
  constructor(
    readonly effects: Effects,
    readonly opts: { packageId?: PackageId } = {},
  ) {}

  /**
   * Returns the container IP. Reruns the context from which it has been called if the underlying value changes
   */
  const() {
    return this.effects.getContainerIp({
      ...this.opts,
      callback:
        this.effects.constRetry &&
        (() => this.effects.constRetry && this.effects.constRetry()),
    })
  }
  /**
   * Returns the container IP. Does nothing if the value changes
   */
  once() {
    return this.effects.getContainerIp(this.opts)
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
      yield await this.effects.getContainerIp({
        ...this.opts,
        callback: () => callback(),
      })
      await waitForNext
    }
    return new Promise<never>((_, rej) => rej(new AbortedError()))
  }

  /**
   * Watches the container IP. Returns an async iterator that yields whenever the value changes
   */
  watch(abort?: AbortSignal): AsyncGenerator<string, never, unknown> {
    const ctrl = new AbortController()
    abort?.addEventListener('abort', () => ctrl.abort())
    return DropGenerator.of(this.watchGen(ctrl.signal), () => ctrl.abort())
  }

  /**
   * Watches the container IP. Takes a custom callback function to run whenever the value changes
   */
  onChange(
    callback: (
      value: string,
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
            'callback function threw an error @ GetContainerIp.onChange',
            e,
          )
        }
      }
    })()
      .catch((e) => callback('', e))
      .catch((e) =>
        console.error(
          'callback function threw an error @ GetContainerIp.onChange',
          e,
        ),
      )
  }

  /**
   * Watches the container IP. Returns when the predicate is true
   */
  waitFor(pred: (value: string) => boolean): Promise<string> {
    const ctrl = new AbortController()
    return DropPromise.of(
      Promise.resolve().then(async () => {
        for await (const next of this.watchGen(ctrl.signal)) {
          if (pred(next)) {
            return next
          }
        }
        return ''
      }),
      () => ctrl.abort(),
    )
  }
}
