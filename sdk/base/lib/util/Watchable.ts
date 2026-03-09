import { Effects } from '../Effects'
import { AbortedError } from './AbortedError'
import { DropGenerator, DropPromise } from './Drop'

export abstract class Watchable<T> {
  constructor(readonly effects: Effects) {}

  protected abstract call(callback?: () => void): Promise<T>
  protected abstract readonly label: string

  /**
   * Returns the value. Reruns the context from which it has been called if the underlying value changes
   */
  const(): Promise<T> {
    return this.call(
      this.effects.constRetry &&
        (() => this.effects.constRetry && this.effects.constRetry()),
    )
  }

  /**
   * Returns the value. Does nothing if the value changes
   */
  once(): Promise<T> {
    return this.call()
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
      yield await this.call(() => callback())
      await waitForNext
    }
    return new Promise<never>((_, rej) => rej(new AbortedError()))
  }

  /**
   * Watches the value. Returns an async iterator that yields whenever the value changes
   */
  watch(abort?: AbortSignal): AsyncGenerator<T, never, unknown> {
    const ctrl = new AbortController()
    abort?.addEventListener('abort', () => ctrl.abort())
    return DropGenerator.of(this.watchGen(ctrl.signal), () => ctrl.abort())
  }

  /**
   * Watches the value. Takes a custom callback function to run whenever the value changes
   */
  onChange(
    callback: (
      value: T | undefined,
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
            `callback function threw an error @ ${this.label}.onChange`,
            e,
          )
        }
      }
    })()
      .catch((e) => callback(undefined, e))
      .catch((e) =>
        console.error(
          `callback function threw an error @ ${this.label}.onChange`,
          e,
        ),
      )
  }

  /**
   * Watches the value. Returns when the predicate is true
   */
  waitFor(pred: (value: T) => boolean): Promise<T> {
    const ctrl = new AbortController()
    return DropPromise.of(
      Promise.resolve().then(async () => {
        for await (const next of this.watchGen(ctrl.signal)) {
          if (pred(next)) {
            return next
          }
        }
        throw new AbortedError()
      }),
      () => ctrl.abort(),
    )
  }
}
