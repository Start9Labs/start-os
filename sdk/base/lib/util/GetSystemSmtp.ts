import { Effects } from '../Effects'
import * as T from '../types'
import { AbortedError } from './AbortedError'
import { DropGenerator, DropPromise } from './Drop'

export class GetSystemSmtp {
  constructor(readonly effects: Effects) {}

  /**
   * Returns the system SMTP credentials. Reruns the context from which it has been called if the underlying value changes
   */
  const() {
    return this.effects.getSystemSmtp({
      callback:
        this.effects.constRetry &&
        (() => this.effects.constRetry && this.effects.constRetry()),
    })
  }
  /**
   * Returns the system SMTP credentials. Does nothing if the credentials change
   */
  once() {
    return this.effects.getSystemSmtp({})
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
      yield await this.effects.getSystemSmtp({
        callback: () => callback(),
      })
      await waitForNext
    }
    return new Promise<never>((_, rej) => rej(new AbortedError()))
  }

  /**
   * Watches the system SMTP credentials. Returns an async iterator that yields whenever the value changes
   */
  watch(
    abort?: AbortSignal,
  ): AsyncGenerator<T.SmtpValue | null, never, unknown> {
    const ctrl = new AbortController()
    abort?.addEventListener('abort', () => ctrl.abort())
    return DropGenerator.of(this.watchGen(ctrl.signal), () => ctrl.abort())
  }

  /**
   * Watches the system SMTP credentials. Takes a custom callback function to run whenever the credentials change
   */
  onChange(
    callback: (
      value: T.SmtpValue | null,
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
            'callback function threw an error @ GetSystemSmtp.onChange',
            e,
          )
        }
      }
    })()
      .catch((e) => callback(null, e))
      .catch((e) =>
        console.error(
          'callback function threw an error @ GetSystemSmtp.onChange',
          e,
        ),
      )
  }

  /**
   * Watches the system SMTP credentials. Returns when the predicate is true
   */
  waitFor(
    pred: (value: T.SmtpValue | null) => boolean,
  ): Promise<T.SmtpValue | null> {
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
