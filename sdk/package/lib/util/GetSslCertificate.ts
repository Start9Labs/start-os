import { T } from '..'
import { Effects } from '../../../base/lib/Effects'
import { AbortedError } from '../../../base/lib/util/AbortedError'
import { DropGenerator, DropPromise } from '../../../base/lib/util/Drop'

export class GetSslCertificate {
  constructor(
    readonly effects: Effects,
    readonly hostnames: string[],
    readonly algorithm?: T.Algorithm,
  ) {}

  /**
   * Returns the an SSL Certificate for the given hostnames if permitted. Restarts the service if it changes
   */
  const() {
    return this.effects.getSslCertificate({
      hostnames: this.hostnames,
      algorithm: this.algorithm,
      callback:
        this.effects.constRetry &&
        (() => this.effects.constRetry && this.effects.constRetry()),
    })
  }
  /**
   * Returns the an SSL Certificate for the given hostnames if permitted. Does nothing if it changes
   */
  once() {
    return this.effects.getSslCertificate({
      hostnames: this.hostnames,
      algorithm: this.algorithm,
    })
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
      yield await this.effects.getSslCertificate({
        hostnames: this.hostnames,
        algorithm: this.algorithm,
        callback: () => callback(),
      })
      await waitForNext
    }
    return new Promise<never>((_, rej) => rej(new AbortedError()))
  }

  /**
   * Watches the SSL Certificate for the given hostnames if permitted. Returns an async iterator that yields whenever the value changes
   */
  watch(
    abort?: AbortSignal,
  ): AsyncGenerator<[string, string, string], never, unknown> {
    const ctrl = new AbortController()
    abort?.addEventListener('abort', () => ctrl.abort())
    return DropGenerator.of(this.watchGen(ctrl.signal), () => ctrl.abort())
  }

  /**
   * Watches the SSL Certificate for the given hostnames if permitted. Takes a custom callback function to run whenever it changes
   */
  onChange(
    callback: (
      value: [string, string, string] | null,
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
            'callback function threw an error @ GetSslCertificate.onChange',
            e,
          )
        }
      }
    })()
      .catch((e) => callback(null, e))
      .catch((e) =>
        console.error(
          'callback function threw an error @ GetSslCertificate.onChange',
          e,
        ),
      )
  }

  /**
   * Watches the SSL Certificate for the given hostnames if permitted. Returns when the predicate is true
   */
  waitFor(
    pred: (value: [string, string, string] | null) => boolean,
  ): Promise<[string, string, string] | null> {
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
