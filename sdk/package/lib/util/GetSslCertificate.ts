import { T } from ".."
import { Effects } from "../../../base/lib/Effects"

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

  /**
   * Watches the SSL Certificate for the given hostnames if permitted. Returns an async iterator that yields whenever the value changes
   */
  async *watch() {
    const resolveCell = { resolve: () => {} }
    this.effects.onLeaveContext(() => {
      resolveCell.resolve()
    })
    while (this.effects.isInContext) {
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
  }

  /**
   * Watches the SSL Certificate for the given hostnames if permitted. Takes a custom callback function to run whenever it changes
   */
  onChange(
    callback: (
      value: [string, string, string] | null,
      error?: Error,
    ) => void | Promise<void>,
  ) {
    ;(async () => {
      for await (const value of this.watch()) {
        try {
          await callback(value)
        } catch (e) {
          console.error(
            "callback function threw an error @ GetSslCertificate.onChange",
            e,
          )
        }
      }
    })()
      .catch((e) => callback(null, e))
      .catch((e) =>
        console.error(
          "callback function threw an error @ GetSslCertificate.onChange",
          e,
        ),
      )
  }
}
