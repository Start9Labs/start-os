import { Effects } from "../Effects"
import * as T from "../types"

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

  /**
   * Watches the system SMTP credentials. Returns an async iterator that yields whenever the value changes
   */
  async *watch() {
    while (true) {
      let callback: () => void
      const waitForNext = new Promise<void>((resolve) => {
        callback = resolve
      })
      yield await this.effects.getSystemSmtp({
        callback: () => callback(),
      })
      await waitForNext
    }
  }

  /**
   * Watches the system SMTP credentials. Takes a custom callback function to run whenever the credentials change
   */
  onChange(
    callback: (
      value: T.SmtpValue | null,
      error?: Error,
    ) => void | Promise<void>,
  ) {
    ;(async () => {
      for await (const value of this.watch()) {
        try {
          await callback(value)
        } catch (e) {
          console.error(
            "callback function threw an error @ GetSystemSmtp.onChange",
            e,
          )
        }
      }
    })()
      .catch((e) => callback(null, e))
      .catch((e) =>
        console.error(
          "callback function threw an error @ GetSystemSmtp.onChange",
          e,
        ),
      )
  }
}
