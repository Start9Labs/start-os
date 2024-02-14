import { Effects } from "../types"

export class GetSystemSmtp {
  constructor(readonly effects: Effects) {}

  /**
   * Returns the system SMTP credentials. Restarts the service if the credentials change
   */
  const() {
    return this.effects.getSystemSmtp({
      callback: this.effects.restart,
    })
  }
  /**
   * Returns the system SMTP credentials. Does nothing if the credentials change
   */
  once() {
    return this.effects.getSystemSmtp({
      callback: () => {},
    })
  }
  /**
   * Watches the system SMTP credentials. Takes a custom callback function to run whenever the credentials change
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
}
