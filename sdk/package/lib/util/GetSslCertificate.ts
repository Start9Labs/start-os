import { T } from ".."
import { Effects } from "../../../base/lib/Effects"

export class GetSslCertificate {
  constructor(
    readonly effects: Effects,
    readonly hostnames: string[],
    readonly algorithm?: T.Algorithm,
  ) {}

  /**
   * Returns the system SMTP credentials. Restarts the service if the credentials change
   */
  const() {
    return this.effects.getSslCertificate({
      hostnames: this.hostnames,
      algorithm: this.algorithm,
      callback: () => this.effects.constRetry(),
    })
  }
  /**
   * Returns the system SMTP credentials. Does nothing if the credentials change
   */
  once() {
    return this.effects.getSslCertificate({
      hostnames: this.hostnames,
      algorithm: this.algorithm,
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
      yield await this.effects.getSslCertificate({
        hostnames: this.hostnames,
        algorithm: this.algorithm,
        callback: () => callback(),
      })
      await waitForNext
    }
  }
}
