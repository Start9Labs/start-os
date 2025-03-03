import { Effects } from "../../../base/lib/Effects"
import { PathBuilder, extractJsonPath } from "../util"

export class GetStore<Store, StoreValue> {
  constructor(
    readonly effects: Effects,
    readonly path: PathBuilder<Store, StoreValue>,
    readonly options: {
      /** Defaults to what ever the package currently in */
      packageId?: string | undefined
    } = {},
  ) {}

  /**
   * Returns the value of Store at the provided path. Reruns the context from which it has been called if the underlying value changes
   */
  const() {
    return this.effects.store.get<Store, StoreValue>({
      ...this.options,
      path: extractJsonPath(this.path),
      callback: () => this.effects.constRetry(),
    })
  }
  /**
   * Returns the value of Store at the provided path. Does nothing if the value changes
   */
  once() {
    return this.effects.store.get<Store, StoreValue>({
      ...this.options,
      path: extractJsonPath(this.path),
    })
  }

  /**
   * Watches the value of Store at the provided path. Returns an async iterator that yields whenever the value changes
   */
  async *watch() {
    while (true) {
      let callback: () => void
      const waitForNext = new Promise<void>((resolve) => {
        callback = resolve
      })
      yield await this.effects.store.get<Store, StoreValue>({
        ...this.options,
        path: extractJsonPath(this.path),
        callback: () => callback(),
      })
      await waitForNext
    }
  }

  /**
   * Watches the value of Store at the provided path. Takes a custom callback function to run whenever the value changes
   */
  onChange(
    callback: (value: StoreValue | null, error?: Error) => void | Promise<void>,
  ) {
    ;(async () => {
      for await (const value of this.watch()) {
        try {
          await callback(value)
        } catch (e) {
          console.error(
            "callback function threw an error @ GetStore.onChange",
            e,
          )
        }
      }
    })()
      .catch((e) => callback(null, e))
      .catch((e) =>
        console.error(
          "callback function threw an error @ GetStore.onChange",
          e,
        ),
      )
  }
}
export function getStore<Store, StoreValue>(
  effects: Effects,
  path: PathBuilder<Store, StoreValue>,
  options: {
    /** Defaults to what ever the package currently in */
    packageId?: string | undefined
  } = {},
) {
  return new GetStore<Store, StoreValue>(effects, path, options)
}
