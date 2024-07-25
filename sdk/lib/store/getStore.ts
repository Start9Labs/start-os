import { Effects } from "../types"
import { PathBuilder, extractJsonPath } from "./PathBuilder"

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
   * Returns the value of Store at the provided path. Restart the service if the value changes
   */
  const() {
    return this.effects.store.get<Store, StoreValue>({
      ...this.options,
      path: extractJsonPath(this.path),
      callback: this.effects.restart,
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
   * Watches the value of Store at the provided path. Takes a custom callback function to run whenever the value changes
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
