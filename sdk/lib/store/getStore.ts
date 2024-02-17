import { Effects, EnsureStorePath } from "../types"

export class GetStore<Store, Path extends string> {
  constructor(
    readonly effects: Effects,
    readonly path: Path & EnsureStorePath<Store, Path>,
    readonly options: {
      /** Defaults to what ever the package currently in */
      packageId?: string | undefined
    } = {},
  ) {}

  /**
   * Returns the value of Store at the provided path. Restart the service if the value changes
   */
  const() {
    return this.effects.store.get<Store, Path>({
      ...this.options,
      path: this.path as any,
      callback: this.effects.restart,
    })
  }
  /**
   * Returns the value of Store at the provided path. Does nothing if the value changes
   */
  once() {
    return this.effects.store.get<Store, Path>({
      ...this.options,
      path: this.path as any,
      callback: () => {},
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
      yield await this.effects.store.get<Store, Path>({
        ...this.options,
        path: this.path as any,
        callback: () => callback(),
      })
      await waitForNext
    }
  }
}
export function getStore<Store, Path extends string>(
  effects: Effects,
  path: Path & EnsureStorePath<Store, Path>,
  options: {
    /** Defaults to what ever the package currently in */
    packageId?: string | undefined
  } = {},
) {
  return new GetStore<Store, Path>(effects, path as any, options)
}
