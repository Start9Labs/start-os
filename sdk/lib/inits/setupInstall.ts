import * as T from "../types"

export type InstallFn<Manifest extends T.Manifest, Store> = (opts: {
  effects: T.Effects
}) => Promise<void>
export class Install<Manifest extends T.Manifest, Store> {
  private constructor(readonly fn: InstallFn<Manifest, Store>) {}
  static of<Manifest extends T.Manifest, Store>(
    fn: InstallFn<Manifest, Store>,
  ) {
    return new Install(fn)
  }

  async install({ effects }: Parameters<T.ExpectedExports.init>[0]) {
    await this.fn({
      effects,
    })
  }
}

/**
 * @description Use this function to execute arbitrary logic *once*, on initial install only.
 * @example
 * In the this example, we bootstrap our Store with a random, 16-char admin password.
 *
 * ```
 * const install = sdk.setupInstall(async ({ effects }) => {
 *   await sdk.store.setOwn(
 *     effects,
 *     sdk.StorePath.adminPassword,
 *     utils.getDefaultString({
 *       charset: 'a-z,A-Z,1-9,!,@,$,%,&,*',
 *       len: 16,
 *     }),
 *   )
 * })
 * ```
 */
export function setupInstall<Manifest extends T.Manifest, Store>(
  fn: InstallFn<Manifest, Store>,
) {
  return Install.of(fn)
}
