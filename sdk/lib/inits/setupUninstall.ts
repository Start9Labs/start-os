import * as T from "../types"

export type UninstallFn<Manifest extends T.Manifest, Store> = (opts: {
  effects: T.Effects
}) => Promise<void>
export class Uninstall<Manifest extends T.Manifest, Store> {
  private constructor(readonly fn: UninstallFn<Manifest, Store>) {}
  static of<Manifest extends T.Manifest, Store>(
    fn: UninstallFn<Manifest, Store>,
  ) {
    return new Uninstall(fn)
  }

  async uninstall({
    effects,
    nextVersion,
  }: Parameters<T.ExpectedExports.packageUninit>[0]) {
    if (!nextVersion)
      await this.fn({
        effects,
      })
  }
}

export function setupUninstall<Manifest extends T.Manifest, Store>(
  fn: UninstallFn<Manifest, Store>,
) {
  return Uninstall.of(fn)
}
