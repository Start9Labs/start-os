import * as T from "../../../base/lib/types"

export type UninstallFn<Manifest extends T.SDKManifest, Store> = (opts: {
  effects: T.Effects
}) => Promise<null | void | undefined>
export class Uninstall<Manifest extends T.SDKManifest, Store> {
  private constructor(readonly fn: UninstallFn<Manifest, Store>) {}
  static of<Manifest extends T.SDKManifest, Store>(
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

export function setupUninstall<Manifest extends T.SDKManifest, Store>(
  fn: UninstallFn<Manifest, Store>,
) {
  return Uninstall.of(fn)
}
