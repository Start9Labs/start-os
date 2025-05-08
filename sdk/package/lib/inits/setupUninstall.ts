import * as T from "../../../base/lib/types"

export type UninstallFn<Manifest extends T.SDKManifest> = (opts: {
  effects: T.Effects
}) => Promise<null | void | undefined>
export class Uninstall<Manifest extends T.SDKManifest> {
  private constructor(readonly fn: UninstallFn<Manifest>) {}
  static of<Manifest extends T.SDKManifest>(fn: UninstallFn<Manifest>) {
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

export function setupUninstall<Manifest extends T.SDKManifest>(
  fn: UninstallFn<Manifest>,
) {
  return Uninstall.of(fn)
}
