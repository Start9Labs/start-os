import { SDKManifest } from "../manifest/ManifestTypes"
import { Effects, ExpectedExports } from "../types"

export type UninstallFn<Manifest extends SDKManifest, Store> = (opts: {
  effects: Effects
}) => Promise<void>
export class Uninstall<Manifest extends SDKManifest, Store> {
  private constructor(readonly fn: UninstallFn<Manifest, Store>) {}
  static of<Manifest extends SDKManifest, Store>(
    fn: UninstallFn<Manifest, Store>,
  ) {
    return new Uninstall(fn)
  }

  async uninit({
    effects,
    nextVersion,
  }: Parameters<ExpectedExports.uninit>[0]) {
    if (!nextVersion)
      await this.fn({
        effects,
      })
  }
}

export function setupUninstall<Manifest extends SDKManifest, Store>(
  fn: UninstallFn<Manifest, Store>,
) {
  return Uninstall.of(fn)
}
