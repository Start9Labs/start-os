import { SDKManifest } from "../manifest/ManifestTypes"
import { Effects, ExpectedExports } from "../types"
import { Utils, createUtils } from "../util/utils"

export type InstallFn<Manifest extends SDKManifest, Store> = (opts: {
  effects: Effects
  utils: Utils<Manifest, Store>
}) => Promise<void>
export class Install<Manifest extends SDKManifest, Store> {
  private constructor(readonly fn: InstallFn<Manifest, Store>) {}
  static of<Manifest extends SDKManifest, Store>(
    fn: InstallFn<Manifest, Store>,
  ) {
    return new Install(fn)
  }

  async init({
    effects,
    previousVersion,
  }: Parameters<ExpectedExports.init>[0]) {
    if (!previousVersion)
      await this.fn({
        effects,
        utils: createUtils(effects),
      })
  }
}

export function setupInstall<Manifest extends SDKManifest, Store>(
  fn: InstallFn<Manifest, Store>,
) {
  return Install.of(fn)
}
