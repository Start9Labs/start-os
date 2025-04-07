import * as T from "../../../base/lib/types"

export type InstallFn<Manifest extends T.SDKManifest, Store> = (opts: {
  effects: T.Effects
}) => Promise<null | void | undefined>
export class Install<Manifest extends T.SDKManifest, Store> {
  private constructor(
    readonly fn: InstallFn<Manifest, Store>,
    readonly preFn?: InstallFn<Manifest, Store>,
  ) {}
  static of<Manifest extends T.SDKManifest, Store>(
    fn: InstallFn<Manifest, Store>,
    preFn?: InstallFn<Manifest, Store>,
  ) {
    return new Install(fn, preFn)
  }

  async install({ effects }: Parameters<T.ExpectedExports.packageInit>[0]) {
    await this.fn({
      effects,
    })
  }

  async preInstall({ effects }: Parameters<T.ExpectedExports.packageInit>[0]) {
    this.preFn &&
      (await this.preFn({
        effects,
      }))
  }
}

export function setupInstall<Manifest extends T.SDKManifest, Store>(
  fn: InstallFn<Manifest, Store>,
  preFn?: InstallFn<Manifest, Store>,
) {
  return Install.of(fn, preFn)
}
