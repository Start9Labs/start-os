import * as T from "../../../base/lib/types"

export type InstallFn<Manifest extends T.SDKManifest, Store> = (opts: {
  effects: T.Effects
}) => Promise<null | void | undefined>
export class Install<Manifest extends T.SDKManifest, Store> {
  protected constructor(readonly fn: InstallFn<Manifest, Store>) {}
}

export class PreInstall<Manifest extends T.SDKManifest, Store> extends Install<
  Manifest,
  Store
> {
  private constructor(fn: InstallFn<Manifest, Store>) {
    super(fn)
  }
  static of<Manifest extends T.SDKManifest, Store>(
    fn: InstallFn<Manifest, Store>,
  ) {
    return new PreInstall(fn)
  }

  async preInstall({ effects }: Parameters<T.ExpectedExports.packageInit>[0]) {
    await this.fn({
      effects,
    })
  }
}

export function setupPreInstall<Manifest extends T.SDKManifest, Store>(
  fn: InstallFn<Manifest, Store>,
) {
  return PreInstall.of(fn)
}

export class PostInstall<Manifest extends T.SDKManifest, Store> extends Install<
  Manifest,
  Store
> {
  private constructor(fn: InstallFn<Manifest, Store>) {
    super(fn)
  }
  static of<Manifest extends T.SDKManifest, Store>(
    fn: InstallFn<Manifest, Store>,
  ) {
    return new PostInstall(fn)
  }

  async postInstall({ effects }: Parameters<T.ExpectedExports.packageInit>[0]) {
    await this.fn({
      effects,
    })
  }
}

export function setupPostInstall<Manifest extends T.SDKManifest, Store>(
  fn: InstallFn<Manifest, Store>,
) {
  return PostInstall.of(fn)
}
