import * as T from "../../../base/lib/types"

export type InstallFn<Manifest extends T.SDKManifest> = (opts: {
  effects: T.Effects
}) => Promise<null | void | undefined>
export class Install<Manifest extends T.SDKManifest> {
  protected constructor(readonly fn: InstallFn<Manifest>) {}
}

export class PreInstall<
  Manifest extends T.SDKManifest,
> extends Install<Manifest> {
  private constructor(fn: InstallFn<Manifest>) {
    super(fn)
  }
  static of<Manifest extends T.SDKManifest>(fn: InstallFn<Manifest>) {
    return new PreInstall(fn)
  }

  async preInstall({ effects }: Parameters<T.ExpectedExports.packageInit>[0]) {
    await this.fn({
      effects,
    })
  }
}

export function setupPreInstall<Manifest extends T.SDKManifest>(
  fn: InstallFn<Manifest>,
) {
  return PreInstall.of(fn)
}

export class PostInstall<
  Manifest extends T.SDKManifest,
> extends Install<Manifest> {
  private constructor(fn: InstallFn<Manifest>) {
    super(fn)
  }
  static of<Manifest extends T.SDKManifest>(fn: InstallFn<Manifest>) {
    return new PostInstall(fn)
  }

  async postInstall({ effects }: Parameters<T.ExpectedExports.packageInit>[0]) {
    await this.fn({
      effects,
    })
  }
}

export function setupPostInstall<Manifest extends T.SDKManifest>(
  fn: InstallFn<Manifest>,
) {
  return PostInstall.of(fn)
}
