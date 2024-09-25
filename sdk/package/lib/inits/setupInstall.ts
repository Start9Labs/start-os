import * as T from "../../../base/lib/types"

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

  async install({ effects }: Parameters<T.ExpectedExports.packageInit>[0]) {
    await this.fn({
      effects,
    })
  }
}

export function setupInstall<Manifest extends T.Manifest, Store>(
  fn: InstallFn<Manifest, Store>,
) {
  return Install.of(fn)
}
