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

  async init({
    effects,
    previousVersion,
  }: Parameters<T.ExpectedExports.init>[0]) {
    if (!previousVersion)
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
