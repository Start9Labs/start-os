import { Actions } from "../../../base/lib/actions/setupActions"
import { ExtendedVersion } from "../../../base/lib/exver"
import { UpdateServiceInterfaces } from "../../../base/lib/interfaces/setupInterfaces"
import { ExposedStorePaths } from "../../../base/lib/types"
import * as T from "../../../base/lib/types"
import { StorePath } from "../util"
import { VersionGraph } from "../version/VersionGraph"
import { PostInstall, PreInstall } from "./setupInstall"
import { Uninstall } from "./setupUninstall"

export function setupInit<Manifest extends T.SDKManifest, Store>(
  versions: VersionGraph<string>,
  preInstall: PreInstall<Manifest, Store>,
  postInstall: PostInstall<Manifest, Store>,
  uninstall: Uninstall<Manifest, Store>,
  setServiceInterfaces: UpdateServiceInterfaces<any>,
  setDependencies: (options: {
    effects: T.Effects
  }) => Promise<null | void | undefined>,
  actions: Actions<Store, any>,
  initStore: Store,
  exposedStore: ExposedStorePaths,
): {
  packageInit: T.ExpectedExports.packageInit
  packageUninit: T.ExpectedExports.packageUninit
  containerInit: T.ExpectedExports.containerInit
} {
  return {
    packageInit: async (opts) => {
      const prev = await opts.effects.getDataVersion()
      if (prev) {
        await versions.migrate({
          effects: opts.effects,
          from: ExtendedVersion.parse(prev),
          to: versions.currentVersion(),
        })
      } else {
        await postInstall.postInstall(opts)
        await opts.effects.setDataVersion({
          version: versions.current.options.version,
        })
      }
    },
    packageUninit: async (opts) => {
      if (opts.nextVersion) {
        const prev = await opts.effects.getDataVersion()
        if (prev) {
          await versions.migrate({
            effects: opts.effects,
            from: ExtendedVersion.parse(prev),
            to: ExtendedVersion.parse(opts.nextVersion),
          })
        }
      } else {
        await uninstall.uninstall(opts)
      }
    },
    containerInit: async (opts) => {
      const prev = await opts.effects.getDataVersion()
      if (!prev) {
        await opts.effects.store.set({
          path: "" as StorePath,
          value: initStore,
        })
        await preInstall.preInstall(opts)
      }
      await setServiceInterfaces({
        ...opts,
      })
      await actions.update({ effects: opts.effects })
      await opts.effects.exposeForDependents({ paths: exposedStore })
      await setDependencies({ effects: opts.effects })
    },
  }
}
