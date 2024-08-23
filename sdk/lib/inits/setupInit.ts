import { DependenciesReceipt } from "../config/setupConfig"
import { ExtendedVersion } from "../exver"
import { SetInterfaces } from "../interfaces/setupInterfaces"
import { ExposedStorePaths } from "../store/setupExposeStore"
import * as T from "../types"
import { VersionGraph } from "../version/VersionGraph"
import { Install } from "./setupInstall"
import { Uninstall } from "./setupUninstall"

export function setupInit<Manifest extends T.Manifest, Store>(
  versions: VersionGraph<Manifest["version"]>,
  install: Install<Manifest, Store>,
  uninstall: Uninstall<Manifest, Store>,
  setInterfaces: SetInterfaces<Manifest, Store, any, any>,
  setDependencies: (options: {
    effects: T.Effects
    input: any
  }) => Promise<DependenciesReceipt>,
  exposedStore: ExposedStorePaths,
): {
  init: T.ExpectedExports.init
  uninit: T.ExpectedExports.uninit
} {
  return {
    init: async (opts) => {
      const prev = await opts.effects.getDataVersion()
      if (prev) {
        await versions.migrate({
          effects: opts.effects,
          from: ExtendedVersion.parse(prev),
          to: versions.currentVersion(),
        })
      } else {
        await install.install(opts)
        await opts.effects.setDataVersion({
          version: versions.current.options.version,
        })
      }
      await setInterfaces({
        ...opts,
        input: null,
      })
      await opts.effects.exposeForDependents({ paths: exposedStore })
      await setDependencies({ effects: opts.effects, input: null })
    },
    uninit: async (opts) => {
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
  }
}
