import { Actions } from "../../../base/lib/actions/setupActions"
import { ExtendedVersion } from "../../../base/lib/exver"
import { UpdateServiceInterfaces } from "../../../base/lib/interfaces/setupInterfaces"
import { ExposedStorePaths } from "../../../base/lib/types"
import * as T from "../../../base/lib/types"
import { VersionGraph } from "../version/VersionGraph"
import { Install } from "./setupInstall"
import { Uninstall } from "./setupUninstall"

export function setupInit<Manifest extends T.Manifest, Store>(
  versions: VersionGraph<Manifest["version"]>,
  install: Install<Manifest, Store>,
  uninstall: Uninstall<Manifest, Store>,
  setServiceInterfaces: UpdateServiceInterfaces<any>,
  setDependencies: (options: { effects: T.Effects }) => Promise<null>,
  actions: Actions<Store, any>,
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
        await install.install(opts)
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
      await setServiceInterfaces({
        ...opts,
      })
      await actions.update({ effects: opts.effects })
      await opts.effects.exposeForDependents({ paths: exposedStore })
      await setDependencies({ effects: opts.effects })
    },
  }
}
