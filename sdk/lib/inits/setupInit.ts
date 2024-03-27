import { DependenciesReceipt } from "../config/setupConfig"
import { SetInterfaces } from "../interfaces/setupInterfaces"
import { SDKManifest } from "../manifest/ManifestTypes"
import { ExposedStorePaths } from "../store/setupExposeStore"
import { Effects, ExpectedExports } from "../types"
import { Migrations } from "./migrations/setupMigrations"
import { Install } from "./setupInstall"
import { Uninstall } from "./setupUninstall"

export function setupInit<Manifest extends SDKManifest, Store>(
  migrations: Migrations<Manifest, Store>,
  install: Install<Manifest, Store>,
  uninstall: Uninstall<Manifest, Store>,
  setInterfaces: SetInterfaces<Manifest, Store, any, any>,
  setDependencies: (options: {
    effects: Effects
    input: any
  }) => Promise<DependenciesReceipt>,
  exposedStore: ExposedStorePaths,
): {
  init: ExpectedExports.init
  uninit: ExpectedExports.uninit
} {
  return {
    init: async (opts) => {
      await migrations.init(opts)
      await install.init(opts)
      await setInterfaces({
        ...opts,
        input: null,
      })
      await opts.effects.exposeForDependents({ paths: exposedStore })
      await setDependencies({ effects: opts.effects, input: null })
    },
    uninit: async (opts) => {
      await migrations.uninit(opts)
      await uninstall.uninit(opts)
    },
  }
}
