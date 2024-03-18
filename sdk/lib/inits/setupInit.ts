import { SetInterfaces } from "../interfaces/setupInterfaces"
import { SDKManifest } from "../manifest/ManifestTypes"
import { ExpectedExports, ExposeUiPaths, ExposeUiPathsAll } from "../types"
import { createUtils } from "../util"
import { Migrations } from "./migrations/setupMigrations"
import { SetupExports } from "./setupExports"
import { Install } from "./setupInstall"
import { Uninstall } from "./setupUninstall"

export function setupInit<Manifest extends SDKManifest, Store>(
  migrations: Migrations<Manifest, Store>,
  install: Install<Manifest, Store>,
  uninstall: Uninstall<Manifest, Store>,
  setInterfaces: SetInterfaces<Manifest, Store, any, any>,
  setupExports: SetupExports<Store>,
): {
  init: ExpectedExports.init
  uninit: ExpectedExports.uninit
} {
  return {
    init: async (opts) => {
      const utils = createUtils<Manifest, Store>(opts.effects)
      await migrations.init(opts)
      await install.init(opts)
      await setInterfaces({
        ...opts,
        input: null,
        utils,
      })
      const { services, ui } = await setupExports({
        ...opts,
        utils,
      })
      await opts.effects.exposeForDependents(services)
      await opts.effects.exposeUi(
        forExpose({
          type: "object",
          value: ui,
        }),
      )
    },
    uninit: async (opts) => {
      await migrations.uninit(opts)
      await uninstall.uninit(opts)
    },
  }
}

function forExpose<Store>(ui: ExposeUiPaths<Store>): ExposeUiPathsAll {
  if (ui.type === ("object" as const)) {
    return {
      type: "object" as const,
      value: Object.fromEntries(
        Object.entries(ui.value).map(([key, value]) => [key, forExpose(value)]),
      ),
    }
  }
  return {
    description: null,

    copyable: null,
    qr: null,
    ...ui,
  }
}
