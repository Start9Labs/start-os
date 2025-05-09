import { Actions } from "../../../base/lib/actions/setupActions"
import { ExtendedVersion } from "../../../base/lib/exver"
import { UpdateServiceInterfaces } from "../../../base/lib/interfaces/setupInterfaces"
import * as T from "../../../base/lib/types"
import { VersionGraph } from "../version/VersionGraph"
import { PostInstall, PreInstall } from "./setupInstall"
import { Uninstall } from "./setupUninstall"

export function setupInit<Manifest extends T.SDKManifest>(
  versions: VersionGraph<string>,
  preInstall: PreInstall<Manifest>,
  postInstall: PostInstall<Manifest>,
  uninstall: Uninstall<Manifest>,
  setServiceInterfaces: UpdateServiceInterfaces<any>,
  setDependencies: (options: {
    effects: T.Effects
  }) => Promise<null | void | undefined>,
  actions: Actions<any>,
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
        await preInstall.preInstall(opts)
      }
      await setServiceInterfaces({
        ...opts,
      })
      await actions.update({ effects: opts.effects })
      await setDependencies({ effects: opts.effects })
    },
  }
}
