import * as T from "../types"
import { once } from "../util"

type DependencyType<Manifest extends T.SDKManifest> = {
  [K in keyof Manifest["dependencies"]]: Omit<T.DependencyRequirement, "id">
}

export function setupDependencies<Manifest extends T.SDKManifest>(
  fn: (options: { effects: T.Effects }) => Promise<DependencyType<Manifest>>,
): (options: { effects: T.Effects }) => Promise<null> {
  const cell = { updater: async (_: { effects: T.Effects }) => null }
  cell.updater = async (options: { effects: T.Effects }) => {
    options.effects = {
      ...options.effects,
      constRetry: once(() => {
        cell.updater(options)
      }),
    }
    const dependencyType = await fn(options)
    return await options.effects.setDependencies({
      dependencies: Object.entries(dependencyType).map(
        ([id, { versionRange, ...x }, ,]) =>
          ({
            id,
            ...x,
            versionRange: versionRange.toString(),
          }) as T.DependencyRequirement,
      ),
    })
  }
  return cell.updater
}
