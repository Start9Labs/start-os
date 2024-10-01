import * as T from "../types"
import { once } from "../util"
import { Dependency } from "./Dependency"

type DependencyType<Manifest extends T.Manifest> = {
  [K in keyof {
    [K in keyof Manifest["dependencies"]]: Manifest["dependencies"][K]["optional"] extends false
      ? K
      : never
  }]: Dependency
} & {
  [K in keyof {
    [K in keyof Manifest["dependencies"]]: Manifest["dependencies"][K]["optional"] extends true
      ? K
      : never
  }]?: Dependency
}

export function setupDependencies<Manifest extends T.Manifest>(
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
        ([
          id,
          {
            data: { versionRange, ...x },
          },
        ]) => ({
          id,
          ...x,
          ...(x.type === "running"
            ? {
                kind: "running",
                healthChecks: x.healthChecks,
              }
            : {
                kind: "exists",
              }),
          versionRange: versionRange.toString(),
        }),
      ),
    })
  }
  return cell.updater
}
