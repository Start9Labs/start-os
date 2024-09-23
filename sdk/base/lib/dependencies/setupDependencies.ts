import * as T from "../types"
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
): (options: { effects: T.Effects }) => Promise<void> {
  return (options: { effects: T.Effects }) => {
    const updater = async (options: { effects: T.Effects }) => {
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
    const updaterCtx = { options }
    updaterCtx.options = {
      effects: {
        ...options.effects,
        constRetry: () => updater(updaterCtx.options),
      },
    }
    return updater(updaterCtx.options)
  }
}
