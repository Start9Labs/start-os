import * as T from "../types"
import { once } from "../util"

export type RequiredDependenciesOf<Manifest extends T.SDKManifest> = {
  [K in keyof Manifest["dependencies"]]: Manifest["dependencies"][K]["optional"] extends false
    ? K
    : never
}[keyof Manifest["dependencies"]]
export type OptionalDependenciesOf<Manifest extends T.SDKManifest> = Exclude<
  keyof Manifest["dependencies"],
  RequiredDependenciesOf<Manifest>
>

type DependencyRequirement =
  | {
      kind: "running"
      healthChecks: Array<T.HealthCheckId>
      versionRange: string
    }
  | {
      kind: "exists"
      versionRange: string
    }
type Matches<T, U> = T extends U ? (U extends T ? null : never) : never
const _checkType: Matches<
  DependencyRequirement & { id: T.PackageId },
  T.DependencyRequirement
> = null

export type CurrentDependenciesResult<Manifest extends T.SDKManifest> = {
  [K in RequiredDependenciesOf<Manifest>]: DependencyRequirement
} & {
  [K in OptionalDependenciesOf<Manifest>]?: DependencyRequirement
}

export function setupDependencies<Manifest extends T.SDKManifest>(
  fn: (options: {
    effects: T.Effects
  }) => Promise<CurrentDependenciesResult<Manifest>>,
): (effects: T.Effects) => Promise<null> {
  return async (effects: T.Effects) => {
    const dependencyType = await fn({ effects })
    return await effects.setDependencies({
      dependencies: Object.entries(dependencyType)
        .map(([k, v]) => [k, v as DependencyRequirement] as const)
        .map(
          ([id, { versionRange, ...x }]) =>
            ({
              id,
              ...x,
              versionRange: versionRange.toString(),
            }) as T.DependencyRequirement,
        ),
    })
  }
}
