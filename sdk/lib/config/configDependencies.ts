import * as T from "../types"

export type ConfigDependencies<T extends T.Manifest> = {
  exists(id: keyof T["dependencies"]): T.Dependencies[number]
  running(
    id: keyof T["dependencies"],
    healthChecks: string[],
  ): T.Dependencies[number]
}

export const configDependenciesSet = <
  T extends T.Manifest,
>(): ConfigDependencies<T> => ({
  exists(id: keyof T["dependencies"]) {
    return {
      id,
      kind: "exists",
    } as T.Dependencies[number]
  },

  running(id: keyof T["dependencies"], healthChecks: string[]) {
    return {
      id,
      kind: "running",
      healthChecks,
    } as T.Dependencies[number]
  },
})
