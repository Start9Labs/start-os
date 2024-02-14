import { SDKManifest } from "../manifest/ManifestTypes"
import { Dependency } from "../types"

export type ConfigDependencies<T extends SDKManifest> = {
  exists(id: keyof T["dependencies"]): Dependency
  running(id: keyof T["dependencies"]): Dependency
}

export const configDependenciesSet = <
  T extends SDKManifest,
>(): ConfigDependencies<T> => ({
  exists(id: keyof T["dependencies"]) {
    return {
      id,
      kind: "exists",
    } as Dependency
  },

  running(id: keyof T["dependencies"]) {
    return {
      id,
      kind: "running",
    } as Dependency
  },
})
