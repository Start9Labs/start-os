import { SDKManifest } from "../manifest/ManifestTypes"
import { Dependencies } from "../types"

export type ConfigDependencies<T extends SDKManifest> = {
  exists(id: keyof T["dependencies"]): Dependencies[number]
  running(
    id: keyof T["dependencies"],
    healthChecks: string[],
  ): Dependencies[number]
}

export const configDependenciesSet = <
  T extends SDKManifest,
>(): ConfigDependencies<T> => ({
  exists(id: keyof T["dependencies"]) {
    return {
      id,
      kind: "exists",
    } as Dependencies[number]
  },

  running(id: keyof T["dependencies"], healthChecks: string[]) {
    return {
      id,
      kind: "running",
      healthChecks,
    } as Dependencies[number]
  },
})
