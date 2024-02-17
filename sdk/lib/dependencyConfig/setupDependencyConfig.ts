import { Config } from "../config/builder/config"
import { SDKManifest } from "../manifest/ManifestTypes"
import { ExpectedExports } from "../types"
import { DependencyConfig } from "./DependencyConfig"

export function setupDependencyConfig<
  Store,
  Input extends Record<string, any>,
  Manifest extends SDKManifest,
>(
  _config: Config<Input, Store> | Config<Input, never>,
  autoConfigs: {
    [key in keyof Manifest["dependencies"] & string]: DependencyConfig<
      Manifest,
      Store,
      Input,
      any
    >
  },
): ExpectedExports.dependencyConfig {
  return autoConfigs
}
