import { Config } from "../config/builder/config"

import * as T from "../types"
import { DependencyConfig } from "./DependencyConfig"

export function setupDependencyConfig<
  Store,
  Input extends Record<string, any>,
  Manifest extends T.Manifest,
>(
  _config: Config<Input, Store> | Config<Input, never>,
  autoConfigs: {
    [key in keyof Manifest["dependencies"] & string]: DependencyConfig<
      Manifest,
      Store,
      Input,
      any
    > | null
  },
): T.ExpectedExports.dependencyConfig {
  return autoConfigs
}
