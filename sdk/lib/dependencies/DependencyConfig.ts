import * as T from "../types"
import { deepEqual } from "../util/deepEqual"
import { deepMerge } from "../util/deepMerge"

export type Update<QueryResults, RemoteConfig> = (options: {
  remoteConfig: RemoteConfig
  queryResults: QueryResults
}) => Promise<RemoteConfig>

export class DependencyConfig<
  Manifest extends T.Manifest,
  Store,
  Input extends Record<string, any>,
  RemoteConfig extends Record<string, any>,
> {
  static defaultUpdate = async (options: {
    queryResults: unknown
    remoteConfig: unknown
  }): Promise<unknown> => {
    return deepMerge({}, options.remoteConfig, options.queryResults || {})
  }
  constructor(
    readonly dependencyConfig: (options: {
      effects: T.Effects
      localConfig: Input
    }) => Promise<void | T.DeepPartial<RemoteConfig>>,
    readonly update: Update<
      void | T.DeepPartial<RemoteConfig>,
      RemoteConfig
    > = DependencyConfig.defaultUpdate as any,
  ) {}

  async query(options: { effects: T.Effects; localConfig: unknown }) {
    return this.dependencyConfig({
      localConfig: options.localConfig as Input,
      effects: options.effects,
    })
  }
}
