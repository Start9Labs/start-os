import {
  DependencyConfig as DependencyConfigType,
  DeepPartial,
  Effects,
} from "../types"
import { Utils, createUtils } from "../util/utils"
import { deepEqual } from "../util/deepEqual"
import { deepMerge } from "../util/deepMerge"
import { SDKManifest } from "../manifest/ManifestTypes"

export type Update<QueryResults, RemoteConfig> = (options: {
  remoteConfig: RemoteConfig
  queryResults: QueryResults
}) => Promise<RemoteConfig>

export class DependencyConfig<
  Manifest extends SDKManifest,
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
      effects: Effects
      localConfig: Input
      utils: Utils<Manifest, Store>
    }) => Promise<void | DeepPartial<RemoteConfig>>,
    readonly update: Update<
      void | DeepPartial<RemoteConfig>,
      RemoteConfig
    > = DependencyConfig.defaultUpdate as any,
  ) {}

  async query(options: { effects: Effects; localConfig: unknown }) {
    return this.dependencyConfig({
      localConfig: options.localConfig as Input,
      effects: options.effects,
      utils: createUtils<Manifest, Store>(options.effects),
    })
  }
}
