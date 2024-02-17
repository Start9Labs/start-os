import { Effects, ExpectedExports } from "../types"
import { SDKManifest } from "../manifest/ManifestTypes"
import * as D from "./configDependencies"
import { Config, ExtractConfigType } from "./builder/config"
import { Utils, createUtils } from "../util/utils"
import nullIfEmpty from "../util/nullIfEmpty"
import { InterfaceReceipt } from "../interfaces/interfaceReceipt"
import { InterfacesReceipt as InterfacesReceipt } from "../interfaces/setupInterfaces"

declare const dependencyProof: unique symbol
export type DependenciesReceipt = void & {
  [dependencyProof]: never
}

export type Save<
  Store,
  A extends
    | Record<string, any>
    | Config<Record<string, any>, any>
    | Config<Record<string, never>, never>,
  Manifest extends SDKManifest,
> = (options: {
  effects: Effects
  input: ExtractConfigType<A> & Record<string, any>
  utils: Utils<Manifest, Store>
  dependencies: D.ConfigDependencies<Manifest>
}) => Promise<{
  dependenciesReceipt: DependenciesReceipt
  interfacesReceipt: InterfacesReceipt
  restart: boolean
}>
export type Read<
  Manifest extends SDKManifest,
  Store,
  A extends
    | Record<string, any>
    | Config<Record<string, any>, any>
    | Config<Record<string, any>, never>,
> = (options: {
  effects: Effects
  utils: Utils<Manifest, Store>
}) => Promise<void | (ExtractConfigType<A> & Record<string, any>)>
/**
 * We want to setup a config export with a get and set, this
 * is going to be the default helper to setup config, because it will help
 * enforce that we have a spec, write, and reading.
 * @param options
 * @returns
 */
export function setupConfig<
  Store,
  ConfigType extends
    | Record<string, any>
    | Config<any, any>
    | Config<any, never>,
  Manifest extends SDKManifest,
  Type extends Record<string, any> = ExtractConfigType<ConfigType>,
>(
  spec: Config<Type, Store> | Config<Type, never>,
  write: Save<Store, Type, Manifest>,
  read: Read<Manifest, Store, Type>,
) {
  const validator = spec.validator
  return {
    setConfig: (async ({ effects, input }) => {
      if (!validator.test(input)) {
        await console.error(String(validator.errorMessage(input)))
        return { error: "Set config type error for config" }
      }
      await effects.clearBindings()
      await effects.clearNetworkInterfaces()
      const { restart } = await write({
        input: JSON.parse(JSON.stringify(input)),
        effects,
        utils: createUtils(effects),
        dependencies: D.configDependenciesSet<Manifest>(),
      })
      if (restart) {
        await effects.restart()
      }
    }) as ExpectedExports.setConfig,
    getConfig: (async ({ effects }) => {
      const myUtils = createUtils<Manifest, Store>(effects)
      const configValue = nullIfEmpty(
        (await read({ effects, utils: myUtils })) || null,
      )
      return {
        spec: await spec.build({
          effects,
          utils: myUtils as any,
        }),
        config: configValue,
      }
    }) as ExpectedExports.getConfig,
  }
}

export default setupConfig
