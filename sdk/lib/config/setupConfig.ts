import * as T from "../types"

import * as D from "./configDependencies"
import { Config, ExtractConfigType } from "./builder/config"
import nullIfEmpty from "../util/nullIfEmpty"
import { InterfacesReceipt as InterfacesReceipt } from "../interfaces/setupInterfaces"

declare const dependencyProof: unique symbol
export type DependenciesReceipt = void & {
  [dependencyProof]: never
}

export type Save<
  A extends
    | Record<string, any>
    | Config<Record<string, any>, any>
    | Config<Record<string, never>, never>,
> = (options: {
  effects: T.Effects
  input: ExtractConfigType<A> & Record<string, any>
}) => Promise<{
  dependenciesReceipt: DependenciesReceipt
  interfacesReceipt: InterfacesReceipt
  restart: boolean
}>
export type Read<
  Manifest extends T.Manifest,
  Store,
  A extends
    | Record<string, any>
    | Config<Record<string, any>, any>
    | Config<Record<string, any>, never>,
> = (options: {
  effects: T.Effects
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
  Manifest extends T.Manifest,
  Type extends Record<string, any> = ExtractConfigType<ConfigType>,
>(
  spec: Config<Type, Store> | Config<Type, never>,
  write: Save<Type>,
  read: Read<Manifest, Store, Type>,
) {
  const validator = spec.validator
  return {
    setConfig: (async ({ effects, input }) => {
      if (!validator.test(input)) {
        await console.error(
          new Error(validator.errorMessage(input)?.toString()),
        )
        return { error: "Set config type error for config" }
      }
      await effects.clearBindings()
      await effects.clearServiceInterfaces()
      const { restart } = await write({
        input: JSON.parse(JSON.stringify(input)) as any,
        effects,
      })
      if (restart) {
        await effects.restart()
      }
    }) as T.ExpectedExports.setConfig,
    getConfig: (async ({ effects }) => {
      const configValue = nullIfEmpty((await read({ effects })) || null)
      return {
        spec: await spec.build({
          effects,
        }),
        config: configValue,
      }
    }) as T.ExpectedExports.getConfig,
  }
}

export default setupConfig
