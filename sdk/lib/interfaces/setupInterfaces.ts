import { Config } from "../config/builder/config"

import * as T from "../types"
import { AddressReceipt } from "./AddressReceipt"

export type InterfacesReceipt = Array<T.AddressInfo[] & AddressReceipt>
export type SetInterfaces<
  Manifest extends T.Manifest,
  Store,
  ConfigInput extends Record<string, any>,
  Output extends InterfacesReceipt,
> = (opts: { effects: T.Effects; input: null | ConfigInput }) => Promise<Output>
export type SetupInterfaces = <
  Manifest extends T.Manifest,
  Store,
  ConfigInput extends Record<string, any>,
  Output extends InterfacesReceipt,
>(
  config: Config<ConfigInput, Store>,
  fn: SetInterfaces<Manifest, Store, ConfigInput, Output>,
) => SetInterfaces<Manifest, Store, ConfigInput, Output>
export const NO_INTERFACE_CHANGES = [] as InterfacesReceipt
export const setupInterfaces: SetupInterfaces = (_config, fn) => fn
