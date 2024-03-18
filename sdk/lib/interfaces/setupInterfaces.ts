import { Config } from "../config/builder/config"
import { SDKManifest } from "../manifest/ManifestTypes"
import { AddressInfo, Effects } from "../types"
import { AddressReceipt } from "./AddressReceipt"

export type InterfacesReceipt = Array<AddressInfo[] & AddressReceipt>
export type SetInterfaces<
  Manifest extends SDKManifest,
  Store,
  ConfigInput extends Record<string, any>,
  Output extends InterfacesReceipt,
> = (opts: { effects: Effects; input: null | ConfigInput }) => Promise<Output>
export type SetupInterfaces = <
  Manifest extends SDKManifest,
  Store,
  ConfigInput extends Record<string, any>,
  Output extends InterfacesReceipt,
>(
  config: Config<ConfigInput, Store>,
  fn: SetInterfaces<Manifest, Store, ConfigInput, Output>,
) => SetInterfaces<Manifest, Store, ConfigInput, Output>
export const NO_INTERFACE_CHANGES = [] as InterfacesReceipt
export const setupInterfaces: SetupInterfaces = (_config, fn) => fn
