import * as T from "../types"
import { once } from "../util"
import { AddressReceipt } from "./AddressReceipt"

declare const UpdateServiceInterfacesProof: unique symbol
export type UpdateServiceInterfacesReceipt = {
  [UpdateServiceInterfacesProof]: never
}

export type ServiceInterfacesReceipt = Array<T.AddressInfo[] & AddressReceipt>
export type SetServiceInterfaces<Output extends ServiceInterfacesReceipt> =
  (opts: { effects: T.Effects }) => Promise<Output>
export type UpdateServiceInterfaces<Output extends ServiceInterfacesReceipt> =
  (opts: {
    effects: T.Effects
  }) => Promise<Output & UpdateServiceInterfacesReceipt>
export type SetupServiceInterfaces = <Output extends ServiceInterfacesReceipt>(
  fn: SetServiceInterfaces<Output>,
) => UpdateServiceInterfaces<Output>
export const NO_INTERFACE_CHANGES = {} as UpdateServiceInterfacesReceipt
export const setupServiceInterfaces: SetupServiceInterfaces = <
  Output extends ServiceInterfacesReceipt,
>(
  fn: SetServiceInterfaces<Output>,
) => {
  const cell = {
    updater: (async (options: { effects: T.Effects }) =>
      [] as any as Output) as UpdateServiceInterfaces<Output>,
  }
  cell.updater = (async (options: { effects: T.Effects }) => {
    const childEffects = options.effects.child("setupInterfaces")
    childEffects.constRetry = once(() => {
      cell.updater({ effects: options.effects })
    })
    const bindings: T.BindId[] = []
    const interfaces: T.ServiceInterfaceId[] = []
    const res = await fn({
      effects: {
        ...childEffects,
        bind: (params: T.BindParams) => {
          bindings.push({ id: params.id, internalPort: params.internalPort })
          return childEffects.bind(params)
        },
        exportServiceInterface: (params: T.ExportServiceInterfaceParams) => {
          interfaces.push(params.id)
          return childEffects.exportServiceInterface(params)
        },
      },
    })
    await options.effects.clearBindings({ except: bindings })
    await options.effects.clearServiceInterfaces({ except: interfaces })
    return res
  }) as UpdateServiceInterfaces<Output>
  return cell.updater
}
