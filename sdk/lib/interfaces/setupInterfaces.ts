import * as T from "../types"
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
export const setupServiceInterfaces: SetupServiceInterfaces =
  (fn) => async (opts: { effects: T.Effects }) => {
    const bindings: T.BindId[] = []
    const interfaces: T.ServiceInterfaceId[] = []
    const res = await fn({
      effects: {
        ...opts.effects,
        bind: (params: T.BindParams) => {
          bindings.push({ id: params.id, internalPort: params.internalPort })
          return opts.effects.bind(params)
        },
        exportServiceInterface: (params: T.ExportServiceInterfaceParams) => {
          interfaces.push(params.id)
          return opts.effects.exportServiceInterface(params)
        },
      },
    })
    await opts.effects.clearBindings({ except: bindings })
    await opts.effects.clearServiceInterfaces({ except: interfaces })
    return res as typeof res & UpdateServiceInterfacesReceipt
  }
