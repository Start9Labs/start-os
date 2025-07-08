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
export type UpdateServiceInterfaces = (effects: T.Effects) => Promise<null>
export type SetupServiceInterfaces = <Output extends ServiceInterfacesReceipt>(
  fn: SetServiceInterfaces<Output>,
) => UpdateServiceInterfaces
export const NO_INTERFACE_CHANGES = {} as UpdateServiceInterfacesReceipt
export const setupServiceInterfaces: SetupServiceInterfaces = <
  Output extends ServiceInterfacesReceipt,
>(
  fn: SetServiceInterfaces<Output>,
) => {
  return (async (effects: T.Effects) => {
    const bindings: T.BindId[] = []
    const interfaces: T.ServiceInterfaceId[] = []
    await fn({
      effects: {
        ...effects,
        bind: (params: T.BindParams) => {
          bindings.push({ id: params.id, internalPort: params.internalPort })
          return effects.bind(params)
        },
        exportServiceInterface: (params: T.ExportServiceInterfaceParams) => {
          interfaces.push(params.id)
          return effects.exportServiceInterface(params)
        },
      },
    })
    await effects.clearBindings({ except: bindings })
    await effects.clearServiceInterfaces({ except: interfaces })
    return null
  }) as UpdateServiceInterfaces
}
