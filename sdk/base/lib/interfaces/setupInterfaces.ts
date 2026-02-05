/**
 * @module setupInterfaces
 *
 * This module provides utilities for setting up network interfaces (service endpoints)
 * that are exposed to users through the StartOS UI.
 *
 * Service interfaces are the clickable links users see to access your service's
 * web UI, API endpoints, or peer-to-peer connections.
 *
 * @example
 * ```typescript
 * export const setInterfaces = sdk.setupInterfaces(async ({ effects }) => {
 *   const webHost = sdk.MultiHost.of(effects, 'webui')
 *   const webOrigin = await webHost.bindPort(8080, { protocol: 'http' })
 *
 *   await webOrigin.export([
 *     sdk.ServiceInterfaceBuilder.of({
 *       effects,
 *       name: 'Web Interface',
 *       id: 'webui',
 *       description: 'Access the web dashboard',
 *       type: 'ui',
 *     })
 *   ])
 *
 *   return [webOrigin]
 * })
 * ```
 */

import * as T from "../types"
import { once } from "../util"
import { AddressReceipt } from "./AddressReceipt"

/** @internal Type brand for interface update receipt */
declare const UpdateServiceInterfacesProof: unique symbol

/**
 * Receipt type proving that service interfaces have been updated.
 * @internal
 */
export type UpdateServiceInterfacesReceipt = {
  [UpdateServiceInterfacesProof]: never
}

/** Array of address info arrays with receipts, representing all exported interfaces */
export type ServiceInterfacesReceipt = Array<T.AddressInfo[] & AddressReceipt>

/**
 * Function type for setting up service interfaces.
 * @typeParam Output - The specific receipt type returned
 */
export type SetServiceInterfaces<Output extends ServiceInterfacesReceipt> =
  (opts: { effects: T.Effects }) => Promise<Output>

/** Function type for the init-compatible interface updater */
export type UpdateServiceInterfaces = (effects: T.Effects) => Promise<null>

/** Function type for the setupServiceInterfaces helper */
export type SetupServiceInterfaces = <Output extends ServiceInterfacesReceipt>(
  fn: SetServiceInterfaces<Output>,
) => UpdateServiceInterfaces

/**
 * Constant indicating no interface changes are needed.
 * Use this as a return value when interfaces don't need to be updated.
 */
export const NO_INTERFACE_CHANGES = {} as UpdateServiceInterfacesReceipt

/**
 * Creates an interface setup function for use in the initialization pipeline.
 *
 * **Note:** This is exposed via `sdk.setupInterfaces`. See the SDK documentation
 * for full usage examples and parameter descriptions.
 *
 * Internally, this wrapper:
 * - Tracks all bindings and interfaces created during setup
 * - Automatically cleans up stale bindings/interfaces that weren't recreated
 *
 * @see sdk.setupInterfaces for usage documentation
 */
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
