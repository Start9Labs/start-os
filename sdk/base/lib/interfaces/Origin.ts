/**
 * @module Origin
 *
 * The Origin class represents a bound network origin (protocol + host + port)
 * that can be used to export service interfaces.
 */

import { AddressInfo } from "../types"
import { AddressReceipt } from "./AddressReceipt"
import { MultiHost, Scheme } from "./Host"
import { ServiceInterfaceBuilder } from "./ServiceInterfaceBuilder"

/**
 * Represents a network origin (protocol://host:port) created from a MultiHost binding.
 *
 * Origins are created by calling `MultiHost.bindPort()` and can be used to
 * export one or more service interfaces that share the same underlying connection.
 *
 * @example
 * ```typescript
 * // Create origin from host binding
 * const origin = await webHost.bindPort(8080, { protocol: 'http' })
 *
 * // Export multiple interfaces sharing this origin
 * await origin.export([
 *   sdk.ServiceInterfaceBuilder.of({
 *     effects,
 *     name: 'Web UI',
 *     id: 'ui',
 *     type: 'ui',
 *     description: 'Main web interface',
 *   }),
 *   sdk.ServiceInterfaceBuilder.of({
 *     effects,
 *     name: 'REST API',
 *     id: 'api',
 *     type: 'api',
 *     description: 'JSON API endpoint',
 *     path: '/api/v1',
 *   }),
 * ])
 * ```
 */
export class Origin {
  constructor(
    /** The MultiHost this origin was created from */
    readonly host: MultiHost,
    /** The internal port this origin is bound to */
    readonly internalPort: number,
    /** The protocol scheme (e.g., "http", "ssh") or null */
    readonly scheme: string | null,
    /** The SSL variant scheme (e.g., "https", "wss") or null */
    readonly sslScheme: string | null,
  ) {}

  /**
   * Builds an AddressInfo object for this origin with the specified options.
   * Used internally by `export()` but can be called directly for custom use cases.
   *
   * @param options - Build options including path, query params, username, and scheme overrides
   * @returns AddressInfo object describing the complete address
   * @internal
   */
  build({
    username,
    path,
    query: search,
    schemeOverride,
  }: BuildOptions): AddressInfo {
    const qpEntries = Object.entries(search)
      .map(
        ([key, val]) => `${encodeURIComponent(key)}=${encodeURIComponent(val)}`,
      )
      .join("&")

    const qp = qpEntries.length ? `?${qpEntries}` : ""

    return {
      hostId: this.host.options.id,
      internalPort: this.internalPort,
      scheme: schemeOverride ? schemeOverride.noSsl : this.scheme,
      sslScheme: schemeOverride ? schemeOverride.ssl : this.sslScheme,
      suffix: `${path}${qp}`,
      username,
    }
  }

  /**
   * Exports one or more service interfaces for this origin.
   *
   * Each service interface becomes a clickable link in the StartOS UI.
   * Multiple interfaces can share the same origin but have different
   * names, descriptions, types, paths, or query parameters.
   *
   * @param serviceInterfaces - Array of ServiceInterfaceBuilder objects to export
   * @returns Promise resolving to array of AddressInfo with an AddressReceipt
   *
   * @example
   * ```typescript
   * await origin.export([
   *   sdk.ServiceInterfaceBuilder.of({
   *     effects,
   *     name: 'Admin Panel',
   *     id: 'admin',
   *     type: 'ui',
   *     description: 'Administrator dashboard',
   *     path: '/admin',
   *   })
   * ])
   * ```
   */
  async export(
    serviceInterfaces: ServiceInterfaceBuilder[],
  ): Promise<AddressInfo[] & AddressReceipt> {
    const addressesInfo = []
    for (let serviceInterface of serviceInterfaces) {
      const {
        name,
        description,
        id,
        type,
        username,
        path,
        query: search,
        schemeOverride,
        masked,
      } = serviceInterface.options

      const addressInfo = this.build({
        username,
        path,
        query: search,
        schemeOverride,
      })

      await serviceInterface.options.effects.exportServiceInterface({
        id,
        name,
        description,
        addressInfo,
        type,
        masked,
      })

      addressesInfo.push(addressInfo)
    }

    return addressesInfo as AddressInfo[] & AddressReceipt
  }
}

/**
 * Options for building an address from an Origin.
 * @internal
 */
type BuildOptions = {
  /** Override the default schemes for SSL and non-SSL connections */
  schemeOverride: { ssl: Scheme; noSsl: Scheme } | null
  /** Optional username for basic auth URLs */
  username: string | null
  /** URL path (e.g., "/api/v1") */
  path: string
  /** Query parameters to append to the URL */
  query: Record<string, string>
}
