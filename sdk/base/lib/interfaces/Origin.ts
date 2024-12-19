import { AddressInfo } from "../types"
import { AddressReceipt } from "./AddressReceipt"
import { Host, Scheme } from "./Host"
import { ServiceInterfaceBuilder } from "./ServiceInterfaceBuilder"

export class Origin<T extends Host> {
  constructor(
    readonly host: T,
    readonly internalPort: number,
    readonly scheme: string | null,
    readonly sslScheme: string | null,
  ) {}

  build({ username, path, search, schemeOverride }: BuildOptions): AddressInfo {
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
   * @description A function to register a group of origins (<PROTOCOL> :// <HOSTNAME> : <PORT>) with StartOS
   *
   *   The returned addressReceipt serves as proof that the addresses were registered
   *
   * @param addressInfo
   * @returns
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
        search,
        schemeOverride,
        masked,
      } = serviceInterface.options

      const addressInfo = this.build({
        username,
        path,
        search,
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

type BuildOptions = {
  schemeOverride: { ssl: Scheme; noSsl: Scheme } | null
  username: string | null
  path: string
  search: Record<string, string>
}
