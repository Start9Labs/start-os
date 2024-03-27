import { AddressInfo } from "../types"
import { AddressReceipt } from "./AddressReceipt"
import { Host, BindOptions, Scheme } from "./Host"
import { ServiceInterfaceBuilder } from "./ServiceInterfaceBuilder"

export class Origin<T extends Host> {
  constructor(
    readonly host: T,
    readonly options: BindOptions,
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
      bindOptions: {
        ...this.options,
        scheme: schemeOverride ? schemeOverride.noSsl : this.options.scheme,
        addSsl: this.options.addSsl
          ? {
              ...this.options.addSsl,
              scheme: schemeOverride
                ? schemeOverride.ssl
                : this.options.addSsl.scheme,
            }
          : null,
      },
      suffix: `${path}${qp}`,
      username,
    }
  }

  /**
   * A function to register a group of origins (<PROTOCOL> :// <HOSTNAME> : <PORT>) with StartOS
   *
   * The returned addressReceipt serves as proof that the addresses were registered
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
        hasPrimary,
        disabled,
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
        hasPrimary,
        disabled,
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
