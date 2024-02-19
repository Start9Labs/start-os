import { AddressInfo, Effects } from "../types"
import { ServiceInterfaceType } from "../util/utils"
import { AddressReceipt } from "./AddressReceipt"
import { Host } from "./Host"
import { Origin } from "./Origin"

/**
 * A helper class for creating a Network Interface
 *
 * Network Interfaces are collections of web addresses that expose the same API or other resource,
 * display to the user with under a common name and description.
 *
 * All URIs on an interface inherit the same ui: bool, basic auth credentials, path, and search (query) params
 *
 * @param options
 * @returns
 */
export class ServiceInterfaceBuilder {
  constructor(
    readonly options: {
      effects: Effects
      name: string
      id: string
      description: string
      hasPrimary: boolean
      disabled: boolean
      type: ServiceInterfaceType
      username: null | string
      path: string
      search: Record<string, string>
    },
  ) {}

  /**
   * A function to register a group of origins (<PROTOCOL> :// <HOSTNAME> : <PORT>) with StartOS
   *
   * The returned addressReceipt serves as proof that the addresses were registered
   *
   * @param addressInfo
   * @returns
   */
  async export<OriginForHost extends Origin<Host>>(
    origin: OriginForHost,
  ): Promise<AddressInfo & AddressReceipt> {
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
    } = this.options

    const addressInfo = origin.build({ username, path, search, scheme: null })

    await this.options.effects.exportServiceInterface({
      id,
      name,
      description,
      hasPrimary,
      disabled,
      addressInfo,
      type,
    })

    return addressInfo as AddressInfo & AddressReceipt
  }
}
