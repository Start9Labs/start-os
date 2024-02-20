import { AddressInfo, Effects, Hostname, ServiceInterface } from "../types"
import * as regexes from "./regexes"
import { ServiceInterfaceType } from "./utils"

export type UrlString = string
export type HostId = string

const getHostnameRegex = /^(\w+:\/\/)?([^\/\:]+)(:\d{1,3})?(\/)?/
export const getHostname = (url: string): Hostname | null => {
  const founds = url.match(getHostnameRegex)?.[2]
  if (!founds) return null
  const parts = founds.split("@")
  const last = parts[parts.length - 1] as Hostname | null
  return last
}

export type Filled = {
  hostnames: Hostname[]
  onionHostnames: Hostname[]
  localHostnames: Hostname[]
  ipHostnames: Hostname[]
  ipv4Hostnames: Hostname[]
  ipv6Hostnames: Hostname[]
  nonIpHostnames: Hostname[]
  allHostnames: Hostname[]

  urls: UrlString[]
  onionUrls: UrlString[]
  localUrls: UrlString[]
  ipUrls: UrlString[]
  ipv4Urls: UrlString[]
  ipv6Urls: UrlString[]
  nonIpUrls: UrlString[]
  allUrls: UrlString[]
}
export type FilledAddressInfo = AddressInfo & Filled
export type ServiceInterfaceFilled = {
  id: string
  /** The title of this field to be displayed */
  name: string
  /** Human readable description, used as tooltip usually */
  description: string
  /** Whether or not the interface has a primary URL */
  hasPrimary: boolean
  /** Whether or not the interface disabled */
  disabled: boolean
  /** Whether or not to mask the URIs for this interface. Useful if the URIs contain sensitive information, such as a password, macaroon, or API key */
  masked: boolean
  /** URI information */
  addressInfo: FilledAddressInfo
  /** Indicates if we are a ui/p2p/api for the kind of interface that this is representing */
  type: ServiceInterfaceType
  /** The primary hostname for the service, as chosen by the user */
  primaryHostname: Hostname | null
  /** The primary URL for the service, as chosen by the user */
  primaryUrl: UrlString | null
}
const either =
  <A>(...args: ((a: A) => boolean)[]) =>
  (a: A) =>
    args.some((x) => x(a))
const negate =
  <A>(fn: (a: A) => boolean) =>
  (a: A) =>
    !fn(a)
const unique = <A>(values: A[]) => Array.from(new Set(values))
const addressHostToUrl = (
  { bindOptions, username, suffix }: AddressInfo,
  host: Hostname,
): UrlString => {
  const scheme = host.endsWith(".onion")
    ? bindOptions.scheme
    : bindOptions.addSsl
      ? bindOptions.addSsl.scheme
      : bindOptions.scheme // TODO: encode whether hostname transport is "secure"?
  return `${scheme ? `${scheme}//` : ""}${
    username ? `${username}@` : ""
  }${host}${suffix}`
}
export const filledAddress = (
  hostnames: Hostname[],
  addressInfo: AddressInfo,
): FilledAddressInfo => {
  const toUrl = addressHostToUrl.bind(null, addressInfo)
  return {
    ...addressInfo,
    hostnames,
    get onionHostnames() {
      return hostnames.filter(regexes.torHostname.test)
    },
    get localHostnames() {
      return hostnames.filter(regexes.localHostname.test)
    },
    get ipHostnames() {
      return hostnames.filter(either(regexes.ipv4.test, regexes.ipv6.test))
    },
    get ipv4Hostnames() {
      return hostnames.filter(regexes.ipv4.test)
    },
    get ipv6Hostnames() {
      return hostnames.filter(regexes.ipv6.test)
    },
    get nonIpHostnames() {
      return hostnames.filter(
        negate(either(regexes.ipv4.test, regexes.ipv6.test)),
      )
    },
    allHostnames: hostnames,
    get urls() {
      return hostnames.map(toUrl)
    },
    get onionUrls() {
      return hostnames.filter(regexes.torHostname.test).map(toUrl)
    },
    get localUrls() {
      return hostnames.filter(regexes.localHostname.test).map(toUrl)
    },
    get ipUrls() {
      return hostnames
        .filter(either(regexes.ipv4.test, regexes.ipv6.test))
        .map(toUrl)
    },
    get ipv4Urls() {
      return hostnames.filter(regexes.ipv4.test).map(toUrl)
    },
    get ipv6Urls() {
      return hostnames.filter(regexes.ipv6.test).map(toUrl)
    },
    get nonIpUrls() {
      return hostnames
        .filter(negate(either(regexes.ipv4.test, regexes.ipv6.test)))
        .map(toUrl)
    },
    get allUrls() {
      return hostnames.map(toUrl)
    },
  }
}

const makeInterfaceFilled = async ({
  effects,
  id,
  packageId,
  callback,
}: {
  effects: Effects
  id: string
  packageId: string | undefined
  callback: () => void
}) => {
  const serviceInterfaceValue = await effects.getServiceInterface({
    serviceInterfaceId: id,
    packageId,
    callback,
  })
  const hostIdRecord = await effects.getHostnames({
    packageId,
    hostId: serviceInterfaceValue.addressInfo.hostId,
    callback,
  })
  const primaryUrl = await effects.getPrimaryUrl({
    serviceInterfaceId: id,
    packageId,
    callback,
  })

  const interfaceFilled: ServiceInterfaceFilled = {
    ...serviceInterfaceValue,
    primaryUrl: primaryUrl,
    addressInfo: filledAddress(hostIdRecord, serviceInterfaceValue.addressInfo),
    get primaryHostname() {
      if (primaryUrl == null) return null
      return getHostname(primaryUrl)
    },
  }
  return interfaceFilled
}

export class GetServiceInterface {
  constructor(
    readonly effects: Effects,
    readonly opts: { id: string; packageId?: string },
  ) {}

  /**
   * Returns the value of Store at the provided path. Restart the service if the value changes
   */
  async const() {
    const { id, packageId } = this.opts
    const callback = this.effects.restart
    const interfaceFilled: ServiceInterfaceFilled = await makeInterfaceFilled({
      effects: this.effects,
      id,
      packageId,
      callback,
    })

    return interfaceFilled
  }
  /**
   * Returns the value of ServiceInterfacesFilled at the provided path. Does nothing if the value changes
   */
  async once() {
    const { id, packageId } = this.opts
    const callback = () => {}
    const interfaceFilled: ServiceInterfaceFilled = await makeInterfaceFilled({
      effects: this.effects,
      id,
      packageId,
      callback,
    })

    return interfaceFilled
  }

  /**
   * Watches the value of ServiceInterfacesFilled at the provided path. Takes a custom callback function to run whenever the value changes
   */
  async *watch() {
    const { id, packageId } = this.opts
    while (true) {
      let callback: () => void = () => {}
      const waitForNext = new Promise<void>((resolve) => {
        callback = resolve
      })
      yield await makeInterfaceFilled({
        effects: this.effects,
        id,
        packageId,
        callback,
      })
      await waitForNext
    }
  }
}
export function getServiceInterface(
  effects: Effects,
  opts: { id: string; packageId?: string },
) {
  return new GetServiceInterface(effects, opts)
}
