import { ServiceInterfaceType } from "../StartSdk"
import {
  AddressInfo,
  Effects,
  Host,
  HostAddress,
  HostInfo,
  Hostname,
  HostnameInfo,
} from "../types"

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

  urls: UrlString[]
  onionUrls: UrlString[]
  localUrls: UrlString[]
  ipUrls: UrlString[]
  ipv4Urls: UrlString[]
  ipv6Urls: UrlString[]
  nonIpUrls: UrlString[]
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
  /** Information about the host for this binding */
  host: Host
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
function stringifyHostname(hostAddress: null | HostAddress): [Hostname] | [] {
  let base: string

  if (!hostAddress) return []
  if (hostAddress.kind === "onion")
    return [`${hostAddress.address.replace(/\.onion$/, "")}.onion` as Hostname]

  return []
}
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
  host: Host,
  addressInfo: AddressInfo,
): FilledAddressInfo => {
  const toUrl = addressHostToUrl.bind(null, addressInfo)
  const addresses = host.addresses

  return {
    ...addressInfo,
    hostnames: addresses.flatMap(stringifyHostname),
    get onionHostnames() {
      return addresses
        .filter((h) => h.kind === "onion")
        .flatMap(stringifyHostname)
    },
    get localHostnames() {
      return []
      // TODO
      // return addresses
      //   .filter((h) => h.kind === "ip" && h.hostname.kind === "local")
      //   .flatMap(stringifyHostname)
    },
    get ipHostnames() {
      return []
      // TODO
      // return addresses
      //   .filter(
      //     (h) =>
      //       h.kind === "ip" &&
      //       (h.hostname.kind === "ipv4" || h.hostname.kind === "ipv6"),
      //   )
      //   .flatMap(stringifyHostname)
    },
    get ipv4Hostnames() {
      return []
      // TODO
      // return addresses
      //   .filter((h) => h.kind === "ip" && h.hostname.kind === "ipv4")
      //   .flatMap(stringifyHostname)
    },
    get ipv6Hostnames() {
      return []
      // TODO
      // return addresses
      //   .filter((h) => h.kind === "ip" && h.hostname.kind === "ipv6")
      //   .flatMap(stringifyHostname)
    },
    get nonIpHostnames() {
      return []
      // TODO
      // return addresses
      //   .filter(
      //     (h) =>
      //       h.kind === "ip" &&
      //       h.hostname.kind !== "ipv4" &&
      //       h.hostname.kind !== "ipv6",
      //   )
      //   .flatMap(stringifyHostname)
    },
    get urls() {
      return this.hostnames.map(toUrl)
    },
    get onionUrls() {
      return this.onionHostnames.map(toUrl)
    },
    get localUrls() {
      return this.localHostnames.map(toUrl)
    },
    get ipUrls() {
      return this.ipHostnames.map(toUrl)
    },
    get ipv4Urls() {
      return this.ipv4Hostnames.map(toUrl)
    },
    get ipv6Urls() {
      return this.ipv6Hostnames.map(toUrl)
    },
    get nonIpUrls() {
      return this.nonIpHostnames.map(toUrl)
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
  packageId: string | null
  callback: () => void
}) => {
  const serviceInterfaceValue = await effects.getServiceInterface({
    serviceInterfaceId: id,
    packageId,
    callback,
  })
  const hostId = serviceInterfaceValue.addressInfo.hostId
  const host = await effects.getHostInfo({
    packageId,
    kind: null,
    hostId,
    callback,
  })
  const primaryUrl = await effects
    .getPrimaryUrl({
      serviceInterfaceId: id,
      packageId,
      hostId,
      callback,
    })
    .catch((e) => null)

  const interfaceFilled: ServiceInterfaceFilled = {
    ...serviceInterfaceValue,
    primaryUrl: primaryUrl,
    host,
    addressInfo: filledAddress(host, serviceInterfaceValue.addressInfo),
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
    readonly opts: { id: string; packageId: string | null },
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
  opts: { id: string; packageId: string | null },
) {
  return new GetServiceInterface(effects, opts)
}
