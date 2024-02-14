import { Address, Effects, HostName, NetworkInterface } from "../types"
import * as regexes from "./regexes"
import { NetworkInterfaceType } from "./utils"

export type UrlString = string
export type HostId = string

const getHostnameRegex = /^(\w+:\/\/)?([^\/\:]+)(:\d{1,3})?(\/)?/
export const getHostname = (url: string): HostName | null => {
  const founds = url.match(getHostnameRegex)?.[2]
  if (!founds) return null
  const parts = founds.split("@")
  const last = parts[parts.length - 1] as HostName | null
  return last
}

export type Filled = {
  hostnames: HostName[]
  onionHostnames: HostName[]
  localHostnames: HostName[]
  ipHostnames: HostName[]
  ipv4Hostnames: HostName[]
  ipv6Hostnames: HostName[]
  nonIpHostnames: HostName[]
  allHostnames: HostName[]

  urls: UrlString[]
  onionUrls: UrlString[]
  localUrls: UrlString[]
  ipUrls: UrlString[]
  ipv4Urls: UrlString[]
  ipv6Urls: UrlString[]
  nonIpUrls: UrlString[]
  allUrls: UrlString[]
}
export type FilledAddress = Address & Filled
export type NetworkInterfaceFilled = {
  interfaceId: string
  /** The title of this field to be displayed */
  name: string
  /** Human readable description, used as tooltip usually */
  description: string
  /** Whether or not the interface has a primary URL */
  hasPrimary: boolean
  /** Whether or not the interface disabled */
  disabled: boolean
  /** All URIs */
  addresses: FilledAddress[]

  /** Indicates if we are a ui/ p2p/ api/ other for the kind of interface that this is representing */
  type: NetworkInterfaceType

  primaryHostname: HostName | null
  primaryUrl: UrlString | null
} & Filled
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
  { options, username, suffix }: Address,
  host: HostName,
): UrlString => {
  const scheme = host.endsWith(".onion")
    ? options.scheme
    : options.addSsl
      ? options.addSsl.scheme
      : options.scheme // TODO: encode whether hostname transport is "secure"?
  return `${scheme ? `${scheme}//` : ""}${
    username ? `${username}@` : ""
  }${host}${suffix}`
}
export const filledAddress = (
  mapHostnames: {
    [hostId: string]: HostName[]
  },
  address: Address,
): FilledAddress => {
  const toUrl = addressHostToUrl.bind(null, address)
  const hostnames = mapHostnames[address.hostId] ?? []
  return {
    ...address,
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

export const networkInterfaceFilled = (
  interfaceValue: NetworkInterface,
  primaryUrl: UrlString | null,
  addresses: FilledAddress[],
): NetworkInterfaceFilled => {
  return {
    ...interfaceValue,
    addresses,
    get hostnames() {
      return unique(addresses.flatMap((x) => x.hostnames))
    },
    get onionHostnames() {
      return unique(addresses.flatMap((x) => x.onionHostnames))
    },
    get localHostnames() {
      return unique(addresses.flatMap((x) => x.localHostnames))
    },
    get ipHostnames() {
      return unique(addresses.flatMap((x) => x.ipHostnames))
    },
    get ipv4Hostnames() {
      return unique(addresses.flatMap((x) => x.ipv4Hostnames))
    },
    get ipv6Hostnames() {
      return unique(addresses.flatMap((x) => x.ipv6Hostnames))
    },
    get nonIpHostnames() {
      return unique(addresses.flatMap((x) => x.nonIpHostnames))
    },
    get allHostnames() {
      return unique(addresses.flatMap((x) => x.allHostnames))
    },
    get primaryHostname() {
      if (primaryUrl == null) return null
      return getHostname(primaryUrl)
    },
    get urls() {
      return unique(addresses.flatMap((x) => x.urls))
    },
    get onionUrls() {
      return unique(addresses.flatMap((x) => x.onionUrls))
    },
    get localUrls() {
      return unique(addresses.flatMap((x) => x.localUrls))
    },
    get ipUrls() {
      return unique(addresses.flatMap((x) => x.ipUrls))
    },
    get ipv4Urls() {
      return unique(addresses.flatMap((x) => x.ipv4Urls))
    },
    get ipv6Urls() {
      return unique(addresses.flatMap((x) => x.ipv6Urls))
    },
    get nonIpUrls() {
      return unique(addresses.flatMap((x) => x.nonIpUrls))
    },
    get allUrls() {
      return unique(addresses.flatMap((x) => x.allUrls))
    },
    primaryUrl,
  }
}
const makeInterfaceFilled = async ({
  effects,
  interfaceId,
  packageId,
  callback,
}: {
  effects: Effects
  interfaceId: string
  packageId: string | undefined
  callback: () => void
}) => {
  const interfaceValue = await effects.getInterface({
    interfaceId,
    packageId,
    callback,
  })
  const hostIdsRecord = Promise.all(
    unique(interfaceValue.addresses.map((x) => x.hostId)).map(
      async (hostId) =>
        [
          hostId,
          await effects.getHostnames({
            packageId,
            hostId,
            callback,
          }),
        ] as const,
    ),
  )
  const primaryUrl = effects.getPrimaryUrl({
    interfaceId,
    packageId,
    callback,
  })

  const fillAddress = filledAddress.bind(
    null,
    Object.fromEntries(await hostIdsRecord),
  )
  const interfaceFilled: NetworkInterfaceFilled = networkInterfaceFilled(
    interfaceValue,
    await primaryUrl,
    interfaceValue.addresses.map(fillAddress),
  )
  return interfaceFilled
}

export class GetNetworkInterface {
  constructor(
    readonly effects: Effects,
    readonly opts: { interfaceId: string; packageId?: string },
  ) {}

  /**
   * Returns the value of Store at the provided path. Restart the service if the value changes
   */
  async const() {
    const { interfaceId, packageId } = this.opts
    const callback = this.effects.restart
    const interfaceFilled: NetworkInterfaceFilled = await makeInterfaceFilled({
      effects: this.effects,
      interfaceId,
      packageId,
      callback,
    })

    return interfaceFilled
  }
  /**
   * Returns the value of NetworkInterfacesFilled at the provided path. Does nothing if the value changes
   */
  async once() {
    const { interfaceId, packageId } = this.opts
    const callback = () => {}
    const interfaceFilled: NetworkInterfaceFilled = await makeInterfaceFilled({
      effects: this.effects,
      interfaceId,
      packageId,
      callback,
    })

    return interfaceFilled
  }

  /**
   * Watches the value of NetworkInterfacesFilled at the provided path. Takes a custom callback function to run whenever the value changes
   */
  async *watch() {
    const { interfaceId, packageId } = this.opts
    while (true) {
      let callback: () => void = () => {}
      const waitForNext = new Promise<void>((resolve) => {
        callback = resolve
      })
      yield await makeInterfaceFilled({
        effects: this.effects,
        interfaceId,
        packageId,
        callback,
      })
      await waitForNext
    }
  }
}
export function getNetworkInterface(
  effects: Effects,
  opts: { interfaceId: string; packageId?: string },
) {
  return new GetNetworkInterface(effects, opts)
}
