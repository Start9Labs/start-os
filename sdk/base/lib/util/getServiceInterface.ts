import { ServiceInterfaceType } from "../types"
import { knownProtocols } from "../interfaces/Host"
import { AddressInfo, Host, Hostname, HostnameInfo } from "../types"
import { Effects } from "../Effects"

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
  hostnames: HostnameInfo[]
  publicHostnames: HostnameInfo[]
  onionHostnames: HostnameInfo[]
  localHostnames: HostnameInfo[]
  ipHostnames: HostnameInfo[]
  ipv4Hostnames: HostnameInfo[]
  ipv6Hostnames: HostnameInfo[]
  nonIpHostnames: HostnameInfo[]

  urls: UrlString[]
  publicUrls: UrlString[]
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
  /** Whether or not to mask the URIs for this interface. Useful if the URIs contain sensitive information, such as a password, macaroon, or API key */
  masked: boolean
  /** Information about the host for this binding */
  host: Host | null
  /** URI information */
  addressInfo: FilledAddressInfo | null
  /** Indicates if we are a ui/p2p/api for the kind of interface that this is representing */
  type: ServiceInterfaceType
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
export const addressHostToUrl = (
  { scheme, sslScheme, username, suffix }: AddressInfo,
  host: HostnameInfo,
): UrlString[] => {
  const res = []
  const fmt = (scheme: string | null, host: HostnameInfo, port: number) => {
    const excludePort =
      scheme &&
      scheme in knownProtocols &&
      port === knownProtocols[scheme as keyof typeof knownProtocols].defaultPort
    let hostname
    if (host.kind === "onion") {
      hostname = host.hostname.value
    } else if (host.kind === "ip") {
      if (host.hostname.kind === "domain") {
        hostname = `${host.hostname.subdomain ? `${host.hostname.subdomain}.` : ""}${host.hostname.domain}`
      } else if (host.hostname.kind === "ipv6") {
        hostname = host.hostname.value.startsWith("fe80::")
          ? `[${host.hostname.value}%${host.hostname.scopeId}]`
          : `[${host.hostname.value}]`
      } else {
        hostname = host.hostname.value
      }
    }
    return `${scheme ? `${scheme}://` : ""}${
      username ? `${username}@` : ""
    }${hostname}${excludePort ? "" : `:${port}`}${suffix}`
  }
  if (host.hostname.sslPort !== null) {
    res.push(fmt(sslScheme, host, host.hostname.sslPort))
  }
  if (host.hostname.port !== null) {
    res.push(fmt(scheme, host, host.hostname.port))
  }

  return res
}

export const filledAddress = (
  host: Host,
  addressInfo: AddressInfo,
): FilledAddressInfo => {
  const toUrl = addressHostToUrl.bind(null, addressInfo)
  const hostnames = host.hostnameInfo[addressInfo.internalPort]

  return {
    ...addressInfo,
    hostnames,
    get publicHostnames() {
      return hostnames.filter((h) => h.kind === "onion" || h.public)
    },
    get onionHostnames() {
      return hostnames.filter((h) => h.kind === "onion")
    },
    get localHostnames() {
      return hostnames.filter(
        (h) => h.kind === "ip" && h.hostname.kind === "local",
      )
    },
    get ipHostnames() {
      return hostnames.filter(
        (h) =>
          h.kind === "ip" &&
          (h.hostname.kind === "ipv4" || h.hostname.kind === "ipv6"),
      )
    },
    get ipv4Hostnames() {
      return hostnames.filter(
        (h) => h.kind === "ip" && h.hostname.kind === "ipv4",
      )
    },
    get ipv6Hostnames() {
      return hostnames.filter(
        (h) => h.kind === "ip" && h.hostname.kind === "ipv6",
      )
    },
    get nonIpHostnames() {
      return hostnames.filter(
        (h) =>
          h.kind === "ip" &&
          h.hostname.kind !== "ipv4" &&
          h.hostname.kind !== "ipv6",
      )
    },
    get urls() {
      return this.hostnames.flatMap(toUrl)
    },
    get publicUrls() {
      return this.publicHostnames.flatMap(toUrl)
    },
    get onionUrls() {
      return this.onionHostnames.flatMap(toUrl)
    },
    get localUrls() {
      return this.localHostnames.flatMap(toUrl)
    },
    get ipUrls() {
      return this.ipHostnames.flatMap(toUrl)
    },
    get ipv4Urls() {
      return this.ipv4Hostnames.flatMap(toUrl)
    },
    get ipv6Urls() {
      return this.ipv6Hostnames.flatMap(toUrl)
    },
    get nonIpUrls() {
      return this.nonIpHostnames.flatMap(toUrl)
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
  packageId?: string
  callback?: () => void
}) => {
  const serviceInterfaceValue = await effects.getServiceInterface({
    serviceInterfaceId: id,
    packageId,
    callback,
  })
  if (!serviceInterfaceValue) {
    return null
  }
  const hostId = serviceInterfaceValue.addressInfo.hostId
  const host = await effects.getHostInfo({
    packageId,
    hostId,
    callback,
  })

  const interfaceFilled: ServiceInterfaceFilled = {
    ...serviceInterfaceValue,
    host,
    addressInfo: host
      ? filledAddress(host, serviceInterfaceValue.addressInfo)
      : null,
  }
  return interfaceFilled
}

export class GetServiceInterface {
  constructor(
    readonly effects: Effects,
    readonly opts: { id: string; packageId?: string },
  ) {}

  /**
   * Returns the requested service interface. Reruns the context from which it has been called if the underlying value changes
   */
  async const() {
    const { id, packageId } = this.opts
    const callback =
      this.effects.constRetry &&
      (() => this.effects.constRetry && this.effects.constRetry())
    const interfaceFilled = await makeInterfaceFilled({
      effects: this.effects,
      id,
      packageId,
      callback,
    })

    return interfaceFilled
  }
  /**
   * Returns the requested service interface. Does nothing if the value changes
   */
  async once() {
    const { id, packageId } = this.opts
    const interfaceFilled = await makeInterfaceFilled({
      effects: this.effects,
      id,
      packageId,
    })

    return interfaceFilled
  }

  /**
   * Watches the requested service interface. Returns an async iterator that yields whenever the value changes
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

  /**
   * Watches the requested service interface. Takes a custom callback function to run whenever the value changes
   */
  onChange(
    callback: (
      value: ServiceInterfaceFilled | null,
      error?: Error,
    ) => void | Promise<void>,
  ) {
    ;(async () => {
      for await (const value of this.watch()) {
        try {
          await callback(value)
        } catch (e) {
          console.error(
            "callback function threw an error @ GetServiceInterface.onChange",
            e,
          )
        }
      }
    })()
      .catch((e) => callback(null, e))
      .catch((e) =>
        console.error(
          "callback function threw an error @ GetServiceInterface.onChange",
          e,
        ),
      )
  }
}
export function getServiceInterface(
  effects: Effects,
  opts: { id: string; packageId?: string },
) {
  return new GetServiceInterface(effects, opts)
}
