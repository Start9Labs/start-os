import { PackageId, ServiceInterfaceId, ServiceInterfaceType } from '../types'
import { knownProtocols } from '../interfaces/Host'
import {
  AddressInfo,
  DerivedAddressInfo,
  Host,
  Hostname,
  HostnameInfo,
} from '../types'
import { Effects } from '../Effects'
import { IpAddress, IPV6_LINK_LOCAL } from './ip'
import { deepEqual } from './deepEqual'
import { once } from './once'
import { Watchable } from './Watchable'

export type UrlString = string
export type HostId = string

const getHostnameRegex = /^(\w+:\/\/)?([^\/\:]+)(:\d{1,3})?(\/)?/
export const getHostname = (url: string): Hostname | null => {
  const founds = url.match(getHostnameRegex)?.[2]
  if (!founds) return null
  const parts = founds.split('@')
  const last = parts[parts.length - 1] as Hostname | null
  return last
}

/**
 * The kinds of hostnames that can be filtered on.
 *
 * - `'mdns'` — mDNS / Bonjour `.local` hostnames
 * - `'domain'` — any os-managed domain name (matches both `'private-domain'` and `'public-domain'` metadata kinds)
 * - `'ip'` — shorthand for both `'ipv4'` and `'ipv6'`
 * - `'ipv4'` — IPv4 addresses only
 * - `'ipv6'` — IPv6 addresses only
 * - `'localhost'` — loopback addresses (`localhost`, `127.0.0.1`, `::1`)
 * - `'link-local'` — IPv6 link-local addresses (fe80::/10)
 * - `'bridge'` — The LXC bridge interface
 * - `'plugin'` — hostnames provided by a plugin package
 */
type FilterKinds =
  | 'mdns'
  | 'domain'
  | 'ip'
  | 'ipv4'
  | 'ipv6'
  | 'localhost'
  | 'link-local'
  | 'bridge'
  | 'plugin'

/**
 * Describes which hostnames to include (or exclude) when filtering a `Filled` address.
 *
 * Every field is optional — omitted fields impose no constraint.
 * Filters are composable: the `.filter()` method intersects successive filters,
 * and the `exclude` field inverts a nested filter.
 */
export type Filter = {
  /** Keep only hostnames with the given visibility. `'public'` = externally reachable, `'private'` = LAN-only. */
  visibility?: 'public' | 'private'
  /** Keep only hostnames whose metadata kind matches. A single kind or array of kinds. `'ip'` expands to `['ipv4','ipv6']`, `'domain'` matches both `'private-domain'` and `'public-domain'`. */
  kind?: FilterKinds | FilterKinds[]
  /** Arbitrary predicate — hostnames for which this returns `false` are excluded. */
  predicate?: (h: HostnameInfo) => boolean
  /** Keep only plugin hostnames provided by this package. Implies `kind: 'plugin'`. */
  pluginId?: PackageId
  /** A nested filter whose matches are *removed* from the result (logical NOT). */
  exclude?: Filter
}

type VisibilityFilter<V extends 'public' | 'private'> = V extends 'public'
  ? (HostnameInfo & { public: true }) | VisibilityFilter<Exclude<V, 'public'>>
  : V extends 'private'
    ?
        | (HostnameInfo & { public: false })
        | VisibilityFilter<Exclude<V, 'private'>>
    : never
type KindFilter<K extends FilterKinds> = K extends 'mdns'
  ?
      | (HostnameInfo & { metadata: { kind: 'mdns' } })
      | KindFilter<Exclude<K, 'mdns'>>
  : K extends 'domain'
    ?
        | (HostnameInfo & { metadata: { kind: 'private-domain' } })
        | (HostnameInfo & { metadata: { kind: 'public-domain' } })
        | KindFilter<Exclude<K, 'domain'>>
    : K extends 'ipv4'
      ?
          | (HostnameInfo & { metadata: { kind: 'ipv4' } })
          | KindFilter<Exclude<K, 'ipv4'>>
      : K extends 'ipv6'
        ?
            | (HostnameInfo & { metadata: { kind: 'ipv6' } })
            | KindFilter<Exclude<K, 'ipv6'>>
        : K extends 'plugin'
          ?
              | (HostnameInfo & { metadata: { kind: 'plugin' } })
              | KindFilter<Exclude<K, 'plugin'>>
          : K extends 'ip'
            ? KindFilter<Exclude<K, 'ip'> | 'ipv4' | 'ipv6'>
            : never

type FilterReturnTy<F extends Filter> = F extends {
  visibility: infer V extends 'public' | 'private'
}
  ? VisibilityFilter<V> & FilterReturnTy<Omit<F, 'visibility'>>
  : F extends {
        kind: (infer K extends FilterKinds) | (infer K extends FilterKinds)[]
      }
    ? KindFilter<K> & FilterReturnTy<Omit<F, 'kind'>>
    : F extends {
          predicate: (h: HostnameInfo) => h is infer H extends HostnameInfo
        }
      ? H & FilterReturnTy<Omit<F, 'predicate'>>
      : F extends { exclude: infer E extends Filter } // MUST BE LAST
        ? HostnameInfo extends FilterReturnTy<E>
          ? HostnameInfo
          : Exclude<HostnameInfo, FilterReturnTy<E>>
        : HostnameInfo

const nonLocalFilter = {
  exclude: {
    kind: ['localhost', 'link-local', 'bridge'] as (
      | 'localhost'
      | 'link-local'
      | 'bridge'
    )[],
  },
} as const
const publicFilter = {
  visibility: 'public',
} as const
type Formats = 'hostname-info' | 'urlstring' | 'url'
type FormatReturnTy<
  F extends Filter,
  Format extends Formats,
> = Format extends 'hostname-info'
  ? FilterReturnTy<F> | FormatReturnTy<F, Exclude<Format, 'hostname-info'>>
  : Format extends 'url'
    ? URL | FormatReturnTy<F, Exclude<Format, 'url'>>
    : Format extends 'urlstring'
      ? UrlString | FormatReturnTy<F, Exclude<Format, 'urlstring'>>
      : never

/**
 * A resolved address with its hostnames already populated, plus helpers
 * for filtering, formatting, and converting hostnames to URLs.
 *
 * Filters are chainable and each call returns a new `Filled` narrowed to the
 * matching subset of hostnames:
 *
 * ```ts
 * addresses.nonLocal                         // exclude localhost & link-local
 * addresses.public                           // only publicly-reachable hostnames
 * addresses.filter({ kind: 'domain' })       // only domain-name hostnames
 * addresses.filter({ visibility: 'private' }) // only LAN-reachable hostnames
 * addresses.nonLocal.filter({ kind: 'ip' })  // chainable — non-local IPs only
 * ```
 */
export type Filled<F extends Filter = {}> = {
  /** The hostnames that survived all applied filters. */
  hostnames: HostnameInfo[]

  /** Convert a single hostname into a fully-formed URL string, applying the address's scheme, username, and suffix. */
  toUrl: (h: HostnameInfo) => UrlString

  /**
   * Return every hostname in the requested format.
   *
   * - `'urlstring'` (default) — formatted URL strings
   * - `'url'`                 — `URL` objects
   * - `'hostname-info'`       — raw `HostnameInfo` objects
   */
  format: <Format extends Formats = 'urlstring'>(
    format?: Format,
  ) => FormatReturnTy<{}, Format>[]

  /**
   * Apply an arbitrary {@link Filter} and return a new `Filled` containing only
   * the hostnames that match. Filters compose: calling `.filter()` on an
   * already-filtered `Filled` intersects the constraints.
   */
  filter: <NewFilter extends Filter>(
    filter: NewFilter,
  ) => Filled<NewFilter & Filter>

  /**
   * Apply multiple filters and return hostnames that match **any** of them (union / OR).
   *
   * ```ts
   * addresses.matchesAny([{ kind: 'domain' }, { kind: 'mdns' }])
   * ```
   */
  matchesAny: <NewFilters extends Filter[]>(
    filters: [...NewFilters],
  ) => Filled<NewFilters[number] & F>

  /** Shorthand filter that excludes `localhost` and IPv6 link-local addresses — keeps only network-reachable hostnames. */
  nonLocal: Filled<typeof nonLocalFilter & Filter>
  /** Shorthand filter that keeps only publicly-reachable hostnames (those with `public: true`). */
  public: Filled<typeof publicFilter & Filter>
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
  hostname: HostnameInfo,
): UrlString => {
  const effectiveScheme = hostname.ssl ? sslScheme : scheme
  let host: string
  if (hostname.metadata.kind === 'ipv6') {
    host = IPV6_LINK_LOCAL.contains(hostname.hostname)
      ? `[${hostname.hostname}%${hostname.metadata.scopeId}]`
      : `[${hostname.hostname}]`
  } else {
    host = hostname.hostname
  }
  let portStr = ''
  if (hostname.port !== null) {
    const excludePort =
      effectiveScheme &&
      effectiveScheme in knownProtocols &&
      hostname.port ===
        knownProtocols[effectiveScheme as keyof typeof knownProtocols]
          .defaultPort
    if (!excludePort) portStr = `:${hostname.port}`
  }
  return `${effectiveScheme ? `${effectiveScheme}://` : ''}${
    username ? `${username}@` : ''
  }${host}${portStr}${suffix}`
}

function filterRec(
  hostnames: HostnameInfo[],
  filter: Filter,
  invert: boolean,
): HostnameInfo[] {
  if (filter.predicate) {
    const pred = filter.predicate
    hostnames = hostnames.filter((h) => invert !== pred(h))
  }
  if (filter.visibility === 'public')
    hostnames = hostnames.filter((h) => invert !== h.public)
  if (filter.visibility === 'private')
    hostnames = hostnames.filter((h) => invert !== !h.public)
  if (filter.kind) {
    const kind = new Set(
      Array.isArray(filter.kind) ? filter.kind : [filter.kind],
    )
    if (kind.has('ip')) {
      kind.add('ipv4')
      kind.add('ipv6')
    }
    hostnames = hostnames.filter(
      (h) =>
        invert !==
        ((kind.has('mdns') && h.metadata.kind === 'mdns') ||
          (kind.has('domain') &&
            (h.metadata.kind === 'private-domain' ||
              h.metadata.kind === 'public-domain')) ||
          (kind.has('ipv4') && h.metadata.kind === 'ipv4') ||
          (kind.has('ipv6') && h.metadata.kind === 'ipv6') ||
          (kind.has('localhost') &&
            ['localhost', '127.0.0.1', '::1'].includes(h.hostname)) ||
          (kind.has('link-local') &&
            h.metadata.kind === 'ipv6' &&
            IPV6_LINK_LOCAL.contains(IpAddress.parse(h.hostname))) ||
          (kind.has('bridge') &&
            h.metadata.kind === 'ipv4' &&
            h.metadata.gateway === 'lxcbr0') ||
          (kind.has('plugin') && h.metadata.kind === 'plugin')),
    )
  }
  if (filter.pluginId) {
    const id = filter.pluginId
    hostnames = hostnames.filter(
      (h) =>
        invert !==
        (h.metadata.kind === 'plugin' && h.metadata.packageId === id),
    )
  }

  if (filter.exclude) return filterRec(hostnames, filter.exclude, !invert)

  return hostnames
}

function isPublicIp(h: HostnameInfo): boolean {
  return h.public && (h.metadata.kind === 'ipv4' || h.metadata.kind === 'ipv6')
}

function enabledAddresses(addr: DerivedAddressInfo): HostnameInfo[] {
  return addr.available.filter((h) => {
    if (isPublicIp(h)) {
      // Public IPs: disabled by default, explicitly enabled via SocketAddr string
      if (h.port === null) return true
      const sa =
        h.metadata.kind === 'ipv6'
          ? `[${h.hostname}]:${h.port}`
          : `${h.hostname}:${h.port}`
      return addr.enabled.includes(sa)
    } else {
      // Everything else: enabled by default, explicitly disabled via [hostname, port] tuple
      return !addr.disabled.some(
        ([hostname, port]) => hostname === h.hostname && port === (h.port ?? 0),
      )
    }
  })
}

/**
 * Filters out localhost and IPv6 link-local hostnames from a list.
 * Equivalent to the `nonLocal` filter on `Filled` addresses.
 */
export function filterNonLocal(hostnames: HostnameInfo[]): HostnameInfo[] {
  return filterRec(hostnames, nonLocalFilter, false)
}

export const filledAddress = (
  host: Host,
  addressInfo: AddressInfo,
): FilledAddressInfo => {
  const toUrl = addressHostToUrl.bind(null, addressInfo)
  const binding = host.bindings[addressInfo.internalPort]
  const hostnames = binding ? enabledAddresses(binding.addresses) : []

  function filledAddressFromHostnames<F extends Filter>(
    hostnames: HostnameInfo[],
  ): Filled<F> & AddressInfo {
    const getNonLocal = once(() =>
      filledAddressFromHostnames<typeof nonLocalFilter & F>(
        filterRec(hostnames, nonLocalFilter, false),
      ),
    )
    const getPublic = once(() =>
      filledAddressFromHostnames<typeof publicFilter & F>(
        filterRec(hostnames, publicFilter, false),
      ),
    )
    return {
      ...addressInfo,
      hostnames,
      toUrl,
      format: <Format extends Formats = 'urlstring'>(format?: Format) => {
        let res: FormatReturnTy<{}, Format>[] = hostnames as any
        if (format === 'hostname-info') return res
        const urls = hostnames.map(toUrl)
        if (format === 'url') res = urls.map((u) => new URL(u)) as any
        else res = urls as any
        return res
      },
      filter: <NewFilter extends Filter>(filter: NewFilter) => {
        return filledAddressFromHostnames<NewFilter & F>(
          filterRec(hostnames, filter, false),
        )
      },
      matchesAny: <NewFilters extends Filter[]>(filters: [...NewFilters]) => {
        const seen = new Set<HostnameInfo>()
        const union: HostnameInfo[] = []
        for (const f of filters) {
          for (const h of filterRec(hostnames, f, false)) {
            if (!seen.has(h)) {
              seen.add(h)
              union.push(h)
            }
          }
        }
        return filledAddressFromHostnames<NewFilters[number] & F>(union)
      },
      get nonLocal(): Filled<typeof nonLocalFilter & F> {
        return getNonLocal()
      },
      get public(): Filled<typeof publicFilter & F> {
        return getPublic()
      },
    }
  }

  return filledAddressFromHostnames<{}>(hostnames)
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

export class GetServiceInterface<
  Mapped = ServiceInterfaceFilled | null,
> extends Watchable<ServiceInterfaceFilled | null, Mapped> {
  protected readonly label = 'GetServiceInterface'

  constructor(
    effects: Effects,
    readonly opts: { id: string; packageId?: string },
    options?: {
      map?: (value: ServiceInterfaceFilled | null) => Mapped
      eq?: (a: Mapped, b: Mapped) => boolean
    },
  ) {
    super(effects, options)
  }

  protected fetch(callback?: () => void) {
    return makeInterfaceFilled({
      effects: this.effects,
      id: this.opts.id,
      packageId: this.opts.packageId,
      callback,
    })
  }
}

export function getOwnServiceInterface(
  effects: Effects,
  id: ServiceInterfaceId,
): GetServiceInterface
export function getOwnServiceInterface<Mapped>(
  effects: Effects,
  id: ServiceInterfaceId,
  map: (interfaces: ServiceInterfaceFilled | null) => Mapped,
  eq?: (a: Mapped, b: Mapped) => boolean,
): GetServiceInterface<Mapped>
export function getOwnServiceInterface<Mapped>(
  effects: Effects,
  id: ServiceInterfaceId,
  map?: (interfaces: ServiceInterfaceFilled | null) => Mapped,
  eq?: (a: Mapped, b: Mapped) => boolean,
): GetServiceInterface<Mapped> {
  return new GetServiceInterface<Mapped>(
    effects,
    { id },
    {
      map: map ?? ((a) => a as Mapped),
      eq: eq ?? ((a, b) => deepEqual(a, b)),
    },
  )
}

export function getServiceInterface(
  effects: Effects,
  opts: { id: ServiceInterfaceId; packageId: PackageId },
): GetServiceInterface
export function getServiceInterface<Mapped>(
  effects: Effects,
  opts: { id: ServiceInterfaceId; packageId: PackageId },
  map: (interfaces: ServiceInterfaceFilled | null) => Mapped,
  eq?: (a: Mapped, b: Mapped) => boolean,
): GetServiceInterface<Mapped>
export function getServiceInterface<Mapped>(
  effects: Effects,
  opts: { id: ServiceInterfaceId; packageId: PackageId },
  map?: (interfaces: ServiceInterfaceFilled | null) => Mapped,
  eq?: (a: Mapped, b: Mapped) => boolean,
): GetServiceInterface<Mapped> {
  return new GetServiceInterface<Mapped>(effects, opts, {
    map: map ?? ((a) => a as Mapped),
    eq: eq ?? ((a, b) => deepEqual(a, b)),
  })
}
