import { BackupJob, ServerNotifications } from '../api/api.types'
import { T } from '@start9labs/start-sdk'
import { config } from '@start9labs/start-sdk'
import { PackageDataEntry as PDE } from '../../../../../../../core/startos/bindings/PackageDataEntry'
import { FullProgress } from '../../../../../../../core/startos/bindings/FullProgress'
import { Manifest } from '../../../../../../../core/startos/bindings/Manifest'

export type DataModel = {
  ui: UIData
  serverInfo: ServerInfo
  packageData: Record<string, PackageDataEntry>
}

export type UIData = {
  name: string | null
  ackWelcome: string // emver
  marketplace: UIMarketplaceData
  gaming: {
    snake: {
      highScore: number
    }
  }
  ackInstructions: Record<string, boolean>
  theme: string
  desktop: readonly string[]
}

export type UIMarketplaceData = {
  selectedUrl: string
  knownHosts: {
    'https://registry.start9.com/': UIStore
    'https://community-registry.start9.com/': UIStore
    [url: string]: UIStore
  }
}

export type UIStore = {
  name?: string
}

export type ServerInfo = {
  id: string
  version: string
  country: string
  ui: T.HostnameInfo[]
  network: NetworkInfo
  lastBackup: string | null
  unreadNotifications: {
    count: number
    recent: ServerNotifications
  }
  statusInfo: ServerStatusInfo
  eosVersionCompat: string
  pubkey: string
  caFingerprint: string
  ntpSynced: boolean
  smtp: typeof config.constants.customSmtp.validator._TYPE
  passwordHash: string
  platform: string
  arch: string
  governor: string | null
}

export type NetworkInfo = {
  wifi: WiFiInfo
  start9ToSubdomain: Omit<Domain, 'provider'> | null
  domains: Domain[]
  wanConfig: {
    upnp: boolean
    forwards: PortForward[]
  }
  proxies: Proxy[]
  outboundProxy: string | null
}

export type DomainInfo = {
  domain: string
  subdomain: string | null
}

export type PortForward = {
  assigned: number
  override: number | null
  target: number
  error: string | null
}

export type WiFiInfo = {
  enabled: boolean
  lastRegion: string | null
}

export type Domain = {
  value: string
  createdAt: string
  provider: string
  networkStrategy: NetworkStrategy
  usedBy: {
    service: { id: string | null; title: string } // null means startos
    interfaces: { id: string | null; title: string }[] // null means startos
  }[]
}

export type NetworkStrategy =
  | { proxy: string }
  | { ipStrategy: 'ipv4' | 'ipv6' | 'dualstack' }

export type Proxy = {
  id: string
  name: string
  createdAt: string
  type: 'outbound' | 'inbound-outbound' | 'vlan' | { error: string }
  endpoint: string
  // below is overlay only
  usedBy: {
    services: { id: string | null; title: string }[] // implies outbound - null means startos
    domains: string[] // implies inbound
  }
}

export interface ServerStatusInfo {
  currentBackup: null | {
    job: BackupJob
    backupProgress: Record<string, boolean>
  }
  updated: boolean
  updateProgress: { size: number | null; downloaded: number } | null
  restarting: boolean
  shuttingDown: boolean
}

export type PackageDataEntry<T extends StateInfo = StateInfo> = PDE & {
  stateInfo: T
  installedAt: string
  outboundProxy: string | null
}

export type StateInfo = InstalledState | InstallingState | UpdatingState

export type InstalledState = {
  state: 'installed' | 'removing'
  manifest: Manifest
  installingInfo?: undefined
}

export type InstallingState = {
  state: 'installing' | 'restoring'
  installingInfo: InstallingInfo
  manifest?: undefined
}

export type UpdatingState = {
  state: 'updating'
  installingInfo: InstallingInfo
  manifest: Manifest
}

export type InstallingInfo = {
  progress: FullProgress
  newManifest: Manifest
}
