import { BackupJob, ServerNotifications } from '../api/api.types'
import { T } from '@start9labs/start-sdk'

export type DataModel = {
  ui: UIData
  serverInfo: Omit<
    T.Public['serverInfo'],
    'wifi' | 'unreadNotificationCount'
  > & {
    network: NetworkInfo
    unreadNotifications: {
      count: number
      recent: ServerNotifications
    }
  }
  packageData: Record<string, PackageDataEntry>
}

export type UIData = {
  name: string | null
  marketplace: UIMarketplaceData
  gaming: {
    snake: {
      highScore: number
    }
  }
  ackInstructions: Record<string, boolean>
  theme: string
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
  interface: string | null
  ssids: Array<string>
  selected: string | null
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

export type PackageDataEntry<T extends StateInfo = StateInfo> =
  T.PackageDataEntry & {
    stateInfo: T
    installedAt: string
    outboundProxy: string | null
    nextBackup: string | null
  }

export type AllPackageData = NonNullable<
  T.AllPackageData & Record<string, PackageDataEntry<StateInfo>>
>

export type StateInfo = InstalledState | InstallingState | UpdatingState

export type InstalledState = {
  state: 'installed' | 'removing'
  manifest: T.Manifest
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
  manifest: T.Manifest
}

export type InstallingInfo = {
  progress: T.FullProgress
  newManifest: T.Manifest
}
