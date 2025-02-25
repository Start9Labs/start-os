import { T } from '@start9labs/start-sdk'

export type DataModel = Omit<T.Public, 'serverInfo'> & {
  ui: UIData
  // @TODO 040
  serverInfo: Omit<
    T.Public['serverInfo'],
    'wifi' | 'networkInterfaces' | 'host'
  > & {
    network: NetworkInfo
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
  wifi: T.WifiInfo & { enabled: boolean }
  host: T.Host
  networkInterfaces: {
    [id: string]: {
      inbound: boolean | null
      outbound: boolean | null
      ipInfo:
        | (T.IpInfo & {
            name: string
          })
        | null
    }
  }
  // @TODO 041
  // start9To: {
  //   subdomain: string
  //   networkInterfaceId: string
  // } | null
  // domains: {
  //   [key: string]: Domain
  // }
  // wanConfig: {
  //   upnp: boolean
  //   forwards: PortForward[]
  // }
  // outboundProxy: string | null
}

export type PackageDataEntry<T extends StateInfo = StateInfo> =
  T.PackageDataEntry & {
    stateInfo: T
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

// @TODO 041
// export type ServerStatusInfo = Omit<T.ServerStatus, 'backupProgress'> & {
//   currentBackup: null | {
//     job: BackupJob
//     backupProgress: Record<string, boolean>
//   }
// }
