import { Url } from '@start9labs/shared'
import { types } from '@start9labs/start-sdk'
import { ActionMetadata } from '@start9labs/start-sdk/cjs/sdk/lib/types'
import { Public } from '../../../../../../../core/startos/bindings/Public'
import { PackageDataEntry as PDE } from '../../../../../../../core/startos/bindings/PackageDataEntry'
import { FullProgress } from '../../../../../../../core/startos/bindings/FullProgress'
import { Manifest } from '../../../../../../../core/startos/bindings/Manifest'
type ServiceInterfaceWithHostInfo = types.ServiceInterfaceWithHostInfo

export type DataModel = Public & {
  ui: UIData
  packageData: Record<string, PackageDataEntry>
}

export interface UIData {
  name: string | null
  ackWelcome: string // eOS emver
  marketplace: UIMarketplaceData
  gaming: {
    snake: {
      highScore: number
    }
  }
  ackInstructions: Record<string, boolean>
  theme: string
  widgets: readonly Widget[]
}

export interface Widget {
  id: string
  meta: {
    name: string
    width: number
    height: number
    mobileWidth: number
    mobileHeight: number
  }
  url?: string
  settings?: string
}

export interface UIMarketplaceData {
  selectedUrl: string
  knownHosts: {
    'https://registry.start9.com/': UIStore
    'https://community-registry.start9.com/': UIStore
    [url: string]: UIStore
  }
}

export interface UIStore {
  name?: string
}

export type PackageDataEntry<T extends StateInfo = StateInfo> = PDE & {
  stateInfo: T
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
