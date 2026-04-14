import { FullKeyboard, Languages } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'

export type DataModel = {
  ui: UIData
  serverInfo: T.ServerInfo & {
    language: Languages
    keyboard: FullKeyboard | null
  }
  packageData: AllPackageData
}

export type UIData = {
  registries: Record<string, string | null>
  snakeHighScore: number
  startosRegistry: string
  hiddenUpdates: Record<string, string[]>
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
