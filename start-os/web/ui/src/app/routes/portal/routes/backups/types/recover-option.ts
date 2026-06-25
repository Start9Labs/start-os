import { T } from '@start9labs/start-sdk'

export interface RecoverOption extends T.PackageBackupInfo {
  id: string
  checked: boolean
  installed: boolean
  newerOs: boolean
}
