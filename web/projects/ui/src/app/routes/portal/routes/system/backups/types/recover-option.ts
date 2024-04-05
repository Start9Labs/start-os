import { PackageBackupInfo } from 'src/app/services/api/api.types'

export interface RecoverOption extends PackageBackupInfo {
  id: string
  checked: boolean
  installed: boolean
  newerStartOs: boolean
}
