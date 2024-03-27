import { BackupInfo } from 'src/app/services/api/api.types'

export interface RecoverData {
  targetId: string
  backupInfo: BackupInfo
  password: string
}
