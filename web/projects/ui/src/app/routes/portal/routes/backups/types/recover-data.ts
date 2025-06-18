import { BackupInfo } from 'src/app/services/api/api.types'

export interface RecoverData {
  targetId: string
  serverId: string
  backupInfo: BackupInfo
  password: string
}
