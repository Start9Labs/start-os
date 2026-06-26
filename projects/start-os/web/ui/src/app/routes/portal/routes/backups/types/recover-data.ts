import { T } from '@start9labs/start-sdk'

export interface RecoverData {
  targetId: string
  serverId: string
  backupInfo: T.BackupInfo
  password: string
}
