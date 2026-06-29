import { T } from '@start9labs/start-core'

export interface RecoverData {
  targetId: string
  serverId: string
  backupInfo: T.BackupInfo
  password: string
}
