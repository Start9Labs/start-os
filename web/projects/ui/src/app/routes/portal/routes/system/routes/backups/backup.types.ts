import { T } from '@start9labs/start-sdk'
import { TuiDialogContext } from '@taiga-ui/core'
import {
  CifsBackupTarget,
  DiskBackupTarget,
} from 'src/app/services/api/api.types'
import { MappedBackupTarget } from './backup.service'

export type BackupContext = TuiDialogContext<
  void,
  MappedBackupTarget<CifsBackupTarget | DiskBackupTarget>
>

export interface RecoverOption extends T.PackageBackupInfo {
  id: string
  checked: boolean
  installed: boolean
  newerOs: boolean
}

export interface RecoverData {
  targetId: string
  serverId: string
  backupInfo: T.BackupInfo
  password: string
}
