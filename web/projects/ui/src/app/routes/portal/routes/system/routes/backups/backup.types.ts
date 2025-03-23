import { TuiDialogContext } from '@taiga-ui/core'
import {
  BackupInfo,
  CifsBackupTarget,
  DiskBackupTarget,
  PackageBackupInfo,
} from 'src/app/services/api/api.types'
import { MappedBackupTarget } from './backup.service'

export type BackupContext = TuiDialogContext<
  void,
  MappedBackupTarget<CifsBackupTarget | DiskBackupTarget>
>

export interface RecoverOption extends PackageBackupInfo {
  id: string
  checked: boolean
  installed: boolean
  newerOs: boolean
}

export interface RecoverData {
  targetId: string
  serverId: string
  backupInfo: BackupInfo
  password: string
}
