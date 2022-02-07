import { BackupReport } from './backup-report'

export type NotificationData<T> = T extends 0
  ? null
  : T extends 1
  ? BackupReport
  : any
