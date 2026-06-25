import { RR } from 'src/app/services/api/api.types'

export type BackupConfig =
  | {
      type: {
        selection: 'dropbox' | 'google-drive'
        value: RR.AddCloudBackupTargetReq
      }
    }
  | {
      type: {
        selection: 'cifs'
        value: RR.AddCifsBackupTargetReq
      }
    }
