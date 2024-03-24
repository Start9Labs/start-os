import { CT } from '@start9labs/start-sdk'
import { RR } from 'src/app/services/api/api.types'

export type BackupConfig =
  | {
      type: {
        [CT.unionSelectKey]: 'dropbox' | 'google-drive'
        [CT.unionValueKey]: RR.AddCloudBackupTargetReq
      }
    }
  | {
      type: {
        [CT.unionSelectKey]: 'cifs'
        [CT.unionValueKey]: RR.AddCifsBackupTargetReq
      }
    }
