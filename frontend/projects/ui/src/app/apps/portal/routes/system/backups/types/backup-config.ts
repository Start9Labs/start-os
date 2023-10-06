import {
  unionSelectKey,
  unionValueKey,
} from '@start9labs/start-sdk/lib/config/configTypes'
import { RR } from 'src/app/services/api/api.types'

export type BackupConfig =
  | {
      type: {
        [unionSelectKey]: 'dropbox' | 'google-drive'
        [unionValueKey]: RR.AddCloudBackupTargetReq
      }
    }
  | {
      type: {
        [unionSelectKey]: 'cifs'
        [unionValueKey]: RR.AddCifsBackupTargetReq
      }
    }
