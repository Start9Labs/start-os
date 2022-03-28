import { Injectable } from '@angular/core'
import { IonicSafeString } from '@ionic/core'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import {
  BackupTarget,
  CifsBackupTarget,
  DiskBackupTarget,
} from 'src/app/services/api/api.types'
import { MappedBackupTarget } from 'src/app/types/mapped-backup-target'
import { getErrorMessage, Emver } from '@start9labs/shared'

@Injectable({
  providedIn: 'root',
})
export class BackupService {
  cifs: MappedBackupTarget<CifsBackupTarget>[]
  drives: MappedBackupTarget<DiskBackupTarget>[]
  loading = true
  loadingError: string | IonicSafeString

  constructor(
    private readonly embassyApi: ApiService,
    private readonly emver: Emver,
  ) {}

  async getBackupTargets(): Promise<void> {
    this.loading = true

    try {
      const targets = await this.embassyApi.getBackupTargets({})
      // cifs
      this.cifs = Object.entries(targets)
        .filter(([_, target]) => target.type === 'cifs')
        .map(([id, cifs]) => {
          return {
            id,
            hasValidBackup: this.hasValidBackup(cifs),
            entry: cifs as CifsBackupTarget,
          }
        })
      // drives
      this.drives = Object.entries(targets)
        .filter(([_, target]) => target.type === 'disk')
        .map(([id, drive]) => {
          return {
            id,
            hasValidBackup: this.hasValidBackup(drive),
            entry: drive as DiskBackupTarget,
          }
        })
    } catch (e) {
      this.loadingError = getErrorMessage(e)
    } finally {
      this.loading = false
    }
  }

  hasValidBackup(target: BackupTarget): boolean {
    return this.emver.compare(target['embassy-os']?.version, '0.3.0') !== -1
  }
}
