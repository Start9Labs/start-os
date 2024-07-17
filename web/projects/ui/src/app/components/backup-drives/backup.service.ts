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
  cifs: MappedBackupTarget<CifsBackupTarget>[] = []
  drives: MappedBackupTarget<DiskBackupTarget>[] = []
  loading = true
  loadingError: string | IonicSafeString = ''

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
            hasAnyBackup: this.hasAnyBackup(cifs),
            entry: cifs as CifsBackupTarget,
          }
        })
      // drives
      this.drives = Object.entries(targets)
        .filter(([_, target]) => target.type === 'disk')
        .map(([id, drive]) => {
          return {
            id,
            hasAnyBackup: this.hasAnyBackup(drive),
            entry: drive as DiskBackupTarget,
          }
        })
    } catch (e: any) {
      this.loadingError = getErrorMessage(e)
    } finally {
      this.loading = false
    }
  }

  hasAnyBackup(target: BackupTarget): boolean {
    return Object.values(target.startOs).some(
      s => this.emver.compare(s.version, '0.3.6') !== -1,
    )
  }

  hasThisBackup(target: BackupTarget, id: string): boolean {
    return (
      target.startOs[id] &&
      this.emver.compare(target.startOs[id].version, '0.3.6') !== -1
    )
  }
}
